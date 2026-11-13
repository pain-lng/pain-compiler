// Query module - structural questions about a program, answered for tools and AI agents:
// who calls a function, what it calls, and the minimal code needed to change it.
//
// The call graph comes from the AST. Pain has no function values, reflection or
// dynamic dispatch, so a resolved edge is a fact, not a guess. Whatever cannot be
// resolved is reported in `notes` and turns `exact` off instead of being dropped.

use crate::ast::*;
use crate::lexer::{tokenize_with_indentation, Token};
use serde_json::{json, Value};
use std::collections::{BTreeMap, BTreeSet, HashMap};

/// Kind of a call-like expression, in the order it appears in the source
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CallKind {
    Direct,
    Method,
    New,
}

/// A function or method definition
#[derive(Debug, Clone)]
pub struct Symbol {
    /// `name` for functions, `Class.name` for methods
    pub name: String,
    pub class: Option<String>,
    pub signature: String,
    pub first_line: usize,
    pub last_line: usize,
}

/// One call from `caller` to `callee`
#[derive(Debug, Clone)]
pub struct CallSite {
    pub caller: String,
    pub callee: String,
    /// 0 when the position could not be recovered
    pub line: usize,
    pub column: usize,
    /// false when a method call was matched by name only (receiver type unknown)
    pub certain: bool,
}

#[derive(Debug, Clone)]
struct ClassInfo {
    first_line: usize,
    last_line: usize,
    methods: Vec<String>,
}

/// Call graph and definition ranges of one source file
pub struct ProgramIndex<'a> {
    lines: Vec<&'a str>,
    symbols: BTreeMap<String, Symbol>,
    classes: BTreeMap<String, ClassInfo>,
    calls: Vec<CallSite>,
    /// Classes each symbol mentions (constructs, takes or returns)
    class_uses: BTreeMap<String, BTreeSet<String>>,
    /// Reasons the graph may be incomplete
    notes: Vec<String>,
}

/// Raw call found in a body, before positions are attached
struct RawCall {
    kind: CallKind,
    name: String,
    callee: Option<String>,
    certain: bool,
}

struct BodyWalker<'p> {
    functions: &'p BTreeSet<String>,
    methods: &'p BTreeMap<String, Vec<String>>,
    return_classes: &'p HashMap<String, String>,
    locals: HashMap<String, Option<String>>,
    owner: String,
    calls: Vec<RawCall>,
    class_uses: BTreeSet<String>,
    notes: Vec<String>,
}

fn named_class(ty: &Type) -> Option<String> {
    match ty {
        Type::Named(name) => Some(name.clone()),
        _ => None,
    }
}

impl BodyWalker<'_> {
    fn class_of(&self, expr: &Expr) -> Option<String> {
        match expr {
            Expr::Ident(name) => self.locals.get(name).cloned().flatten(),
            Expr::New { class_name, .. } => Some(class_name.clone()),
            Expr::Call { callee, .. } => match callee.as_ref() {
                Expr::Ident(name) if !self.locals.contains_key(name) => {
                    self.return_classes.get(name).cloned()
                }
                _ => None,
            },
            _ => None,
        }
    }

    fn block(&mut self, stmts: &[Statement]) {
        for stmt in stmts {
            self.statement(stmt);
        }
    }

    fn statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Expr(expr) => self.expr(expr),
            Statement::Let { name, ty, init, .. } => {
                self.expr(init);
                let class = ty
                    .as_ref()
                    .and_then(named_class)
                    .or_else(|| self.class_of(init));
                if let Some(class) = &class {
                    self.class_uses.insert(class.clone());
                }
                self.locals.insert(name.clone(), class);
            }
            Statement::Return(Some(expr)) => self.expr(expr),
            Statement::Return(None) | Statement::Break | Statement::Continue => {}
            Statement::If { cond, then, else_ } => {
                self.expr(cond);
                self.block(then);
                if let Some(else_) = else_ {
                    self.block(else_);
                }
            }
            Statement::For { var, iter, body } => {
                self.expr(iter);
                self.locals.insert(var.clone(), None);
                self.block(body);
            }
            Statement::While { cond, body } => {
                self.expr(cond);
                self.block(body);
            }
        }
    }

    fn expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Integer(_) | Expr::Float(_) | Expr::String(_) | Expr::Bool(_) | Expr::None => {}
            Expr::FString(template) => {
                // Calls inside an f-string are not parsed into the AST
                if template.contains('(') {
                    self.notes.push(format!(
                        "{}: f-string may contain a call the graph cannot see",
                        self.owner
                    ));
                }
            }
            Expr::Ident(name) => {
                if !self.locals.contains_key(name) && self.functions.contains(name) {
                    self.notes.push(format!(
                        "{}: function `{}` is used as a value, calls through it are not tracked",
                        self.owner, name
                    ));
                }
            }
            Expr::List(items) => items.iter().for_each(|e| self.expr(e)),
            Expr::Add(a, b)
            | Expr::Sub(a, b)
            | Expr::Mul(a, b)
            | Expr::Div(a, b)
            | Expr::Mod(a, b)
            | Expr::Eq(a, b)
            | Expr::Ne(a, b)
            | Expr::Lt(a, b)
            | Expr::Gt(a, b)
            | Expr::Le(a, b)
            | Expr::Ge(a, b)
            | Expr::And(a, b)
            | Expr::Or(a, b)
            | Expr::Assign(a, b)
            | Expr::AddAssign(a, b)
            | Expr::SubAssign(a, b)
            | Expr::MulAssign(a, b)
            | Expr::DivAssign(a, b)
            | Expr::Index(a, b) => {
                self.expr(a);
                self.expr(b);
            }
            Expr::Not(a) | Expr::Neg(a) | Expr::IsInstance(a, _) => self.expr(a),
            Expr::Member(obj, _) => self.expr(obj),
            Expr::New { class_name, args } => {
                self.class_uses.insert(class_name.clone());
                self.calls.push(RawCall {
                    kind: CallKind::New,
                    name: class_name.clone(),
                    callee: None,
                    certain: true,
                });
                args.iter().for_each(|e| self.expr(e));
            }
            Expr::Call { callee, args } => {
                match callee.as_ref() {
                    Expr::Ident(name) => self.direct_call(name),
                    Expr::Member(obj, method) => {
                        // Receiver first: `f().m()` reads `f` before `m`
                        self.expr(obj);
                        self.method_call(obj, method);
                    }
                    other => {
                        self.notes.push(format!(
                            "{}: call through an expression, target unknown",
                            self.owner
                        ));
                        self.expr(other);
                    }
                }
                args.iter().for_each(|e| self.expr(e));
            }
        }
    }

    fn direct_call(&mut self, name: &str) {
        let callee = if self.locals.contains_key(name) {
            self.notes.push(format!(
                "{}: call through local variable `{}`, target unknown",
                self.owner, name
            ));
            None
        } else if self.functions.contains(name) {
            Some(name.to_string())
        } else {
            None // builtin / stdlib
        };
        self.calls.push(RawCall {
            kind: CallKind::Direct,
            name: name.to_string(),
            callee,
            certain: true,
        });
    }

    fn method_call(&mut self, receiver: &Expr, method: &str) {
        let owners = self.methods.get(method).cloned().unwrap_or_default();
        let (callee, certain) = match self.class_of(receiver) {
            Some(class) if owners.contains(&class) => (Some(format!("{}.{}", class, method)), true),
            Some(_) => (None, true),
            None if owners.is_empty() => (None, true), // builtin method
            None => {
                self.notes.push(format!(
                    "{}: receiver type of `.{}()` is unknown, matched by method name",
                    self.owner, method
                ));
                for class in &owners[1..] {
                    // Every candidate is a possible target; positions go to the first one
                    self.calls.push(RawCall {
                        kind: CallKind::Method,
                        name: String::new(),
                        callee: Some(format!("{}.{}", class, method)),
                        certain: false,
                    });
                }
                (Some(format!("{}.{}", owners[0], method)), false)
            }
        };
        self.calls.push(RawCall {
            kind: CallKind::Method,
            name: method.to_string(),
            callee,
            certain,
        });
    }
}

/// A definition header found in the token stream
struct TokenDef {
    is_class: bool,
    name: String,
    line: usize,
    column: usize,
    /// Index of the header token in the token list
    index: usize,
}

struct TokenCall {
    kind: CallKind,
    name: String,
    line: usize,
    column: usize,
    index: usize,
}

struct TokenScan {
    defs: Vec<TokenDef>,
    calls: Vec<TokenCall>,
    /// (line, column) of the first token of every non-empty line
    line_starts: Vec<(usize, usize)>,
}

fn scan_tokens(source: &str) -> TokenScan {
    let tokens: Vec<_> = tokenize_with_indentation(source)
        .into_iter()
        .flatten()
        .filter(|t| !matches!(t.token, Token::Indent | Token::Dedent | Token::Newline))
        .collect();
    let mut scan = TokenScan {
        defs: Vec::new(),
        calls: Vec::new(),
        line_starts: Vec::new(),
    };
    for (i, tok) in tokens.iter().enumerate() {
        let (line, column) = (tok.span.start.line, tok.span.start.column);
        if scan.line_starts.last().map(|l| l.0) != Some(line) {
            scan.line_starts.push((line, column));
        }
        let Token::Ident(name) = &tok.token else {
            continue;
        };
        let prev = i.checked_sub(1).map(|p| &tokens[p].token);
        let next_is_paren = matches!(tokens.get(i + 1).map(|t| &t.token), Some(Token::LParen));
        match prev {
            Some(Token::Fn) | Some(Token::Class) => {
                let header = &tokens[i - 1];
                scan.defs.push(TokenDef {
                    is_class: matches!(header.token, Token::Class),
                    name: name.clone(),
                    line: header.span.start.line,
                    column: header.span.start.column,
                    index: i,
                });
            }
            Some(Token::New) if next_is_paren => scan.calls.push(TokenCall {
                kind: CallKind::New,
                name: name.clone(),
                line,
                column,
                index: i,
            }),
            Some(Token::Dot) if next_is_paren => scan.calls.push(TokenCall {
                kind: CallKind::Method,
                name: name.clone(),
                line,
                column,
                index: i,
            }),
            _ if next_is_paren => scan.calls.push(TokenCall {
                kind: CallKind::Direct,
                name: name.clone(),
                line,
                column,
                index: i,
            }),
            _ => {}
        }
    }
    scan
}

impl TokenScan {
    /// Last line of the definition whose header starts at (line, column):
    /// the line before the next one that is indented no deeper than the header.
    fn last_line(&self, line: usize, column: usize) -> usize {
        let mut last = line;
        for &(l, c) in self.line_starts.iter().filter(|(l, _)| *l > line) {
            if c <= column {
                break;
            }
            last = l;
        }
        last
    }
}

impl<'a> ProgramIndex<'a> {
    pub fn build(program: &Program, source: &'a str) -> Self {
        let scan = scan_tokens(source);
        let lines: Vec<&str> = source.lines().collect();
        let mut notes = Vec::new();

        // Definitions in source order: the token stream and the AST must agree
        let mut ast_defs: Vec<(bool, String, Option<&str>, Option<&Function>)> = Vec::new();
        for item in &program.items {
            match item {
                Item::Function(func) => ast_defs.push((false, func.name.clone(), None, Some(func))),
                Item::Class(class) => {
                    ast_defs.push((true, class.name.clone(), None, None));
                    for method in &class.methods {
                        ast_defs.push((
                            false,
                            method.name.clone(),
                            Some(&class.name),
                            Some(method),
                        ));
                    }
                }
            }
        }
        let positions_ok = ast_defs.len() == scan.defs.len()
            && ast_defs
                .iter()
                .zip(&scan.defs)
                .all(|(a, t)| a.0 == t.is_class && a.1 == t.name);
        if !positions_ok {
            notes.push("definitions could not be matched to source lines".to_string());
        }

        let functions: BTreeSet<String> = program
            .items
            .iter()
            .filter_map(|item| match item {
                Item::Function(f) => Some(f.name.clone()),
                Item::Class(_) => None,
            })
            .collect();
        let mut methods: BTreeMap<String, Vec<String>> = BTreeMap::new();
        let mut class_fields: HashMap<String, Vec<(String, Option<String>)>> = HashMap::new();
        let mut return_classes = HashMap::new();
        for item in &program.items {
            match item {
                Item::Function(f) => {
                    if let Some(class) = f.return_type.as_ref().and_then(named_class) {
                        return_classes.insert(f.name.clone(), class);
                    }
                }
                Item::Class(class) => {
                    for method in &class.methods {
                        methods
                            .entry(method.name.clone())
                            .or_default()
                            .push(class.name.clone());
                    }
                    class_fields.insert(
                        class.name.clone(),
                        class
                            .fields
                            .iter()
                            .map(|f| (f.name.clone(), named_class(&f.ty)))
                            .collect(),
                    );
                }
            }
        }

        let mut symbols = BTreeMap::new();
        let mut classes = BTreeMap::new();
        let mut calls = Vec::new();
        let mut class_uses = BTreeMap::new();

        for (i, (is_class, name, class, func)) in ast_defs.iter().enumerate() {
            let (first_line, last_line, tok_range) = if positions_ok {
                let def = &scan.defs[i];
                let last = scan.last_line(def.line, def.column);
                // Tokens of this body: up to the next definition header
                let end = scan.defs.get(i + 1).map(|d| d.index).unwrap_or(usize::MAX);
                (def.line, last, Some((def.index, end)))
            } else {
                (0, 0, None)
            };
            if *is_class {
                classes.insert(
                    name.clone(),
                    ClassInfo {
                        first_line,
                        last_line,
                        methods: Vec::new(),
                    },
                );
                continue;
            }
            let func = func.expect("function definition");
            let full_name = match class {
                Some(class) => format!("{}.{}", class, name),
                None => name.clone(),
            };
            if let Some(class) = class {
                if let Some(info) = classes.get_mut(*class) {
                    info.methods.push(full_name.clone());
                }
            }

            let mut walker = BodyWalker {
                functions: &functions,
                methods: &methods,
                return_classes: &return_classes,
                locals: HashMap::new(),
                owner: full_name.clone(),
                calls: Vec::new(),
                class_uses: BTreeSet::new(),
                notes: Vec::new(),
            };
            if let Some(class) = class {
                // Fields are visible by bare name inside methods
                for (field, field_class) in class_fields.get(*class).into_iter().flatten() {
                    walker.locals.insert(field.clone(), field_class.clone());
                }
                walker
                    .locals
                    .insert("self".to_string(), Some(class.to_string()));
            }
            for param in &func.params {
                let param_class = named_class(&param.ty);
                if let Some(c) = &param_class {
                    walker.class_uses.insert(c.clone());
                }
                walker.locals.insert(param.name.clone(), param_class);
            }
            if let Some(c) = func.return_type.as_ref().and_then(named_class) {
                walker.class_uses.insert(c);
            }
            walker.block(&func.body);

            // Attach positions: calls in the AST and in the tokens come in the same order
            let located: Vec<&TokenCall> = match tok_range {
                Some((start, end)) => scan
                    .calls
                    .iter()
                    .filter(|c| c.index > start && c.index < end)
                    .collect(),
                None => Vec::new(),
            };
            let visible: Vec<&RawCall> =
                walker.calls.iter().filter(|c| !c.name.is_empty()).collect();
            let aligned = visible.len() == located.len()
                && visible
                    .iter()
                    .zip(&located)
                    .all(|(a, t)| a.kind == t.kind && a.name == t.name);
            if !aligned && positions_ok && !visible.is_empty() {
                walker
                    .notes
                    .push(format!("{}: call positions unavailable", full_name));
            }
            let mut next_located = 0;
            let mut last_pos = (0, 0);
            for raw in &walker.calls {
                if !raw.name.is_empty() {
                    last_pos = if aligned {
                        let t = located[next_located];
                        (t.line, t.column)
                    } else {
                        (0, 0)
                    };
                    next_located += 1;
                }
                if let Some(callee) = &raw.callee {
                    let pos = if raw.name.is_empty() {
                        // Extra candidate of an ambiguous method call: emitted just
                        // before the call it belongs to, so its position comes next
                        if aligned {
                            located
                                .get(next_located)
                                .map(|t| (t.line, t.column))
                                .unwrap_or((0, 0))
                        } else {
                            (0, 0)
                        }
                    } else {
                        last_pos
                    };
                    calls.push(CallSite {
                        caller: full_name.clone(),
                        callee: callee.clone(),
                        line: pos.0,
                        column: pos.1,
                        certain: raw.certain,
                    });
                }
            }

            let signature = lines
                .get(first_line.wrapping_sub(1))
                .map(|l| l.trim().trim_end_matches(':').to_string())
                .unwrap_or_else(|| format!("fn {}", name));
            class_uses.insert(full_name.clone(), walker.class_uses);
            notes.extend(walker.notes);
            symbols.insert(
                full_name.clone(),
                Symbol {
                    name: full_name,
                    class: class.map(|c| c.to_string()),
                    signature,
                    first_line,
                    last_line,
                },
            );
        }

        notes.sort();
        notes.dedup();
        Self {
            lines,
            symbols,
            classes,
            calls,
            class_uses,
            notes,
        }
    }

    pub fn symbols(&self) -> impl Iterator<Item = &Symbol> {
        self.symbols.values()
    }

    pub fn calls(&self) -> &[CallSite] {
        &self.calls
    }

    pub fn notes(&self) -> &[String] {
        &self.notes
    }

    /// Resolve a user-supplied name: exact, or a method given without its class
    fn resolve(&self, name: &str) -> Result<&Symbol, String> {
        if let Some(symbol) = self.symbols.get(name) {
            return Ok(symbol);
        }
        let suffix = format!(".{}", name);
        let matches: Vec<&Symbol> = self
            .symbols
            .values()
            .filter(|s| s.name.ends_with(&suffix))
            .collect();
        match matches.as_slice() {
            [one] => Ok(one),
            [] => Err(format!(
                "unknown symbol `{}`; known: {}",
                name,
                self.symbols.keys().cloned().collect::<Vec<_>>().join(", ")
            )),
            many => Err(format!(
                "ambiguous symbol `{}`: {}",
                name,
                many.iter()
                    .map(|s| s.name.clone())
                    .collect::<Vec<_>>()
                    .join(", ")
            )),
        }
    }

    fn code_at(&self, line: usize) -> &str {
        line.checked_sub(1)
            .and_then(|i| self.lines.get(i))
            .map(|l| l.trim())
            .unwrap_or("")
    }

    fn source_of(&self, first_line: usize, last_line: usize) -> String {
        if first_line == 0 {
            return String::new();
        }
        self.lines[first_line - 1..last_line.min(self.lines.len())].join("\n")
    }

    fn symbol_json(&self, symbol: &Symbol) -> Value {
        json!({
            "name": symbol.name,
            "kind": if symbol.class.is_some() { "method" } else { "function" },
            "signature": symbol.signature,
            "lines": [symbol.first_line, symbol.last_line],
        })
    }

    fn site_json(&self, site: &CallSite) -> Value {
        json!({
            "caller": site.caller,
            "callee": site.callee,
            "line": site.line,
            "column": site.column,
            "code": self.code_at(site.line),
            "certain": site.certain,
        })
    }

    fn envelope(&self, query: &str, target: Option<&Symbol>) -> serde_json::Map<String, Value> {
        let mut out = serde_json::Map::new();
        out.insert("query".into(), json!(query));
        if let Some(target) = target {
            out.insert("target".into(), self.symbol_json(target));
        }
        // Exact = nothing in the file escapes the graph
        out.insert("exact".into(), json!(self.notes.is_empty()));
        out.insert("notes".into(), json!(self.notes));
        out
    }

    /// Functions nothing else calls: the starting points of every call chain
    fn roots(&self) -> BTreeSet<&str> {
        self.symbols
            .keys()
            .filter(|name| {
                !self
                    .calls
                    .iter()
                    .any(|c| &c.callee == *name && &c.caller != *name)
            })
            .map(|s| s.as_str())
            .collect()
    }

    /// All symbols in the file with signatures and line ranges
    pub fn query_symbols(&self) -> Value {
        let mut out = self.envelope("symbols", None);
        out.insert(
            "symbols".into(),
            Value::Array(self.symbols.values().map(|s| self.symbol_json(s)).collect()),
        );
        out.insert(
            "classes".into(),
            Value::Array(
                self.classes
                    .iter()
                    .map(|(name, info)| {
                        json!({
                            "name": name,
                            "lines": [info.first_line, info.last_line],
                            "methods": info.methods,
                        })
                    })
                    .collect(),
            ),
        );
        Value::Object(out)
    }

    /// Every call site of `name`, and the call chains that reach it from the roots
    pub fn query_callers(&self, name: &str, max_paths: usize) -> Result<Value, String> {
        let target = self.resolve(name)?;
        let sites: Vec<&CallSite> = self
            .calls
            .iter()
            .filter(|c| c.callee == target.name)
            .collect();
        let callers: BTreeSet<&str> = sites.iter().map(|c| c.caller.as_str()).collect();

        // Walk the graph backwards from the target to the roots
        let roots = self.roots();
        let mut paths: Vec<Vec<String>> = Vec::new();
        let mut truncated = false;
        let mut stack = vec![vec![target.name.as_str()]];
        while let Some(path) = stack.pop() {
            let head = *path.last().expect("non-empty path");
            let up: BTreeSet<&str> = self
                .calls
                .iter()
                .filter(|c| c.callee == head && !path.contains(&c.caller.as_str()))
                .map(|c| c.caller.as_str())
                .collect();
            if up.is_empty() || roots.contains(head) {
                if path.len() > 1 {
                    if paths.len() >= max_paths {
                        truncated = true;
                        break;
                    }
                    paths.push(path.iter().rev().map(|s| s.to_string()).collect());
                }
                if up.is_empty() {
                    continue;
                }
            }
            for caller in up.into_iter().rev() {
                let mut next = path.clone();
                next.push(caller);
                stack.push(next);
            }
        }

        let mut out = self.envelope("callers", Some(target));
        out.insert(
            "call_sites".into(),
            Value::Array(sites.iter().map(|s| self.site_json(s)).collect()),
        );
        out.insert("callers".into(), json!(callers));
        out.insert("paths".into(), json!(paths));
        out.insert("paths_truncated".into(), json!(truncated));
        Ok(Value::Object(out))
    }

    /// What `name` calls: directly, and everything reachable from it
    pub fn query_callees(&self, name: &str) -> Result<Value, String> {
        let target = self.resolve(name)?;
        let direct: Vec<&CallSite> = self
            .calls
            .iter()
            .filter(|c| c.caller == target.name)
            .collect();
        let mut reachable: BTreeSet<&str> = BTreeSet::new();
        let mut queue = vec![target.name.as_str()];
        while let Some(current) = queue.pop() {
            for call in self.calls.iter().filter(|c| c.caller == current) {
                if reachable.insert(call.callee.as_str()) {
                    queue.push(call.callee.as_str());
                }
            }
        }
        let mut out = self.envelope("callees", Some(target));
        out.insert(
            "call_sites".into(),
            Value::Array(direct.iter().map(|s| self.site_json(s)).collect()),
        );
        out.insert("reachable".into(), json!(reachable));
        Ok(Value::Object(out))
    }

    /// The minimal code needed to change `name`: its body, the signatures of what
    /// it calls, the classes it touches and the lines that call it.
    pub fn query_slice(&self, name: &str) -> Result<Value, String> {
        let target = self.resolve(name)?;
        let callees: BTreeSet<&str> = self
            .calls
            .iter()
            .filter(|c| c.caller == target.name && c.callee != target.name)
            .map(|c| c.callee.as_str())
            .collect();
        let mut used_classes: BTreeSet<&str> = self
            .class_uses
            .get(&target.name)
            .into_iter()
            .flatten()
            .map(|s| s.as_str())
            .collect();
        if let Some(class) = &target.class {
            used_classes.insert(class.as_str());
        }
        let sites: Vec<&CallSite> = self
            .calls
            .iter()
            .filter(|c| c.callee == target.name && c.caller != target.name)
            .collect();
        let source = self.source_of(target.first_line, target.last_line);

        let mut text = format!(
            "# {} (lines {}-{})\n{}\n",
            target.name, target.first_line, target.last_line, source
        );
        if !callees.is_empty() {
            text.push_str("\n# calls\n");
            for callee in &callees {
                let s = &self.symbols[*callee];
                text.push_str(&format!(
                    "{}  # {}, line {}\n",
                    s.signature, s.name, s.first_line
                ));
            }
        }
        let mut classes_json = Vec::new();
        for class in &used_classes {
            let Some(info) = self.classes.get(*class) else {
                continue;
            };
            // Class header and fields: everything before its first method
            let fields_end = info
                .methods
                .iter()
                .filter_map(|m| self.symbols.get(m))
                .map(|s| s.first_line.saturating_sub(1))
                .min()
                .unwrap_or(info.last_line);
            let header = self.source_of(info.first_line, fields_end);
            let header = header.trim_end();
            let signatures: Vec<&str> = info
                .methods
                .iter()
                .filter_map(|m| self.symbols.get(m))
                .map(|s| s.signature.as_str())
                .collect();
            text.push_str(&format!(
                "\n# class {} (line {})\n{}\n",
                class, info.first_line, header
            ));
            for signature in &signatures {
                text.push_str(&format!("    {}\n", signature));
            }
            classes_json.push(json!({
                "name": class,
                "lines": [info.first_line, info.last_line],
                "header": header,
                "methods": signatures,
            }));
        }
        if !sites.is_empty() {
            text.push_str("\n# called from\n");
            for site in &sites {
                text.push_str(&format!(
                    "{}:{}: {}\n",
                    site.caller,
                    site.line,
                    self.code_at(site.line)
                ));
            }
        }

        let mut out = self.envelope("slice", Some(target));
        out.insert("source".into(), json!(source));
        out.insert(
            "callees".into(),
            Value::Array(
                callees
                    .iter()
                    .map(|c| self.symbol_json(&self.symbols[*c]))
                    .collect(),
            ),
        );
        out.insert("classes".into(), Value::Array(classes_json));
        out.insert(
            "called_from".into(),
            Value::Array(sites.iter().map(|s| self.site_json(s)).collect()),
        );
        out.insert("text".into(), json!(text));
        Ok(Value::Object(out))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;

    const PROGRAM: &str = "fn square(x: int) -> int:
    return x * x

fn sum_squares(n: int) -> int:
    var total = 0
    var i = 1
    while i <= n:
        total = total + square(i)
        i = i + 1
    return total

fn report(n: int) -> int:
    print(sum_squares(n))
    return square(n) + square(n + 1)

fn main() -> int:
    return report(3)
";

    fn with_index<T>(source: &str, f: impl FnOnce(&ProgramIndex) -> T) -> T {
        let program = parse(source).expect("program parses");
        f(&ProgramIndex::build(&program, source))
    }

    #[test]
    fn test_callers_lists_every_call_site_with_position() {
        with_index(PROGRAM, |index| {
            let result = index.query_callers("square", 20).unwrap();
            assert_eq!(result["exact"], json!(true));
            let sites: Vec<(String, u64, String)> = result["call_sites"]
                .as_array()
                .unwrap()
                .iter()
                .map(|s| {
                    (
                        s["caller"].as_str().unwrap().to_string(),
                        s["line"].as_u64().unwrap(),
                        s["code"].as_str().unwrap().to_string(),
                    )
                })
                .collect();
            assert_eq!(
                sites,
                vec![
                    (
                        "sum_squares".to_string(),
                        8,
                        "total = total + square(i)".to_string()
                    ),
                    (
                        "report".to_string(),
                        14,
                        "return square(n) + square(n + 1)".to_string()
                    ),
                    (
                        "report".to_string(),
                        14,
                        "return square(n) + square(n + 1)".to_string()
                    ),
                ]
            );
            assert_eq!(result["call_sites"][1]["column"], json!(12));
            assert_eq!(result["call_sites"][2]["column"], json!(24));
        });
    }

    #[test]
    fn test_callers_paths_start_at_roots() {
        with_index(PROGRAM, |index| {
            let result = index.query_callers("square", 20).unwrap();
            assert_eq!(
                result["paths"],
                json!([
                    ["main", "report", "square"],
                    ["main", "report", "sum_squares", "square"]
                ])
            );
            assert_eq!(result["paths_truncated"], json!(false));
            let limited = index.query_callers("square", 1).unwrap();
            assert_eq!(limited["paths"].as_array().unwrap().len(), 1);
            assert_eq!(limited["paths_truncated"], json!(true));
        });
    }

    #[test]
    fn test_definition_line_ranges() {
        with_index(PROGRAM, |index| {
            let ranges: Vec<(String, usize, usize)> = index
                .symbols()
                .map(|s| (s.name.clone(), s.first_line, s.last_line))
                .collect();
            assert_eq!(
                ranges,
                vec![
                    ("main".to_string(), 16, 17),
                    ("report".to_string(), 12, 14),
                    ("square".to_string(), 1, 2),
                    ("sum_squares".to_string(), 4, 10),
                ]
            );
        });
    }

    #[test]
    fn test_callees_and_recursion() {
        let source = "fn fact(n: int) -> int:
    if n <= 1:
        return 1
    return n * fact(n - 1)

fn main() -> int:
    return fact(5)
";
        with_index(source, |index| {
            let callees = index.query_callees("main").unwrap();
            assert_eq!(callees["reachable"], json!(["fact"]));
            let callers = index.query_callers("fact", 20).unwrap();
            assert_eq!(callers["callers"], json!(["fact", "main"]));
            // A recursive call does not produce an endless chain
            assert_eq!(callers["paths"], json!([["main", "fact"]]));
        });
    }

    #[test]
    fn test_method_calls_resolve_through_receiver_type() {
        let source = "class Counter:
    let count: int

    fn bump(step: int) -> int:
        count = count + step
        return count

class Meter:
    let level: int

    fn bump(step: int) -> int:
        level = level + step
        return level

fn main() -> int:
    var c = new Counter(0)
    c.bump(2)
    return c.bump(3)
";
        with_index(source, |index| {
            let result = index.query_callers("Counter.bump", 20).unwrap();
            assert_eq!(result["exact"], json!(true));
            let lines: Vec<u64> = result["call_sites"]
                .as_array()
                .unwrap()
                .iter()
                .map(|s| s["line"].as_u64().unwrap())
                .collect();
            assert_eq!(lines, vec![17, 18]);
            let other = index.query_callers("Meter.bump", 20).unwrap();
            assert_eq!(other["call_sites"], json!([]));
            // A bare method name shared by two classes must be qualified
            assert!(index
                .query_callers("bump", 20)
                .unwrap_err()
                .contains("ambiguous"));
        });
    }

    #[test]
    fn test_unknown_receiver_is_reported_not_dropped() {
        let source = "class Counter:
    let count: int

    fn bump(step: int) -> int:
        count = count + step
        return count

fn make() -> dynamic:
    return new Counter(0)

fn main() -> int:
    var c = make()
    return c.bump(1)
";
        with_index(source, |index| {
            let result = index.query_callers("Counter.bump", 20).unwrap();
            assert_eq!(result["exact"], json!(false));
            assert_eq!(result["call_sites"][0]["certain"], json!(false));
            assert_eq!(result["call_sites"][0]["line"], json!(13));
        });
    }

    #[test]
    fn test_slice_contains_body_signatures_and_call_sites() {
        with_index(PROGRAM, |index| {
            let result = index.query_slice("sum_squares").unwrap();
            let text = result["text"].as_str().unwrap();
            assert!(text.starts_with("# sum_squares (lines 4-10)\nfn sum_squares(n: int) -> int:"));
            assert!(text.contains("# calls\nfn square(x: int) -> int  # square, line 1"));
            assert!(text.contains("# called from\nreport:13: print(sum_squares(n))"));
            // Bodies of other functions stay out of the slice
            assert!(!text.contains("return x * x"));
        });
    }

    #[test]
    fn test_unknown_symbol_lists_known_ones() {
        with_index(PROGRAM, |index| {
            let err = index.query_callers("nope", 20).unwrap_err();
            assert!(err.contains("unknown symbol `nope`"));
            assert!(err.contains("sum_squares"));
        });
    }
}
