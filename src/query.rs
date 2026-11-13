// Query module - structural questions about a program, answered for tools and AI agents:
// who calls a function, what it calls, and the minimal code needed to change it.
//
// The call graph comes from the AST. Pain has no function values, reflection or
// dynamic dispatch, so a resolved edge is a fact, not a guess. Whatever cannot be
// resolved is reported in `notes` and turns `exact` off instead of being dropped.
//
// A program may span several files (see modules.rs). Symbols of imported modules
// are named `module.name`, the same way the linked program names them.

use crate::ast::*;
use crate::lexer::{tokenize_with_indentation, Token};
use crate::modules::ModuleGraph;
use serde_json::{json, Value};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

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
    /// `name` for functions, `Class.name` for methods; prefixed with `module.` outside the entry file
    pub name: String,
    pub class: Option<String>,
    /// Dotted module name, empty for the entry file
    pub module: String,
    /// Index into the files of the index
    pub file: usize,
    pub public: bool,
    pub signature: String,
    pub first_line: usize,
    pub last_line: usize,
}

/// One call from `caller` to `callee`
#[derive(Debug, Clone)]
pub struct CallSite {
    pub caller: String,
    pub callee: String,
    pub file: usize,
    /// 0 when the position could not be recovered
    pub line: usize,
    pub column: usize,
    /// false when a method call was matched by name only (receiver type unknown)
    pub certain: bool,
}

#[derive(Debug, Clone)]
struct ClassInfo {
    file: usize,
    first_line: usize,
    last_line: usize,
    methods: Vec<String>,
}

struct SourceFile {
    /// Path as given to the compiler; empty for a program built from a string
    path: String,
    lines: Vec<String>,
}

/// One file to index: its AST, its text and what its names resolve to
struct ModuleInput<'a> {
    name: &'a str,
    path: String,
    source: &'a str,
    program: &'a Program,
    /// Name visible in the file -> name in the whole program
    scope: HashMap<String, String>,
}

/// Call graph and definition ranges of a program
pub struct ProgramIndex {
    files: Vec<SourceFile>,
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

/// Program-wide facts the body walker resolves names against
struct Globals {
    functions: HashSet<String>,
    classes: HashSet<String>,
    /// Method name -> classes that define it
    methods: BTreeMap<String, Vec<String>>,
    /// Function -> class it returns
    return_classes: HashMap<String, String>,
}

struct BodyWalker<'p> {
    globals: &'p Globals,
    scope: &'p HashMap<String, String>,
    locals: HashMap<String, Option<String>>,
    owner: String,
    calls: Vec<RawCall>,
    class_uses: BTreeSet<String>,
    notes: Vec<String>,
}

/// Class named by a type, as the program names it
fn named_class(ty: &Type, scope: &HashMap<String, String>) -> Option<String> {
    match ty {
        Type::Named(name) => Some(scope.get(name).cloned().unwrap_or_else(|| name.clone())),
        _ => None,
    }
}

impl BodyWalker<'_> {
    fn function_named(&self, name: &str) -> Option<&String> {
        self.scope
            .get(name)
            .filter(|target| self.globals.functions.contains(*target))
    }

    fn class_named(&self, name: &str) -> String {
        self.scope
            .get(name)
            .filter(|target| self.globals.classes.contains(*target))
            .cloned()
            .unwrap_or_else(|| name.to_string())
    }

    fn class_of(&self, expr: &Expr) -> Option<String> {
        match expr {
            Expr::Ident(name) => self.locals.get(name).cloned().flatten(),
            Expr::New { class_name, .. } => Some(self.class_named(class_name)),
            Expr::Call { callee, .. } => match callee.as_ref() {
                Expr::Ident(name) if !self.locals.contains_key(name) => self
                    .function_named(name)
                    .and_then(|f| self.globals.return_classes.get(f).cloned()),
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
                    .and_then(|ty| named_class(ty, self.scope))
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
                if !self.locals.contains_key(name) && self.function_named(name).is_some() {
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
                self.class_uses.insert(self.class_named(class_name));
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
        } else {
            // Not a function of the program: builtin / stdlib
            self.function_named(name).cloned()
        };
        self.calls.push(RawCall {
            kind: CallKind::Direct,
            name: name.to_string(),
            callee,
            certain: true,
        });
    }

    fn method_call(&mut self, receiver: &Expr, method: &str) {
        let owners = self
            .globals
            .methods
            .get(method)
            .cloned()
            .unwrap_or_default();
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

/// A definition in the AST: (is class, name, owning class, function, public)
type AstDef<'a> = (bool, String, Option<String>, Option<&'a Function>, bool);

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
                // The definition starts at `pub` when there is one
                let mut header = &tokens[i - 1];
                if i >= 2 && matches!(tokens[i - 2].token, Token::Pub) {
                    header = &tokens[i - 2];
                }
                scan.defs.push(TokenDef {
                    is_class: matches!(tokens[i - 1].token, Token::Class),
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

impl ProgramIndex {
    /// Index a single-file program
    pub fn build(program: &Program, source: &str) -> Self {
        let scope = program
            .items
            .iter()
            .map(|item| match item {
                Item::Function(f) => (f.name.clone(), f.name.clone()),
                Item::Class(c) => (c.name.clone(), c.name.clone()),
            })
            .collect();
        Self::build_from(&[ModuleInput {
            name: "",
            path: String::new(),
            source,
            program,
            scope,
        }])
    }

    /// Index every file of a multi-module program
    pub fn from_graph(graph: &ModuleGraph) -> Self {
        let inputs: Vec<ModuleInput> = graph
            .modules()
            .iter()
            .map(|module| ModuleInput {
                name: &module.name,
                path: module.path.display().to_string().replace('\\', "/"),
                source: &module.source,
                program: &module.program,
                scope: graph.scope(module),
            })
            .collect();
        Self::build_from(&inputs)
    }

    fn build_from(inputs: &[ModuleInput]) -> Self {
        let mut notes = Vec::new();

        // Program-wide tables first: a call may target any file
        let mut globals = Globals {
            functions: HashSet::new(),
            classes: HashSet::new(),
            methods: BTreeMap::new(),
            return_classes: HashMap::new(),
        };
        let mut class_fields: HashMap<String, Vec<(String, Option<String>)>> = HashMap::new();
        for input in inputs {
            let qualify = |name: &str| {
                input
                    .scope
                    .get(name)
                    .cloned()
                    .unwrap_or_else(|| name.to_string())
            };
            for item in &input.program.items {
                match item {
                    Item::Function(f) => {
                        let name = qualify(&f.name);
                        if let Some(class) = f
                            .return_type
                            .as_ref()
                            .and_then(|ty| named_class(ty, &input.scope))
                        {
                            globals.return_classes.insert(name.clone(), class);
                        }
                        globals.functions.insert(name);
                    }
                    Item::Class(class) => {
                        let name = qualify(&class.name);
                        for method in &class.methods {
                            globals
                                .methods
                                .entry(method.name.clone())
                                .or_default()
                                .push(name.clone());
                        }
                        class_fields.insert(
                            name.clone(),
                            class
                                .fields
                                .iter()
                                .map(|f| (f.name.clone(), named_class(&f.ty, &input.scope)))
                                .collect(),
                        );
                        globals.classes.insert(name);
                    }
                }
            }
        }

        let mut files = Vec::new();
        let mut symbols = BTreeMap::new();
        let mut classes = BTreeMap::new();
        let mut calls = Vec::new();
        let mut class_uses = BTreeMap::new();

        for (file, input) in inputs.iter().enumerate() {
            let scan = scan_tokens(input.source);
            let lines: Vec<String> = input.source.lines().map(str::to_string).collect();
            let qualify = |name: &str| {
                input
                    .scope
                    .get(name)
                    .cloned()
                    .unwrap_or_else(|| name.to_string())
            };

            // Definitions in source order: the token stream and the AST must agree
            let mut ast_defs: Vec<AstDef> = Vec::new();
            for item in &input.program.items {
                match item {
                    Item::Function(func) => {
                        ast_defs.push((false, func.name.clone(), None, Some(func), func.public))
                    }
                    Item::Class(class) => {
                        ast_defs.push((true, class.name.clone(), None, None, class.public));
                        for method in &class.methods {
                            ast_defs.push((
                                false,
                                method.name.clone(),
                                Some(qualify(&class.name)),
                                Some(method),
                                class.public,
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
                notes.push(format!(
                    "{}definitions could not be matched to source lines",
                    if input.path.is_empty() {
                        String::new()
                    } else {
                        format!("{}: ", input.path)
                    }
                ));
            }

            for (i, (is_class, name, class, func, public)) in ast_defs.iter().enumerate() {
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
                        qualify(name),
                        ClassInfo {
                            file,
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
                    None => qualify(name),
                };
                if let Some(class) = class {
                    if let Some(info) = classes.get_mut(class) {
                        info.methods.push(full_name.clone());
                    }
                }

                let mut walker = BodyWalker {
                    globals: &globals,
                    scope: &input.scope,
                    locals: HashMap::new(),
                    owner: full_name.clone(),
                    calls: Vec::new(),
                    class_uses: BTreeSet::new(),
                    notes: Vec::new(),
                };
                if let Some(class) = class {
                    // Fields are visible by bare name inside methods
                    for (field, field_class) in class_fields.get(class).into_iter().flatten() {
                        walker.locals.insert(field.clone(), field_class.clone());
                    }
                    walker
                        .locals
                        .insert("self".to_string(), Some(class.clone()));
                }
                for param in &func.params {
                    let param_class = named_class(&param.ty, &input.scope);
                    if let Some(c) = &param_class {
                        walker.class_uses.insert(c.clone());
                    }
                    walker.locals.insert(param.name.clone(), param_class);
                }
                if let Some(c) = func
                    .return_type
                    .as_ref()
                    .and_then(|ty| named_class(ty, &input.scope))
                {
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
                            file,
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
                        class: class.clone(),
                        module: input.name.to_string(),
                        file,
                        public: *public,
                        signature,
                        first_line,
                        last_line,
                    },
                );
            }
            files.push(SourceFile {
                path: input.path.clone(),
                lines,
            });
        }

        notes.sort();
        notes.dedup();
        Self {
            files,
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

    /// Resolve a user-supplied name: exact, or given without its module / class
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

    fn path_of(&self, file: usize) -> &str {
        &self.files[file].path
    }

    fn code_at(&self, file: usize, line: usize) -> &str {
        line.checked_sub(1)
            .and_then(|i| self.files[file].lines.get(i))
            .map(|l| l.trim())
            .unwrap_or("")
    }

    fn source_of(&self, file: usize, first_line: usize, last_line: usize) -> String {
        if first_line == 0 {
            return String::new();
        }
        let lines = &self.files[file].lines;
        lines[first_line - 1..last_line.min(lines.len())].join("\n")
    }

    /// `path:` prefix for text output; nothing for a program without file names
    fn file_prefix(&self, file: usize) -> String {
        match self.path_of(file) {
            "" => String::new(),
            path => format!("{}:", path),
        }
    }

    fn symbol_json(&self, symbol: &Symbol) -> Value {
        let mut out = json!({
            "name": symbol.name,
            "kind": if symbol.class.is_some() { "method" } else { "function" },
            "signature": symbol.signature,
            "lines": [symbol.first_line, symbol.last_line],
        });
        if !self.path_of(symbol.file).is_empty() {
            out["file"] = json!(self.path_of(symbol.file));
            out["module"] = json!(symbol.module);
            out["public"] = json!(symbol.public);
        }
        out
    }

    fn site_json(&self, site: &CallSite) -> Value {
        let mut out = json!({
            "caller": site.caller,
            "callee": site.callee,
            "line": site.line,
            "column": site.column,
            "code": self.code_at(site.file, site.line),
            "certain": site.certain,
        });
        if !self.path_of(site.file).is_empty() {
            out["file"] = json!(self.path_of(site.file));
        }
        out
    }

    fn envelope(&self, query: &str, target: Option<&Symbol>) -> serde_json::Map<String, Value> {
        let mut out = serde_json::Map::new();
        out.insert("query".into(), json!(query));
        if let Some(target) = target {
            out.insert("target".into(), self.symbol_json(target));
        }
        // Exact = nothing in the program escapes the graph
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

    /// Functions and methods that can never run. With a `main`, everything not
    /// reachable from it; exported (`pub`) items of imported modules also count as
    /// entry points, because another program may import them. Without a `main`
    /// (a library file), everything that is not `pub` and that nothing else calls.
    pub fn unused_symbols(&self) -> BTreeSet<String> {
        let exported = |s: &Symbol| s.public && !s.module.is_empty();
        if !self.symbols.contains_key("main") {
            return self
                .symbols
                .values()
                .filter(|s| !s.public)
                .filter(|s| {
                    !self
                        .calls
                        .iter()
                        .any(|c| c.callee == s.name && c.caller != s.name)
                })
                .map(|s| s.name.clone())
                .collect();
        }
        let mut reached: HashSet<&str> = HashSet::new();
        let mut queue: Vec<&str> = self
            .symbols
            .values()
            .filter(|s| s.name == "main" || exported(s))
            .map(|s| s.name.as_str())
            .collect();
        reached.extend(queue.iter().copied());
        while let Some(current) = queue.pop() {
            for call in self.calls.iter().filter(|c| c.caller == current) {
                if reached.insert(call.callee.as_str()) {
                    queue.push(call.callee.as_str());
                }
            }
        }
        self.symbols
            .keys()
            .filter(|name| !reached.contains(name.as_str()))
            .cloned()
            .collect()
    }

    /// All symbols of the program with signatures and line ranges
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
                        let mut class = json!({
                            "name": name,
                            "lines": [info.first_line, info.last_line],
                            "methods": info.methods,
                        });
                        if !self.path_of(info.file).is_empty() {
                            class["file"] = json!(self.path_of(info.file));
                        }
                        class
                    })
                    .collect(),
            ),
        );
        Value::Object(out)
    }

    /// Every call site of `name`, the call chains that reach it from the roots,
    /// and everything it can be reached from.
    pub fn query_callers(&self, name: &str, max_paths: usize) -> Result<Value, String> {
        let target = self.resolve(name)?;
        let sites: Vec<&CallSite> = self
            .calls
            .iter()
            .filter(|c| c.callee == target.name)
            .collect();
        let callers: BTreeSet<&str> = sites.iter().map(|c| c.caller.as_str()).collect();

        // Everything that reaches the target, directly or through other calls
        let mut reachable_from: BTreeSet<&str> = BTreeSet::new();
        let mut queue = vec![target.name.as_str()];
        while let Some(current) = queue.pop() {
            for call in self.calls.iter().filter(|c| c.callee == current) {
                if call.caller != target.name && reachable_from.insert(call.caller.as_str()) {
                    queue.push(call.caller.as_str());
                }
            }
        }

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
        out.insert("reachable_from".into(), json!(reachable_from));
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
        let source = self.source_of(target.file, target.first_line, target.last_line);

        let mut text = format!(
            "# {} ({}lines {}-{})\n{}\n",
            target.name,
            match self.path_of(target.file) {
                "" => String::new(),
                path => format!("{}, ", path),
            },
            target.first_line,
            target.last_line,
            source
        );
        if !callees.is_empty() {
            text.push_str("\n# calls\n");
            for callee in &callees {
                let s = &self.symbols[*callee];
                text.push_str(&format!(
                    "{}  # {}, {}line {}\n",
                    s.signature,
                    s.name,
                    match self.path_of(s.file) {
                        "" => String::new(),
                        path => format!("{}, ", path),
                    },
                    s.first_line
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
            let header = self.source_of(info.file, info.first_line, fields_end);
            let header = header.trim_end();
            let signatures: Vec<&str> = info
                .methods
                .iter()
                .filter_map(|m| self.symbols.get(m))
                .map(|s| s.signature.as_str())
                .collect();
            text.push_str(&format!(
                "\n# class {} ({}line {})\n{}\n",
                class,
                match self.path_of(info.file) {
                    "" => String::new(),
                    path => format!("{}, ", path),
                },
                info.first_line,
                header
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
                    "{}:{}{}: {}\n",
                    site.caller,
                    self.file_prefix(site.file),
                    site.line,
                    self.code_at(site.file, site.line)
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

    fn with_graph<T>(files: &[(&str, &str)], f: impl FnOnce(&ProgramIndex) -> T) -> T {
        use std::path::PathBuf;
        let files: HashMap<PathBuf, String> = files
            .iter()
            .map(|(path, source)| (PathBuf::from("proj").join(path), source.to_string()))
            .collect();
        let graph = ModuleGraph::load_with(&PathBuf::from("proj").join("main.pain"), &|path| {
            files
                .get(path)
                .cloned()
                .ok_or_else(|| std::io::Error::new(std::io::ErrorKind::NotFound, "no such file"))
        })
        .expect("modules load");
        f(&ProgramIndex::from_graph(&graph))
    }

    const MODULES: &[(&str, &str)] = &[
        (
            "main.pain",
            "import combat: strike, Unit
import ai.target: pick

fn main() -> int:
    var knight = new Unit(40, 9)
    knight.rest(1)
    return strike(knight, pick(3))
",
        ),
        (
            "combat.pain",
            "pub class Unit:
    let hp: int
    let attack: int

    fn rest(turns: int) -> int:
        return hp + turns

pub fn strike(attacker: Unit, seed: int) -> int:
    return clamp(attacker.attack + seed)

fn clamp(value: int) -> int:
    return value % 100
",
        ),
        (
            "ai/target.pain",
            "import combat: strike, Unit

pub fn pick(seed: int) -> int:
    return strike(new Unit(1, 2), seed) + clamp(seed)

fn clamp(seed: int) -> int:
    return seed
",
        ),
    ];

    #[test]
    fn test_callers_across_modules() {
        with_graph(MODULES, |index| {
            let result = index.query_callers("combat.strike", 20).unwrap();
            assert_eq!(result["exact"], json!(true));
            let sites: Vec<(String, String, u64)> = result["call_sites"]
                .as_array()
                .unwrap()
                .iter()
                .map(|s| {
                    (
                        s["caller"].as_str().unwrap().to_string(),
                        s["file"].as_str().unwrap().to_string(),
                        s["line"].as_u64().unwrap(),
                    )
                })
                .collect();
            assert_eq!(
                sites,
                vec![
                    ("main".to_string(), "proj/main.pain".to_string(), 7),
                    (
                        "ai.target.pick".to_string(),
                        "proj/ai/target.pain".to_string(),
                        4
                    ),
                ]
            );
            assert_eq!(result["target"]["file"], json!("proj/combat.pain"));
            assert_eq!(result["target"]["public"], json!(true));
            assert_eq!(result["reachable_from"], json!(["ai.target.pick", "main"]));
        });
    }

    #[test]
    fn test_private_functions_with_one_name_stay_apart() {
        with_graph(MODULES, |index| {
            // Each module has its own private `clamp`
            let combat = index.query_callers("combat.clamp", 20).unwrap();
            assert_eq!(combat["callers"], json!(["combat.strike"]));
            let ai = index.query_callers("ai.target.clamp", 20).unwrap();
            assert_eq!(ai["callers"], json!(["ai.target.pick"]));
            assert!(index
                .query_callers("clamp", 20)
                .unwrap_err()
                .contains("ambiguous"));
            // The short name works when it is unique
            assert_eq!(
                index.query_callers("pick", 20).unwrap()["target"]["name"],
                json!("ai.target.pick")
            );
        });
    }

    #[test]
    fn test_method_of_imported_class_and_slice_across_files() {
        with_graph(MODULES, |index| {
            let rest = index.query_callers("combat.Unit.rest", 20).unwrap();
            assert_eq!(rest["call_sites"][0]["caller"], json!("main"));
            assert_eq!(rest["call_sites"][0]["line"], json!(6));
            let text = index.query_slice("main").unwrap()["text"]
                .as_str()
                .unwrap()
                .to_string();
            assert!(text.starts_with("# main (proj/main.pain, lines 4-7)\nfn main() -> int:"));
            assert!(text.contains(
                "pub fn strike(attacker: Unit, seed: int) -> int  # combat.strike, proj/combat.pain, line 8"
            ));
            assert!(
                text.contains("# class combat.Unit (proj/combat.pain, line 1)\npub class Unit:")
            );
            // Bodies from other files stay out of the slice
            assert!(!text.contains("return clamp("));
        });
    }

    #[test]
    fn test_unused_symbols_follow_reachability_from_main() {
        let source = "class Counter:
    let count: int

    fn bump(step: int) -> int:
        return count + step

    fn reset() -> int:
        return 0

fn helper() -> int:
    return orphan_callee()

fn orphan_callee() -> int:
    return 1

fn used() -> int:
    return 2

fn main() -> int:
    var c = new Counter(0)
    return c.bump(used())
";
        with_index(source, |index| {
            // A called method is used; a function called only from dead code is not
            assert_eq!(
                index.unused_symbols().into_iter().collect::<Vec<_>>(),
                vec!["Counter.reset", "helper", "orphan_callee"]
            );
        });
        // Without main: only what nothing calls
        with_index(
            "fn a() -> int:\n    return b()\n\nfn b() -> int:\n    return 1\n",
            |index| {
                assert_eq!(
                    index.unused_symbols().into_iter().collect::<Vec<_>>(),
                    vec!["a"]
                );
            },
        );
    }

    #[test]
    fn test_exported_items_of_modules_are_not_unused() {
        with_graph(MODULES, |index| {
            // `combat.Unit.rest` is called; both private `clamp`s are called
            assert!(index.unused_symbols().is_empty());
        });
        let files: &[(&str, &str)] = &[
            ("main.pain", "import lib: used\n\nfn main() -> int:\n    return used()\n"),
            (
                "lib.pain",
                "pub fn used() -> int:\n    return 1\n\npub fn api() -> int:\n    return helper()\n\nfn helper() -> int:\n    return 2\n\nfn stale() -> int:\n    return 3\n",
            ),
        ];
        with_graph(files, |index| {
            // `api` is exported, so it and its helper stay; only `stale` is dead
            assert_eq!(
                index.unused_symbols().into_iter().collect::<Vec<_>>(),
                vec!["lib.stale"]
            );
        });
    }

    #[test]
    fn test_reachable_from_lists_transitive_callers() {
        with_index(PROGRAM, |index| {
            let result = index.query_callers("square", 20).unwrap();
            assert_eq!(
                result["reachable_from"],
                json!(["main", "report", "sum_squares"])
            );
        });
    }
}
