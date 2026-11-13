// Modules - loading a program that spans several files.
//
// A module is a file. `import a.b: x, y` takes the `pub` names `x` and `y` from
// `a/b.pain`, looked up from the directory of the entry file. There is one import
// form on purpose: every imported name is listed, no wildcards, no aliases.
//
// The rest of the compiler works on a single `Program`, so the graph is linked into
// one: items of imported modules are renamed to `module.name` and every reference
// is rewritten to the definition it resolves to.

use crate::ast::*;
use crate::parser::parse;
use crate::stdlib::is_stdlib_function;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::path::{Path, PathBuf};

/// One source file of the program
#[derive(Debug, Clone)]
pub struct Module {
    /// Dotted module name as written in imports; empty for the entry file
    pub name: String,
    pub path: PathBuf,
    pub source: String,
    pub program: Program,
}

impl Module {
    /// Name of a top-level item of this module in the linked program
    pub fn qualified(&self, item: &str) -> String {
        if self.name.is_empty() {
            item.to_string()
        } else {
            format!("{}.{}", self.name, item)
        }
    }

    fn item_names(&self) -> impl Iterator<Item = (&str, bool)> {
        self.program.items.iter().map(|item| match item {
            Item::Function(f) => (f.name.as_str(), f.public),
            Item::Class(c) => (c.name.as_str(), c.public),
        })
    }
}

/// A problem with the module structure, tied to a file and line
#[derive(Debug, Clone, PartialEq)]
pub struct ModuleError {
    pub file: PathBuf,
    /// 0 when the problem is not tied to a line
    pub line: usize,
    pub message: String,
}

impl fmt::Display for ModuleError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.line > 0 {
            write!(f, "{}:{}: {}", self.file.display(), self.line, self.message)
        } else {
            write!(f, "{}: {}", self.file.display(), self.message)
        }
    }
}

impl std::error::Error for ModuleError {}

/// All modules reachable from an entry file; the entry is `modules()[0]`
#[derive(Debug, Clone)]
pub struct ModuleGraph {
    modules: Vec<Module>,
}

struct Loader<'a> {
    root: PathBuf,
    read: &'a dyn Fn(&Path) -> std::io::Result<String>,
    modules: Vec<Module>,
    by_path: HashMap<PathBuf, usize>,
    /// Modules being loaded, outermost first: used to report import cycles
    stack: Vec<usize>,
}

impl Loader<'_> {
    fn load(
        &mut self,
        name: String,
        path: PathBuf,
        from: Option<(&Path, usize)>,
    ) -> Result<usize, ModuleError> {
        if let Some(&index) = self.by_path.get(&path) {
            if let Some(pos) = self.stack.iter().position(|&i| i == index) {
                let (file, line) = from.expect("a cycle goes through an import");
                let mut chain: Vec<String> = self.stack[pos..]
                    .iter()
                    .map(|&i| self.display_name(i))
                    .collect();
                chain.push(self.display_name(index));
                return Err(ModuleError {
                    file: file.to_path_buf(),
                    line,
                    message: format!("import cycle: {}", chain.join(" -> ")),
                });
            }
            return Ok(index);
        }
        let source = (self.read)(&path).map_err(|e| match from {
            Some((file, line)) => ModuleError {
                file: file.to_path_buf(),
                line,
                message: format!(
                    "module `{}` not found: expected file {} ({})",
                    name,
                    path.display(),
                    e
                ),
            },
            None => ModuleError {
                file: path.clone(),
                line: 0,
                message: format!("cannot read file: {}", e),
            },
        })?;
        let program = parse(&source).map_err(|e| ModuleError {
            file: path.clone(),
            line: 0,
            message: format!("Parse error: {}", e),
        })?;
        let index = self.modules.len();
        self.modules.push(Module {
            name,
            path: path.clone(),
            source,
            program,
        });
        self.by_path.insert(path.clone(), index);
        self.stack.push(index);
        let imports = self.modules[index].program.imports.clone();
        for import in &imports {
            let mut target = self.root.clone();
            for segment in &import.module {
                target.push(segment);
            }
            target.set_extension("pain");
            self.load(
                import.module.join("."),
                target,
                Some((&path, import.span.line())),
            )?;
        }
        self.stack.pop();
        Ok(index)
    }

    fn display_name(&self, index: usize) -> String {
        let module = &self.modules[index];
        if module.name.is_empty() {
            module
                .path
                .file_stem()
                .map(|s| s.to_string_lossy().into_owned())
                .unwrap_or_default()
        } else {
            module.name.clone()
        }
    }
}

impl ModuleGraph {
    /// Load the entry file and everything it imports from disk
    pub fn load(entry: &Path) -> Result<Self, ModuleError> {
        Self::load_with(entry, &|path| std::fs::read_to_string(path))
    }

    /// Load through a custom reader (in-memory sources in tests and editors)
    pub fn load_with(
        entry: &Path,
        read: &dyn Fn(&Path) -> std::io::Result<String>,
    ) -> Result<Self, ModuleError> {
        let root = entry.parent().map(Path::to_path_buf).unwrap_or_default();
        let mut loader = Loader {
            root,
            read,
            modules: Vec::new(),
            by_path: HashMap::new(),
            stack: Vec::new(),
        };
        loader.load(String::new(), entry.to_path_buf(), None)?;
        let graph = Self {
            modules: loader.modules,
        };
        graph.check_imports()?;
        Ok(graph)
    }

    pub fn modules(&self) -> &[Module] {
        &self.modules
    }

    pub fn entry(&self) -> &Module {
        &self.modules[0]
    }

    pub fn module(&self, name: &str) -> Option<&Module> {
        self.modules.iter().find(|m| m.name == name)
    }

    /// Every import names an existing `pub` item and does not clash with anything
    fn check_imports(&self) -> Result<(), ModuleError> {
        for module in &self.modules {
            let own: HashSet<&str> = module.item_names().map(|(name, _)| name).collect();
            let mut imported: HashMap<&str, String> = HashMap::new();
            for import in &module.program.imports {
                let err = |message: String| ModuleError {
                    file: module.path.clone(),
                    line: import.span.line(),
                    message,
                };
                let target_name = import.module.join(".");
                let target = self
                    .module(&target_name)
                    .expect("imported modules are loaded");
                for name in &import.names {
                    match target.item_names().find(|(item, _)| item == name) {
                        None => {
                            let public: Vec<&str> = target
                                .item_names()
                                .filter(|(_, public)| *public)
                                .map(|(item, _)| item)
                                .collect();
                            return Err(err(format!(
                                "module `{}` has no `{}`; it exports: {}",
                                target_name,
                                name,
                                if public.is_empty() {
                                    "nothing".to_string()
                                } else {
                                    public.join(", ")
                                }
                            )));
                        }
                        Some((_, false)) => {
                            return Err(err(format!(
                                "`{}` in module `{}` is not `pub` and cannot be imported",
                                name, target_name
                            )));
                        }
                        Some((_, true)) => {}
                    }
                    if own.contains(name.as_str()) {
                        return Err(err(format!(
                            "imported name `{}` is also defined in this file",
                            name
                        )));
                    }
                    if let Some(previous) = imported.insert(name, target_name.clone()) {
                        return Err(err(format!(
                            "`{}` is imported twice (from `{}` and `{}`)",
                            name, previous, target_name
                        )));
                    }
                }
            }
        }
        Ok(())
    }

    /// What each name visible in `module` resolves to in the linked program:
    /// its own items and the names it imports.
    pub fn scope(&self, module: &Module) -> HashMap<String, String> {
        let mut scope: HashMap<String, String> = module
            .item_names()
            .map(|(name, _)| (name.to_string(), module.qualified(name)))
            .collect();
        for import in &module.program.imports {
            if let Some(target) = self.module(&import.module.join(".")) {
                for name in &import.names {
                    scope.insert(name.clone(), target.qualified(name));
                }
            }
        }
        scope
    }

    /// Merge all modules into one program for the type checker, the interpreter
    /// and the backends. Items of the entry file keep their names.
    pub fn link(&self) -> Result<Program, ModuleError> {
        let entry_names: HashSet<&str> = self.entry().item_names().map(|(name, _)| name).collect();
        let mut items = Vec::new();
        for (index, module) in self.modules.iter().enumerate() {
            let scope = self.scope(module);
            let mut renamer = Renamer {
                scope: &scope,
                class_methods: HashSet::new(),
                unresolved: Vec::new(),
            };
            for item in &module.program.items {
                let mut item = item.clone();
                match &mut item {
                    Item::Function(func) => {
                        renamer.function(func);
                        func.name = module.qualified(&func.name);
                    }
                    Item::Class(class) => {
                        renamer.class(class);
                        class.name = module.qualified(&class.name);
                    }
                }
                items.push(item);
            }
            // A name this module neither defines nor imports must not silently bind
            // to an item of another file (the entry file keeps its plain names).
            for name in &renamer.unresolved {
                let owner = self.modules.iter().enumerate().find(|(other, m)| {
                    *other != index && m.item_names().any(|(item, _)| item == name)
                });
                let Some((owner_index, owner)) = owner else {
                    continue; // defined nowhere: the type checker reports it
                };
                let here = if module.name.is_empty() {
                    "this file".to_string()
                } else {
                    format!("module `{}`", module.name)
                };
                let public = owner
                    .item_names()
                    .any(|(item, public)| item == name && public);
                let message = if owner_index == 0 || entry_names.contains(name.as_str()) && !public
                {
                    format!(
                        "`{}` is used in {} but is neither defined nor imported there",
                        name, here
                    )
                } else if public {
                    format!(
                        "`{}` is used in {} but not imported; add `import {}: {}`",
                        name, here, owner.name, name
                    )
                } else {
                    format!(
                        "`{}` is used in {} but is private to module `{}`",
                        name, here, owner.name
                    )
                };
                return Err(ModuleError {
                    file: module.path.clone(),
                    line: 0,
                    message,
                });
            }
        }
        Ok(Program {
            imports: Vec::new(),
            items,
            span: self.entry().program.span,
        })
    }
}

impl ModuleGraph {
    /// The linked program with the bodies of every other module's functions removed.
    /// Type checking it reports only the errors that belong to `module_index`, which
    /// tells which file a type error of the whole program is in.
    pub fn link_isolated(&self, module_index: usize) -> Result<Program, ModuleError> {
        let mut program = self.link()?;
        let mut start = 0;
        for (index, module) in self.modules.iter().enumerate() {
            let end = start + module.program.items.len();
            if index != module_index {
                for item in &mut program.items[start..end] {
                    match item {
                        Item::Function(func) => func.body.clear(),
                        Item::Class(class) => class.methods.iter_mut().for_each(|m| m.body.clear()),
                    }
                }
            }
            start = end;
        }
        Ok(program)
    }
}

/// Rewrites the names used in one module to the linked names
struct Renamer<'a> {
    scope: &'a HashMap<String, String>,
    /// Methods of the class being rewritten: bare calls to them are not top-level names
    class_methods: HashSet<String>,
    unresolved: Vec<String>,
}

impl Renamer<'_> {
    fn resolve(&mut self, name: &mut String, is_call: bool) {
        if let Some(target) = self.scope.get(name.as_str()) {
            *name = target.clone();
        } else if !(is_call
            && (is_stdlib_function(name) || self.class_methods.contains(name.as_str())))
        {
            self.unresolved.push(name.clone());
        }
    }

    fn ty(&mut self, ty: &mut Type) {
        match ty {
            Type::Named(name) => self.resolve(name, false),
            Type::List(inner) | Type::Array(inner) => self.ty(inner),
            Type::Map(key, value) => {
                self.ty(key);
                self.ty(value);
            }
            Type::Tensor(inner, dims) => {
                self.ty(inner);
                dims.iter_mut().for_each(|e| self.expr(e));
            }
            Type::Int | Type::Str | Type::Float32 | Type::Float64 | Type::Bool | Type::Dynamic => {}
        }
    }

    fn function(&mut self, func: &mut Function) {
        for param in &mut func.params {
            self.ty(&mut param.ty);
        }
        if let Some(ty) = &mut func.return_type {
            self.ty(ty);
        }
        self.block(&mut func.body);
    }

    fn class(&mut self, class: &mut Class) {
        self.class_methods = class.methods.iter().map(|m| m.name.clone()).collect();
        for field in &mut class.fields {
            self.ty(&mut field.ty);
        }
        for method in &mut class.methods {
            self.function(method);
        }
        self.class_methods.clear();
    }

    fn block(&mut self, stmts: &mut [Statement]) {
        for stmt in stmts {
            match stmt {
                Statement::Expr(expr) => self.expr(expr),
                Statement::Let { ty, init, .. } => {
                    if let Some(ty) = ty {
                        self.ty(ty);
                    }
                    self.expr(init);
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
                Statement::For { iter, body, .. } => {
                    self.expr(iter);
                    self.block(body);
                }
                Statement::While { cond, body } => {
                    self.expr(cond);
                    self.block(body);
                }
            }
        }
    }

    fn expr(&mut self, expr: &mut Expr) {
        match expr {
            Expr::Integer(_)
            | Expr::Float(_)
            | Expr::String(_)
            | Expr::FString(_)
            | Expr::Bool(_)
            | Expr::None
            | Expr::Ident(_) => {}
            Expr::List(items) => items.iter_mut().for_each(|e| self.expr(e)),
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
            Expr::Not(a) | Expr::Neg(a) | Expr::Member(a, _) => self.expr(a),
            Expr::IsInstance(a, ty) => {
                self.expr(a);
                self.ty(ty);
            }
            Expr::New { class_name, args } => {
                self.resolve(class_name, false);
                args.iter_mut().for_each(|e| self.expr(e));
            }
            Expr::Call { callee, args } => {
                match callee.as_mut() {
                    Expr::Ident(name) => self.resolve(name, true),
                    other => self.expr(other),
                }
                args.iter_mut().for_each(|e| self.expr(e));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::Interpreter;
    use crate::type_checker::type_check_program;

    fn graph(files: &[(&str, &str)]) -> Result<ModuleGraph, ModuleError> {
        let files: HashMap<PathBuf, String> = files
            .iter()
            .map(|(path, source)| (PathBuf::from("proj").join(path), source.to_string()))
            .collect();
        ModuleGraph::load_with(&PathBuf::from("proj").join("main.pain"), &|path| {
            files
                .get(path)
                .cloned()
                .ok_or_else(|| std::io::Error::new(std::io::ErrorKind::NotFound, "no such file"))
        })
    }

    fn error(files: &[(&str, &str)]) -> ModuleError {
        match graph(files) {
            Err(e) => e,
            Ok(g) => g.link().expect_err("expected a module error"),
        }
    }

    const COMBAT: &str = "pub class Unit:
    let hp: int
    let attack: int

pub fn strike(attacker: Unit, seed: int) -> int:
    return clamp(attacker.attack + seed, 1, 99)

fn clamp(value: int, low: int, high: int) -> int:
    if value < low:
        return low
    if value > high:
        return high
    return value
";

    #[test]
    fn test_imported_function_and_class_run() {
        let main = "import combat: strike, Unit

fn main() -> int:
    var knight = new Unit(40, 9)
    return strike(knight, 200)
";
        let program = graph(&[("main.pain", main), ("combat.pain", COMBAT)])
            .unwrap()
            .link()
            .unwrap();
        assert!(type_check_program(&program).is_ok());
        let names: Vec<String> = program
            .items
            .iter()
            .map(|item| match item {
                Item::Function(f) => f.name.clone(),
                Item::Class(c) => c.name.clone(),
            })
            .collect();
        assert_eq!(
            names,
            vec!["main", "combat.Unit", "combat.strike", "combat.clamp"]
        );
        let result = Interpreter::new().unwrap().interpret(&program).unwrap();
        assert_eq!(format!("{:?}", result), "Int(99)");
    }

    #[test]
    fn test_private_names_do_not_collide_between_modules() {
        let main = "import combat: strike, Unit

fn clamp(value: int) -> int:
    return value + 1000

fn main() -> int:
    return clamp(strike(new Unit(1, 2), 3))
";
        let program = graph(&[("main.pain", main), ("combat.pain", COMBAT)])
            .unwrap()
            .link()
            .unwrap();
        assert!(type_check_program(&program).is_ok());
        let result = Interpreter::new().unwrap().interpret(&program).unwrap();
        assert_eq!(format!("{:?}", result), "Int(1005)");
    }

    #[test]
    fn test_nested_module_path_and_transitive_import() {
        let main = "import ai.target: pick

fn main() -> int:
    return pick(7)
";
        let target = "import combat: strike, Unit

pub fn pick(seed: int) -> int:
    return strike(new Unit(10, 5), seed)
";
        let g = graph(&[
            ("main.pain", main),
            ("ai/target.pain", target),
            ("combat.pain", COMBAT),
        ])
        .unwrap();
        assert_eq!(
            g.modules()
                .iter()
                .map(|m| m.name.as_str())
                .collect::<Vec<_>>(),
            vec!["", "ai.target", "combat"]
        );
        let program = g.link().unwrap();
        let result = Interpreter::new().unwrap().interpret(&program).unwrap();
        assert_eq!(format!("{:?}", result), "Int(12)");
    }

    #[test]
    fn test_private_item_cannot_be_imported() {
        let e = error(&[
            (
                "main.pain",
                "import combat: clamp\n\nfn main() -> int:\n    return clamp(1, 2, 3)\n",
            ),
            ("combat.pain", COMBAT),
        ]);
        assert_eq!(e.line, 1);
        assert_eq!(
            e.message,
            "`clamp` in module `combat` is not `pub` and cannot be imported"
        );
    }

    #[test]
    fn test_unknown_name_lists_exports() {
        let e = error(&[
            (
                "main.pain",
                "import combat: heal\n\nfn main() -> int:\n    return 0\n",
            ),
            ("combat.pain", COMBAT),
        ]);
        assert_eq!(
            e.message,
            "module `combat` has no `heal`; it exports: Unit, strike"
        );
    }

    #[test]
    fn test_missing_module_names_expected_file() {
        let e = error(&[(
            "main.pain",
            "import ai.path: find\n\nfn main() -> int:\n    return 0\n",
        )]);
        assert_eq!(e.line, 1);
        assert!(e
            .message
            .starts_with("module `ai.path` not found: expected file"));
        assert!(e.message.contains("path.pain"));
    }

    #[test]
    fn test_import_cycle_is_reported_with_the_chain() {
        let e = error(&[
            (
                "main.pain",
                "import a: fa\n\nfn main() -> int:\n    return fa()\n",
            ),
            (
                "a.pain",
                "import b: fb\n\npub fn fa() -> int:\n    return fb()\n",
            ),
            (
                "b.pain",
                "import a: fa\n\npub fn fb() -> int:\n    return fa()\n",
            ),
        ]);
        assert_eq!(e.message, "import cycle: a -> b -> a");
    }

    #[test]
    fn test_name_clashes_are_errors() {
        let e = error(&[
            ("main.pain", "import combat: strike\n\nfn strike() -> int:\n    return 0\n\nfn main() -> int:\n    return 0\n"),
            ("combat.pain", COMBAT),
        ]);
        assert_eq!(
            e.message,
            "imported name `strike` is also defined in this file"
        );
        let e = error(&[
            (
                "main.pain",
                "import combat: strike\nimport other: strike\n\nfn main() -> int:\n    return 0\n",
            ),
            ("combat.pain", COMBAT),
            ("other.pain", "pub fn strike() -> int:\n    return 1\n"),
        ]);
        assert_eq!(e.line, 2);
        assert_eq!(
            e.message,
            "`strike` is imported twice (from `combat` and `other`)"
        );
    }

    #[test]
    fn test_module_cannot_reach_into_the_entry_file() {
        let e = error(&[
            ("main.pain", "import util: twice\n\nfn secret() -> int:\n    return 7\n\nfn main() -> int:\n    return twice()\n"),
            ("util.pain", "pub fn twice() -> int:\n    return secret() * 2\n"),
        ]);
        assert_eq!(
            e.message,
            "`secret` is used in module `util` but is neither defined nor imported there"
        );
    }

    #[test]
    fn test_using_a_name_without_importing_it_is_an_error() {
        let main = "import combat: Unit

fn main() -> int:
    return strike(new Unit(1, 2), 3)
";
        let e = error(&[("main.pain", main), ("combat.pain", COMBAT)]);
        assert_eq!(
            e.message,
            "`strike` is used in this file but not imported; add `import combat: strike`"
        );
        let main = main.replace("strike(new Unit(1, 2), 3)", "clamp(1, 2, 3)");
        let e = error(&[("main.pain", &main), ("combat.pain", COMBAT)]);
        assert_eq!(
            e.message,
            "`clamp` is used in this file but is private to module `combat`"
        );
    }

    #[test]
    fn test_import_syntax_errors() {
        assert!(parse("import combat\n\nfn main() -> int:\n    return 0\n")
            .unwrap_err()
            .contains("write `import combat: name`"));
        assert!(
            parse("fn main() -> int:\n    return 0\n\nimport combat: strike\n")
                .unwrap_err()
                .contains("Imports must come before functions and classes")
        );
        assert!(
            parse("class A:\n    let x: int\n\n    pub fn get() -> int:\n        return x\n")
                .unwrap_err()
                .contains("`pub` is not allowed on fields and methods")
        );
    }
}
