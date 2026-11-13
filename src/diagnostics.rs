// Diagnostics as data: everything `pain check` finds, in one machine-readable report.
//
// Each diagnostic has a stable code, so a tool or an AI agent can react to the kind
// of problem without parsing English text, and - when there is an obvious repair -
// a `fix` object describing it. Codes never change meaning; new problems get new codes.
//
//   P001  syntax error
//   M001  imported module not found            M006  import cycle
//   M002  module has no such name              M007  name used but not imported
//   M003  imported name is not `pub`           M008  name is private to another module
//   M004  imported name also defined here      M009  name neither defined nor imported
//   M005  name imported twice                  M010  file cannot be read
//   T001  undefined variable                   T003  invalid operation
//   T002  type mismatch                        T004  invalid call or untypable expression
//   T005  unknown class
//   W001  unused variable                      W003  dead code
//   W002  unused function                      W004  unreachable code

use crate::ast::{Function, Item, Program, Statement, Type};
use crate::error::find_similar_names;
use crate::modules::{ModuleError, ModuleGraph};
use crate::query::ProgramIndex;
use crate::type_checker::{type_check_program_with_context, TypeContext, TypeError};
use crate::warnings::{Warning, WarningCollector};
use serde_json::{json, Value};
use std::path::Path;

#[derive(Debug, Clone, PartialEq)]
pub struct Diagnostic {
    pub code: &'static str,
    /// "error" or "warning"
    pub severity: &'static str,
    pub file: String,
    /// None when the compiler does not know the position
    pub line: Option<usize>,
    pub column: Option<usize>,
    pub message: String,
    pub fix: Option<Value>,
}

impl Diagnostic {
    pub fn to_json(&self) -> Value {
        let mut out = json!({
            "code": self.code,
            "severity": self.severity,
            "file": self.file,
            "line": self.line,
            "column": self.column,
            "message": self.message,
        });
        if let Some(fix) = &self.fix {
            out["fix"] = fix.clone();
        }
        out
    }
}

/// Result of checking a program
#[derive(Debug, Clone, PartialEq)]
pub struct Report {
    pub diagnostics: Vec<Diagnostic>,
}

impl Report {
    /// No errors (warnings do not count)
    pub fn ok(&self) -> bool {
        !self.diagnostics.iter().any(|d| d.severity == "error")
    }

    pub fn to_json(&self) -> Value {
        json!({
            "ok": self.ok(),
            "errors": self.diagnostics.iter().filter(|d| d.severity == "error").count(),
            "warnings": self.diagnostics.iter().filter(|d| d.severity == "warning").count(),
            "diagnostics": self.diagnostics.iter().map(Diagnostic::to_json).collect::<Vec<_>>(),
        })
    }
}

fn display(path: &Path) -> String {
    path.display().to_string().replace('\\', "/")
}

/// `... at 12:5` at the end of a parser message
fn position_in_message(message: &str) -> (Option<usize>, Option<usize>) {
    let Some(at) = message.rfind(" at ") else {
        return (None, None);
    };
    let tail: String = message[at + 4..]
        .chars()
        .take_while(|c| c.is_ascii_digit() || *c == ':')
        .collect();
    let mut parts = tail.split(':');
    match (
        parts.next().and_then(|p| p.parse().ok()),
        parts.next().and_then(|p| p.parse().ok()),
    ) {
        (Some(line), Some(column)) => (Some(line), Some(column)),
        _ => (None, None),
    }
}

fn type_name(ty: &Type) -> String {
    match ty {
        Type::Int => "int".to_string(),
        Type::Str => "str".to_string(),
        Type::Float32 => "float32".to_string(),
        Type::Float64 => "float64".to_string(),
        Type::Bool => "bool".to_string(),
        Type::Dynamic => "dynamic".to_string(),
        Type::List(inner) => format!("list[{}]", type_name(inner)),
        Type::Array(inner) => format!("array[{}]", type_name(inner)),
        Type::Map(key, value) => format!("map[{}, {}]", type_name(key), type_name(value)),
        Type::Tensor(inner, _) => format!("Tensor[{}]", type_name(inner)),
        Type::Named(name) => name.clone(),
    }
}

fn module_diagnostic(err: &ModuleError) -> Diagnostic {
    let (line, column) = if err.code == "P001" {
        position_in_message(&err.message)
    } else {
        ((err.line > 0).then_some(err.line), None)
    };
    Diagnostic {
        code: err.code,
        severity: "error",
        file: display(&err.file),
        line,
        column,
        message: err.message.clone(),
        fix: err.fix.clone(),
    }
}

/// Names a typo in this file could have meant: parameters and local variables.
/// The type checker does not keep the scope an error happened in, so this is every
/// name declared anywhere in the file - a superset of the right scope.
fn declared_names(program: &Program) -> Vec<String> {
    fn block(stmts: &[Statement], out: &mut Vec<String>) {
        for stmt in stmts {
            match stmt {
                Statement::Let { name, .. } => out.push(name.clone()),
                Statement::If { then, else_, .. } => {
                    block(then, out);
                    if let Some(else_) = else_ {
                        block(else_, out);
                    }
                }
                Statement::For { var, body, .. } => {
                    out.push(var.clone());
                    block(body, out);
                }
                Statement::While { body, .. } => block(body, out),
                _ => {}
            }
        }
    }
    fn function(func: &Function, out: &mut Vec<String>) {
        out.extend(func.params.iter().map(|p| p.name.clone()));
        block(&func.body, out);
    }
    let mut out = Vec::new();
    for item in &program.items {
        match item {
            Item::Function(func) => function(func, &mut out),
            Item::Class(class) => {
                out.extend(class.fields.iter().map(|f| f.name.clone()));
                class.methods.iter().for_each(|m| function(m, &mut out));
            }
        }
    }
    out.sort();
    out.dedup();
    out
}

fn type_diagnostic(
    err: &TypeError,
    file: String,
    ctx: &TypeContext,
    local_names: &[String],
) -> Diagnostic {
    let (code, message, fix) = match err {
        TypeError::UndefinedVariable { name, .. } => {
            if let Some(class) = name
                .strip_prefix("Class '")
                .and_then(|rest| rest.strip_suffix("' not found"))
            {
                ("T005", format!("unknown class `{}`", class), None)
            } else {
                let mut candidates = ctx.get_variable_names();
                candidates.extend(ctx.get_function_names());
                candidates.extend(local_names.iter().cloned());
                let similar = find_similar_names(name, &candidates, 3);
                let fix = (!similar.is_empty())
                    .then(|| json!({"kind": "choose_name", "candidates": similar}));
                ("T001", format!("undefined variable `{}`", name), fix)
            }
        }
        TypeError::TypeMismatch {
            expected, found, ..
        } => (
            "T002",
            format!(
                "type mismatch: expected `{}`, found `{}`",
                type_name(expected),
                type_name(found)
            ),
            Some(json!({
                "kind": "match_type",
                "expected": type_name(expected),
                "found": type_name(found),
            })),
        ),
        TypeError::InvalidOperation {
            op, left, right, ..
        } => (
            "T003",
            match right {
                Some(right) => format!(
                    "invalid operation `{}` on `{}` and `{}`",
                    op,
                    type_name(left),
                    type_name(right)
                ),
                None => format!("invalid operation `{}` on `{}`", op, type_name(left)),
            },
            None,
        ),
        TypeError::CannotInferType { message, .. } => ("T004", message.clone(), None),
    };
    Diagnostic {
        code,
        severity: "error",
        file,
        // Expressions carry no position yet: say so instead of pointing at line 1
        line: None,
        column: None,
        message,
        fix,
    }
}

fn warning_diagnostic(warning: &Warning, file: String) -> Diagnostic {
    let (code, message, span) = match warning {
        Warning::UnusedVariable { name, span } => (
            "W001",
            format!("unused variable `{}`", name),
            None::<usize>.or(Some(span.line())),
        ),
        Warning::UnusedFunction { name, span } => (
            "W002",
            format!("unused function `{}`", name),
            Some(span.line()),
        ),
        Warning::DeadCode { span, reason } => {
            ("W003", format!("dead code: {}", reason), Some(span.line()))
        }
        Warning::UnreachableCode { span } => {
            ("W004", "unreachable code".to_string(), Some(span.line()))
        }
    };
    // Variables have no position of their own yet (the span is a placeholder)
    let line = if code == "W001" { None } else { span };
    let fix = (code == "W002").then(|| json!({"kind": "remove_function"}));
    Diagnostic {
        code,
        severity: "warning",
        file,
        line,
        column: None,
        message,
        fix,
    }
}

/// Check the program that starts at `entry`, reading files from disk
pub fn check(entry: &Path) -> Report {
    report(ModuleGraph::load(entry))
}

/// Check a program loaded through a custom reader (tests, editors)
pub fn check_with(entry: &Path, read: &dyn Fn(&Path) -> std::io::Result<String>) -> Report {
    report(ModuleGraph::load_with(entry, read))
}

fn report(loaded: Result<ModuleGraph, ModuleError>) -> Report {
    let graph = match loaded {
        Ok(graph) => graph,
        Err(err) => {
            return Report {
                diagnostics: vec![module_diagnostic(&err)],
            }
        }
    };
    let program = match graph.link() {
        Ok(program) => program,
        Err(err) => {
            return Report {
                diagnostics: vec![module_diagnostic(&err)],
            }
        }
    };

    let mut ctx = TypeContext::new();
    for item in &program.items {
        match item {
            Item::Function(func) => ctx.add_function(func.name.clone(), func.clone()),
            Item::Class(class) => ctx.add_class(class.name.clone(), class.clone()),
        }
    }
    let mut diagnostics = Vec::new();
    if let Err(error) = type_check_program_with_context(&program, &mut ctx) {
        // Expressions carry no file: find the modules whose own code fails to check
        for (index, module) in graph.modules().iter().enumerate() {
            let Ok(isolated) = graph.link_isolated(index) else {
                continue;
            };
            let mut module_ctx = TypeContext::new();
            if let Err(module_error) = type_check_program_with_context(&isolated, &mut module_ctx) {
                diagnostics.push(type_diagnostic(
                    &module_error,
                    display(&module.path),
                    &ctx,
                    &declared_names(&module.program),
                ));
            }
        }
        if diagnostics.is_empty() {
            diagnostics.push(type_diagnostic(
                &error,
                display(&graph.entry().path),
                &ctx,
                &declared_names(&graph.entry().program),
            ));
        }
    }
    let index_of_program = ProgramIndex::from_graph(&graph);
    for (index, warning) in WarningCollector::collect_graph_warnings(&graph) {
        let module = &graph.modules()[index];
        let mut diagnostic = warning_diagnostic(&warning, display(&module.path));
        if let Warning::UnusedFunction { name, .. } = &warning {
            // The whole definition, so it can be deleted without looking for its end
            let qualified = module.qualified(name);
            if let Some(symbol) = index_of_program.symbols().find(|s| s.name == qualified) {
                if symbol.first_line > 0 {
                    diagnostic.fix = Some(json!({
                        "kind": "remove_function",
                        "lines": [symbol.first_line, symbol.last_line],
                    }));
                }
            }
        }
        diagnostics.push(diagnostic);
    }
    Report { diagnostics }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;
    use std::path::PathBuf;

    fn check_files(files: &[(&str, &str)]) -> Report {
        let files: HashMap<PathBuf, String> = files
            .iter()
            .map(|(path, source)| (PathBuf::from("proj").join(path), source.to_string()))
            .collect();
        check_with(&PathBuf::from("proj").join("main.pain"), &|path| {
            files
                .get(path)
                .cloned()
                .ok_or_else(|| std::io::Error::new(std::io::ErrorKind::NotFound, "no such file"))
        })
    }

    const LIB: &str = "pub fn area(w: int, h: int) -> int:\n    return w * h\n\nfn secret() -> int:\n    return 1\n";

    #[test]
    fn test_clean_program_is_ok() {
        let report = check_files(&[
            (
                "main.pain",
                "import lib: area\n\nfn main() -> int:\n    return area(2, 3)\n",
            ),
            ("lib.pain", LIB),
        ]);
        // `secret` is private and unused: a warning, not an error
        assert!(report.ok());
        let json = report.to_json();
        assert_eq!(json["errors"], json!(0));
        assert_eq!(json["warnings"], json!(1));
        assert_eq!(json["diagnostics"][0]["code"], json!("W002"));
        assert_eq!(json["diagnostics"][0]["file"], json!("proj/lib.pain"));
        assert_eq!(json["diagnostics"][0]["line"], json!(4));
        assert_eq!(
            json["diagnostics"][0]["fix"],
            json!({"kind": "remove_function", "lines": [4, 5]})
        );
    }

    #[test]
    fn test_missing_import_carries_the_line_to_add() {
        let report = check_files(&[
            ("main.pain", "fn main() -> int:\n    return area(2, 3)\n"),
            (
                "other.pain",
                "import lib: area\n\npub fn twice() -> int:\n    return area(1, 1)\n",
            ),
            ("lib.pain", LIB),
        ]);
        // lib is not reachable from main: nothing knows about `area`, the type checker decides
        assert_eq!(
            report
                .diagnostics
                .iter()
                .filter(|d| d.code == "M007")
                .count(),
            0
        );

        let report = check_files(&[
            ("main.pain", "import lib: area\n\nfn main() -> int:\n    return area(2, 3) + secret() + helper()\n"),
            ("lib.pain", &format!("{}\npub fn helper() -> int:\n    return 2\n", LIB)),
        ]);
        assert!(!report.ok());
        let d = &report.diagnostics[0];
        // The first unresolved name decides; both kinds carry a repair
        assert!(d.code == "M007" || d.code == "M008");
        if d.code == "M007" {
            assert_eq!(d.fix.as_ref().unwrap()["text"], json!("import lib: helper"));
        } else {
            assert_eq!(d.fix.as_ref().unwrap()["kind"], json!("make_public"));
        }
    }

    #[test]
    fn test_module_errors_have_stable_codes_and_fixes() {
        let private = check_files(&[
            (
                "main.pain",
                "import lib: secret\n\nfn main() -> int:\n    return secret()\n",
            ),
            ("lib.pain", LIB),
        ]);
        let d = &private.diagnostics[0];
        assert_eq!((d.code, d.line), ("M003", Some(1)));
        assert_eq!(
            d.fix,
            Some(json!({"kind": "make_public", "file": "proj/lib.pain", "name": "secret"}))
        );

        let unknown = check_files(&[
            (
                "main.pain",
                "import lib: aera\n\nfn main() -> int:\n    return 0\n",
            ),
            ("lib.pain", LIB),
        ]);
        let d = &unknown.diagnostics[0];
        assert_eq!(d.code, "M002");
        assert_eq!(
            d.fix,
            Some(json!({"kind": "choose_name", "candidates": ["area"]}))
        );

        let missing = check_files(&[(
            "main.pain",
            "import nowhere: thing\n\nfn main() -> int:\n    return 0\n",
        )]);
        let d = &missing.diagnostics[0];
        assert_eq!(d.code, "M001");
        assert_eq!(d.fix.as_ref().unwrap()["kind"], json!("create_file"));
        assert_eq!(d.fix.as_ref().unwrap()["file"], json!("proj/nowhere.pain"));
    }

    #[test]
    fn test_syntax_error_has_position() {
        let report = check_files(&[("main.pain", "fn main() -> int\n    return 0\n")]);
        let d = &report.diagnostics[0];
        assert_eq!(d.code, "P001");
        assert_eq!(d.file, "proj/main.pain");
        assert!(d.line.is_some() && d.column.is_some());
    }

    #[test]
    fn test_type_errors_name_the_file_and_do_not_invent_a_line() {
        let report = check_files(&[
            ("main.pain", "import lib: area\n\nfn main() -> int:\n    return area(2, 3)\n"),
            (
                "lib.pain",
                "pub fn area(w: int, h: int) -> int:\n    var label: int = \"wide\"\n    return w * h\n",
            ),
        ]);
        let d = &report.diagnostics[0];
        assert_eq!(d.code, "T002");
        assert_eq!(d.file, "proj/lib.pain");
        assert_eq!(d.line, None);
        assert_eq!(
            d.fix,
            Some(json!({"kind": "match_type", "expected": "int", "found": "str"}))
        );
    }

    #[test]
    fn test_undefined_variable_suggests_names() {
        let report = check_files(&[(
            "main.pain",
            "fn main() -> int:\n    var total = 1\n    return totl\n",
        )]);
        let d = &report.diagnostics[0];
        assert_eq!(d.code, "T001");
        assert_eq!(
            d.fix,
            Some(json!({"kind": "choose_name", "candidates": ["total"]}))
        );
    }
}
