// Code formatter for Pain language

use crate::ast::*;

pub struct Formatter {
    output: String,
    indent_level: usize,
    indent_size: usize,
}

impl Formatter {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            indent_level: 0,
            indent_size: 4, // 4 spaces per indent level
        }
    }

    pub fn format(program: &Program) -> String {
        let mut formatter = Self::new();
        formatter.format_program(program);
        formatter.output
    }

    fn format_program(&mut self, program: &Program) {
        for (i, item) in program.items.iter().enumerate() {
            if i > 0 {
                self.newline();
            }
            self.format_item(item);
        }
    }

    fn format_item(&mut self, item: &Item) {
        match item {
            Item::Function(func) => self.format_function(func),
            Item::Class(_class) => {
                // TODO: Format classes
            }
        }
    }

    fn format_function(&mut self, func: &Function) {
        // Format doc comment if present
        if let Some(ref doc) = func.doc {
            self.write_indent();
            self.write("\"\"\"");
            self.write(doc);
            self.write("\"\"\"");
            self.newline();
        }

        // Format attributes
        for attr in &func.attrs {
            self.write_indent();
            self.write(&format!("@{}\n", attr.name));
        }

        // Function signature
        self.write_indent();
        self.write("fn ");
        self.write(&func.name);
        self.write("(");

        // Parameters
        for (i, param) in func.params.iter().enumerate() {
            if i > 0 {
                self.write(", ");
            }
            self.write(&param.name);
            self.write(": ");
            self.format_type(&param.ty);
        }

        self.write(")");

        // Return type
        if let Some(ref return_type) = func.return_type {
            self.write(" -> ");
            self.format_type(return_type);
        }

        self.write(":");
        self.newline();

        // Function body
        self.indent();
        for stmt in &func.body {
            self.format_statement(stmt);
        }
        self.dedent();
    }

    fn format_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Expr(expr) => {
                self.write_indent();
                self.format_expr(expr);
                self.newline();
            }
            Statement::Let {
                mutable,
                name,
                ty,
                init,
            } => {
                self.write_indent();
                if *mutable {
                    self.write("var ");
                } else {
                    self.write("let ");
                }
                self.write(name);
                if let Some(ref type_ann) = ty {
                    self.write(": ");
                    self.format_type(type_ann);
                }
                self.write(" = ");
                self.format_expr(init);
                self.newline();
            }
            Statement::Return(expr) => {
                self.write_indent();
                self.write("return");
                if let Some(ref expr) = expr {
                    self.write(" ");
                    self.format_expr(expr);
                }
                self.newline();
            }
            Statement::If { cond, then, else_ } => {
                self.write_indent();
                self.write("if ");
                self.format_expr(cond);
                self.write(":");
                self.newline();
                self.indent();
                for stmt in then {
                    self.format_statement(stmt);
                }
                self.dedent();
                if let Some(ref else_body) = else_ {
                    self.write_indent();
                    self.write("else:");
                    self.newline();
                    self.indent();
                    for stmt in else_body {
                        self.format_statement(stmt);
                    }
                    self.dedent();
                }
            }
            Statement::For { var, iter, body } => {
                self.write_indent();
                self.write("for ");
                self.write(var);
                self.write(" in ");
                self.format_expr(iter);
                self.write(":");
                self.newline();
                self.indent();
                for stmt in body {
                    self.format_statement(stmt);
                }
                self.dedent();
            }
            Statement::While { cond, body } => {
                self.write_indent();
                self.write("while ");
                self.format_expr(cond);
                self.write(":");
                self.newline();
                self.indent();
                for stmt in body {
                    self.format_statement(stmt);
                }
                self.dedent();
            }
            Statement::Break => {
                self.write_indent();
                self.write("break");
                self.newline();
            }
            Statement::Continue => {
                self.write_indent();
                self.write("continue");
                self.newline();
            }
        }
    }

    fn format_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Integer(n) => {
                self.write(&n.to_string());
            }
            Expr::Float(f) => {
                self.write(&f.to_string());
            }
            Expr::String(s) => {
                self.write("\"");
                self.write(&escape_string(s));
                self.write("\"");
            }
            Expr::FString(s) => {
                self.write("f\"");
                self.write(&escape_string(s));
                self.write("\"");
            }
            Expr::Bool(b) => {
                self.write(if *b { "true" } else { "false" });
            }
            Expr::None => {
                self.write("None");
            }
            Expr::List(elements) => {
                self.write("[");
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.format_expr(elem);
                }
                self.write("]");
            }
            Expr::Ident(name) => {
                self.write(name);
            }
            Expr::Add(lhs, rhs) => {
                self.format_binary_op(lhs, "+", rhs);
            }
            Expr::Sub(lhs, rhs) => {
                self.format_binary_op(lhs, "-", rhs);
            }
            Expr::Mul(lhs, rhs) => {
                self.format_binary_op(lhs, "*", rhs);
            }
            Expr::Div(lhs, rhs) => {
                self.format_binary_op(lhs, "/", rhs);
            }
            Expr::Mod(lhs, rhs) => {
                self.format_binary_op(lhs, "%", rhs);
            }
            Expr::Eq(lhs, rhs) => {
                self.format_binary_op(lhs, "==", rhs);
            }
            Expr::Ne(lhs, rhs) => {
                self.format_binary_op(lhs, "!=", rhs);
            }
            Expr::Lt(lhs, rhs) => {
                self.format_binary_op(lhs, "<", rhs);
            }
            Expr::Gt(lhs, rhs) => {
                self.format_binary_op(lhs, ">", rhs);
            }
            Expr::Le(lhs, rhs) => {
                self.format_binary_op(lhs, "<=", rhs);
            }
            Expr::Ge(lhs, rhs) => {
                self.format_binary_op(lhs, ">=", rhs);
            }
            Expr::And(lhs, rhs) => {
                self.format_binary_op(lhs, "&&", rhs);
            }
            Expr::Or(lhs, rhs) => {
                self.format_binary_op(lhs, "||", rhs);
            }
            Expr::Not(operand) => {
                self.write("!");
                self.format_expr(operand);
            }
            Expr::Neg(operand) => {
                self.write("-");
                self.format_expr(operand);
            }
            Expr::Assign(lhs, rhs) => {
                self.format_expr(lhs);
                self.write(" = ");
                self.format_expr(rhs);
            }
            Expr::AddAssign(lhs, rhs) => {
                self.format_expr(lhs);
                self.write(" += ");
                self.format_expr(rhs);
            }
            Expr::SubAssign(lhs, rhs) => {
                self.format_expr(lhs);
                self.write(" -= ");
                self.format_expr(rhs);
            }
            Expr::MulAssign(lhs, rhs) => {
                self.format_expr(lhs);
                self.write(" *= ");
                self.format_expr(rhs);
            }
            Expr::DivAssign(lhs, rhs) => {
                self.format_expr(lhs);
                self.write(" /= ");
                self.format_expr(rhs);
            }
            Expr::Call { callee, args } => {
                self.format_expr(callee);
                self.write("(");
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.format_expr(arg);
                }
                self.write(")");
            }
            Expr::Index(array, index) => {
                self.format_expr(array);
                self.write("[");
                self.format_expr(index);
                self.write("]");
            }
            Expr::Member(object, field) => {
                self.format_expr(object);
                self.write(".");
                self.write(field);
            }
            Expr::IsInstance(expr, ty) => {
                self.write("isinstance(");
                self.format_expr(expr);
                self.write(", ");
                self.format_type(ty);
                self.write(")");
            }
            Expr::New { class_name, args } => {
                self.write("new ");
                self.write(class_name);
                self.write("(");
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.format_expr(arg);
                }
                self.write(")");
            }
        }
    }

    fn format_binary_op(&mut self, lhs: &Expr, op: &str, rhs: &Expr) {
        let op_prec = self.get_op_precedence(op);
        let needs_parens_lhs = self.needs_parens_for_op(lhs, op_prec, false);
        let needs_parens_rhs = self.needs_parens_for_op(rhs, op_prec, true);

        if needs_parens_lhs {
            self.write("(");
        }
        self.format_expr(lhs);
        if needs_parens_lhs {
            self.write(")");
        }

        self.write(" ");
        self.write(op);
        self.write(" ");

        if needs_parens_rhs {
            self.write("(");
        }
        self.format_expr(rhs);
        if needs_parens_rhs {
            self.write(")");
        }
    }

    // Get operator precedence (higher number = higher precedence)
    fn get_op_precedence(&self, op: &str) -> u8 {
        match op {
            "*" | "/" | "%" => 5,
            "+" | "-" => 4,
            "<" | ">" | "<=" | ">=" => 3,
            "==" | "!=" => 2,
            "&&" => 1,
            "||" => 0,
            _ => 0,
        }
    }

    // Check if expression needs parentheses based on operator precedence
    fn needs_parens_for_op(&self, expr: &Expr, parent_prec: u8, is_right: bool) -> bool {
        match expr {
            Expr::Add(_, _) | Expr::Sub(_, _) => {
                let prec = self.get_op_precedence("+");
                // Left-associative: need parens if same or lower precedence
                prec <= parent_prec
            }
            Expr::Mul(_, _) | Expr::Div(_, _) | Expr::Mod(_, _) => {
                let prec = self.get_op_precedence("*");
                // For right operand with higher or equal precedence, add parens for clarity
                // For left operand, only if lower precedence
                if is_right {
                    prec >= parent_prec
                } else {
                    prec < parent_prec
                }
            }
            Expr::Eq(_, _) | Expr::Ne(_, _) => {
                let prec = self.get_op_precedence("==");
                // Non-associative: always need parens if same or lower precedence
                prec <= parent_prec
            }
            Expr::Lt(_, _) | Expr::Gt(_, _) | Expr::Le(_, _) | Expr::Ge(_, _) => {
                let prec = self.get_op_precedence("<");
                // Non-associative: always need parens if same or lower precedence
                prec <= parent_prec
            }
            Expr::And(_, _) => {
                let prec = self.get_op_precedence("&&");
                // Left-associative: need parens if same or lower precedence
                prec <= parent_prec
            }
            Expr::Or(_, _) => {
                let prec = self.get_op_precedence("||");
                // Left-associative: need parens if same or lower precedence
                prec <= parent_prec
            }
            _ => false,
        }
    }

    fn format_type(&mut self, ty: &Type) {
        match ty {
            Type::Int => self.write("int"),
            Type::Str => self.write("str"),
            Type::Float32 => self.write("float32"),
            Type::Float64 => self.write("float64"),
            Type::Bool => self.write("bool"),
            Type::Dynamic => self.write("dynamic"),
            Type::List(element) => {
                self.write("list[");
                self.format_type(element);
                self.write("]");
            }
            Type::Array(element) => {
                self.write("array[");
                self.format_type(element);
                self.write("]");
            }
            Type::Map(key, value) => {
                self.write("map[");
                self.format_type(key);
                self.write(", ");
                self.format_type(value);
                self.write("]");
            }
            Type::Tensor(element, dims) => {
                self.write("Tensor[");
                self.format_type(element);
                if !dims.is_empty() {
                    self.write(", (");
                    for (i, dim) in dims.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        self.format_expr(dim);
                    }
                    self.write(")");
                }
                self.write("]");
            }
            Type::Named(name) => self.write(name),
        }
    }

    fn write(&mut self, s: &str) {
        self.output.push_str(s);
    }

    fn newline(&mut self) {
        self.output.push('\n');
    }

    fn write_indent(&mut self) {
        for _ in 0..(self.indent_level * self.indent_size) {
            self.output.push(' ');
        }
    }

    fn indent(&mut self) {
        self.indent_level += 1;
    }

    fn dedent(&mut self) {
        if self.indent_level > 0 {
            self.indent_level -= 1;
        }
    }
}

fn escape_string(s: &str) -> String {
    s.chars()
        .flat_map(|c| match c {
            '"' => vec!['\\', '"'],
            '\\' => vec!['\\', '\\'],
            '\n' => vec!['\\', 'n'],
            '\r' => vec!['\\', 'r'],
            '\t' => vec!['\\', 't'],
            _ => vec![c],
        })
        .collect()
}

impl Default for Formatter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;
    use crate::span::{Position, Span};

    #[test]
    fn test_format_simple_function() {
        let source = "fn test() -> int:
    return 42";
        let program = parse(source).unwrap();
        let formatted = Formatter::format(&program);
        assert!(formatted.contains("fn test"));
        assert!(formatted.contains("return 42"));
    }

    #[test]
    fn test_format_function_with_params() {
        let source = "fn add(a: int, b: int) -> int:
    return a + b";
        let program = parse(source).unwrap();
        let formatted = Formatter::format(&program);
        assert!(formatted.contains("fn add"));
        assert!(formatted.contains("a: int"));
        assert!(formatted.contains("b: int"));
    }

    #[test]
    fn test_format_if_statement() {
        let source = "fn test(x: int) -> int:
    if x > 0:
        return 1
    else:
        return 0";
        let program = parse(source).unwrap();
        let formatted = Formatter::format(&program);
        assert!(formatted.contains("if x > 0"));
        assert!(formatted.contains("else"));
    }

    #[test]
    fn test_format_while_loop() {
        let source = "fn test():
    let i = 0
    while i < 10:
        i = i + 1";
        let program = parse(source).unwrap();
        let formatted = Formatter::format(&program);
        assert!(formatted.contains("while i < 10"));
    }

    #[test]
    fn test_format_let_statement() {
        let source = "fn test():
    let x = 10
    let y: int = 20";
        let program = parse(source).unwrap();
        let formatted = Formatter::format(&program);
        assert!(formatted.contains("let x = 10"));
        assert!(formatted.contains("let y: int = 20"));
    }

    #[test]
    fn test_format_list_literal() {
        let source = "fn test() -> list[int]:
    return [1, 2, 3]";
        let program = parse(source).unwrap();
        let formatted = Formatter::format(&program);
        assert!(formatted.contains("[1, 2, 3]"));
    }

    #[test]
    fn test_format_doc_comment() {
        let program = Program {
            items: vec![Item::Function(Function {
                doc: Some("This is a test function.".to_string()),
                attrs: vec![],
                name: "test".to_string(),
                params: vec![],
                return_type: None,
                body: vec![],
                span: Span::single(Position::start()),
            })],
            span: Span::single(Position::start()),
        };
        let formatted = Formatter::format(&program);
        assert!(formatted.contains("\"\"\""));
        assert!(formatted.contains("This is a test function."));
    }

    #[test]
    fn test_format_attributes() {
        let program = Program {
            items: vec![Item::Function(Function {
                doc: None,
                attrs: vec![Attribute {
                    name: "inline".to_string(),
                    args: vec![],
                }],
                name: "test".to_string(),
                params: vec![],
                return_type: None,
                body: vec![],
                span: Span::single(Position::start()),
            })],
            span: Span::single(Position::start()),
        };
        let formatted = Formatter::format(&program);
        assert!(formatted.contains("@inline"));
    }

    #[test]
    fn test_format_binary_operations() {
        let source = "fn test(a: int, b: int) -> int:
    return a + b * c";
        let program = parse(source).unwrap();
        let formatted = Formatter::format(&program);
        assert!(formatted.contains("a + (b * c)"));
    }
}
