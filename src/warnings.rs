// Warning system for unused variables, dead code, etc.

use crate::ast::*;
use crate::span::{Position, Span};
use crate::type_checker::TypeContext;
use std::collections::{HashMap, HashSet};

// Helper to get span from expression (placeholder for now)
fn get_expr_span(_expr: &Expr) -> Option<Span> {
    // TODO: Add span to Expr enum
    Some(Span::single(Position::start()))
}

#[derive(Debug, Clone, PartialEq)]
pub enum Warning {
    UnusedVariable { name: String, span: Span },
    UnusedFunction { name: String, span: Span },
    DeadCode { span: Span, reason: String },
    UnreachableCode { span: Span },
}

pub struct WarningCollector {
    warnings: Vec<Warning>,
}

impl WarningCollector {
    pub fn new() -> Self {
        Self {
            warnings: Vec::new(),
        }
    }

    pub fn collect_warnings(program: &Program, ctx: &TypeContext) -> Vec<Warning> {
        let mut collector = Self::new();
        collector.check_program(program, ctx);
        collector.warnings
    }

    fn check_program(&mut self, program: &Program, _ctx: &TypeContext) {
        // Track which functions are called
        let mut called_functions = HashSet::new();
        let mut defined_functions = HashMap::new();

        for item in &program.items {
            match item {
                Item::Function(func) => {
                    defined_functions.insert(func.name.clone(), func.span);
                    self.check_function(func, &mut called_functions);
                }
                Item::Class(class) => {
                    for method in &class.methods {
                        defined_functions.insert(method.name.clone(), method.span);
                        self.check_function(method, &mut called_functions);
                    }
                }
            }
        }

        // Check for unused functions (except main)
        for (name, span) in defined_functions {
            if !called_functions.contains(&name) && name != "main" {
                self.warnings.push(Warning::UnusedFunction { name, span });
            }
        }
    }

    fn check_function(&mut self, func: &Function, called_functions: &mut HashSet<String>) {
        let mut used_vars = HashSet::new();
        let mut defined_vars = HashMap::new();
        let mut has_return = false;

        // Add parameters to defined vars
        for param in &func.params {
            defined_vars.insert(param.name.clone(), param.ty.clone());
        }

        for stmt in &func.body {
            self.check_statement(
                stmt,
                &mut used_vars,
                &mut defined_vars,
                &mut has_return,
                called_functions,
            );
        }

        // Check for unused variables
        for (name, _) in &defined_vars {
            if !used_vars.contains(name) {
                // Find the span where variable was defined
                for stmt in &func.body {
                    if let Statement::Let {
                        name: var_name,
                        init,
                        ..
                    } = stmt
                    {
                        if var_name == name {
                            // Use span from init expression or function span as fallback
                            let span = get_expr_span(init).unwrap_or(func.span);
                            self.warnings.push(Warning::UnusedVariable {
                                name: name.clone(),
                                span,
                            });
                            break;
                        }
                    }
                }
            }
        }
    }

    fn check_statement(
        &mut self,
        stmt: &Statement,
        used_vars: &mut HashSet<String>,
        defined_vars: &mut HashMap<String, crate::ast::Type>,
        has_return: &mut bool,
        called_functions: &mut HashSet<String>,
    ) {
        match stmt {
            Statement::Let { name, init, .. } => {
                self.check_expr(init, used_vars, called_functions);
                defined_vars.insert(name.clone(), crate::ast::Type::Dynamic);
            }
            Statement::Return(expr) => {
                if let Some(expr) = expr {
                    self.check_expr(expr, used_vars, called_functions);
                }
                *has_return = true;
            }
            Statement::Expr(expr) => {
                self.check_expr(expr, used_vars, called_functions);
            }
            Statement::If { cond, then, else_ } => {
                self.check_expr(cond, used_vars, called_functions);
                for s in then {
                    self.check_statement(s, used_vars, defined_vars, has_return, called_functions);
                }
                if let Some(else_body) = else_ {
                    for s in else_body {
                        self.check_statement(
                            s,
                            used_vars,
                            defined_vars,
                            has_return,
                            called_functions,
                        );
                    }
                }
            }
            Statement::For { var, iter, body } => {
                self.check_expr(iter, used_vars, called_functions);
                defined_vars.insert(var.clone(), crate::ast::Type::Dynamic);
                for s in body {
                    self.check_statement(s, used_vars, defined_vars, has_return, called_functions);
                }
            }
            Statement::While { cond, body } => {
                self.check_expr(cond, used_vars, called_functions);
                for s in body {
                    self.check_statement(s, used_vars, defined_vars, has_return, called_functions);
                }
            }
            Statement::Break | Statement::Continue => {}
        }
    }

    fn check_expr(
        &mut self,
        expr: &Expr,
        used_vars: &mut HashSet<String>,
        called_functions: &mut HashSet<String>,
    ) {
        match expr {
            Expr::Ident(name) => {
                used_vars.insert(name.clone());
            }
            Expr::Call { callee, args } => {
                self.check_expr(callee, used_vars, called_functions);
                if let Expr::Ident(func_name) = callee.as_ref() {
                    called_functions.insert(func_name.clone());
                }
                for arg in args {
                    self.check_expr(arg, used_vars, called_functions);
                }
            }
            Expr::Add(l, r)
            | Expr::Sub(l, r)
            | Expr::Mul(l, r)
            | Expr::Div(l, r)
            | Expr::Mod(l, r)
            | Expr::Eq(l, r)
            | Expr::Ne(l, r)
            | Expr::Lt(l, r)
            | Expr::Gt(l, r)
            | Expr::Le(l, r)
            | Expr::Ge(l, r)
            | Expr::And(l, r)
            | Expr::Or(l, r)
            | Expr::Assign(l, r)
            | Expr::AddAssign(l, r)
            | Expr::SubAssign(l, r)
            | Expr::MulAssign(l, r)
            | Expr::DivAssign(l, r) => {
                self.check_expr(l, used_vars, called_functions);
                self.check_expr(r, used_vars, called_functions);
            }
            Expr::Not(e) | Expr::Neg(e) => {
                self.check_expr(e, used_vars, called_functions);
            }
            Expr::Index(container, index) => {
                self.check_expr(container, used_vars, called_functions);
                self.check_expr(index, used_vars, called_functions);
            }
            Expr::Member(obj, _) => {
                self.check_expr(obj, used_vars, called_functions);
            }
            Expr::IsInstance(expr, _) => {
                self.check_expr(expr, used_vars, called_functions);
            }
            Expr::New { args, .. } => {
                for arg in args {
                    self.check_expr(arg, used_vars, called_functions);
                }
            }
            Expr::List(elements) => {
                for elem in elements {
                    self.check_expr(elem, used_vars, called_functions);
                }
            }
            _ => {} // Literals don't use variables
        }
    }
}

impl Default for WarningCollector {
    fn default() -> Self {
        Self::new()
    }
}
