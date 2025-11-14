// Simple interpreter for Pain AST execution

use crate::ast::*;
use crate::stdlib::is_stdlib_function;
use pain_runtime::{Runtime, Value};
use std::collections::HashMap;

pub struct Interpreter {
    #[allow(dead_code)]
    runtime: Runtime, // Reserved for future memory management
    variables: HashMap<String, Value>,
    functions: HashMap<String, Function>,
}

#[derive(Debug)]
pub enum InterpreterError {
    UndefinedVariable(String),
    UndefinedFunction(String),
    TypeMismatch { expected: String, found: String },
    RuntimeError(String),
}

impl Interpreter {
    pub fn new() -> Result<Self, &'static str> {
        Ok(Self {
            runtime: Runtime::new()?,
            variables: HashMap::new(),
            functions: HashMap::new(),
        })
    }

    pub fn interpret(&mut self, program: &Program) -> Result<Value, InterpreterError> {
        // First pass: collect functions
        for item in &program.items {
            let Item::Function(func) = item;
            self.functions.insert(func.name.clone(), func.clone());
        }

        // Find and execute main function
        let main_func = self.functions.get("main")
            .ok_or_else(|| InterpreterError::RuntimeError("No main function found".to_string()))?
            .clone();
        self.execute_function(&main_func, &[])?;
        Ok(Value::Int(0)) // Default return value
    }

    fn execute_function(&mut self, func: &Function, args: &[Value]) -> Result<Value, InterpreterError> {
        // Create new scope
        let mut scope = self.variables.clone();

        // Bind parameters
        for (param, arg) in func.params.iter().zip(args.iter()) {
            scope.insert(param.name.clone(), arg.clone());
        }

        // Save old scope
        let old_variables = std::mem::replace(&mut self.variables, scope);

        // Execute body
        let mut result = Value::None;
        for stmt in &func.body {
            result = self.execute_statement(stmt)?;
        }

        // Restore old scope
        self.variables = old_variables;

        Ok(result)
    }

    fn execute_statement(&mut self, stmt: &Statement) -> Result<Value, InterpreterError> {
        match stmt {
            Statement::Expr(expr) => {
                self.evaluate_expr(expr)?;
                Ok(Value::None)
            }
            Statement::Let { name, init, .. } => {
                let value = self.evaluate_expr(init)?;
                self.variables.insert(name.clone(), value.clone());
                Ok(value)
            }
            Statement::Return(expr) => {
                if let Some(expr) = expr {
                    self.evaluate_expr(expr)
                } else {
                    Ok(Value::None)
                }
            }
            Statement::If { cond, then, else_ } => {
                let cond_value = self.evaluate_expr(cond)?;
                let cond_bool = match cond_value {
                    Value::Bool(b) => b,
                    _ => return Err(InterpreterError::TypeMismatch {
                        expected: "bool".to_string(),
                        found: format!("{:?}", cond_value),
                    }),
                };

                if cond_bool {
                    for stmt in then {
                        self.execute_statement(stmt)?;
                    }
                } else if let Some(else_body) = else_ {
                    for stmt in else_body {
                        self.execute_statement(stmt)?;
                    }
                }
                Ok(Value::None)
            }
            Statement::For { var, iter, body } => {
                let iter_value = self.evaluate_expr(iter)?;
                // For now, support only integer ranges (simplified)
                // TODO: Support list/array iteration
                match iter_value {
                    Value::Int(end) => {
                        // Simple range: for x in 0..end
                        for i in 0..end {
                            self.variables.insert(var.clone(), Value::Int(i));
                            for stmt in body {
                                match self.execute_statement(stmt)? {
                                    Value::None => {}
                                    _ => break, // Break on return
                                }
                            }
                        }
                    }
                    _ => {
                        return Err(InterpreterError::RuntimeError(
                            format!("For loop iteration not supported for type: {:?}", iter_value)
                        ));
                    }
                }
                Ok(Value::None)
            }
            Statement::While { cond, body } => {
                loop {
                    let cond_value = self.evaluate_expr(cond)?;
                    let cond_bool = match cond_value {
                        Value::Bool(b) => b,
                        _ => break,
                    };
                    if !cond_bool {
                        break;
                    }
                    for stmt in body {
                        self.execute_statement(stmt)?;
                    }
                }
                Ok(Value::None)
            }
            Statement::Break | Statement::Continue => Ok(Value::None),
        }
    }

    fn evaluate_expr(&mut self, expr: &Expr) -> Result<Value, InterpreterError> {
        match expr {
            Expr::Integer(n) => Ok(Value::Int(*n)),
            Expr::Float(f) => Ok(Value::Float(*f)),
            Expr::String(s) => Ok(Value::String(s.clone())),
            Expr::Bool(b) => Ok(Value::Bool(*b)),
            Expr::None => Ok(Value::None),
            Expr::Ident(name) => {
                self.variables.get(name)
                    .cloned()
                    .ok_or_else(|| InterpreterError::UndefinedVariable(name.clone()))
            }
            Expr::Add(left, right) => {
                let l = self.evaluate_expr(left)?;
                let r = self.evaluate_expr(right)?;
                match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
                    (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 + b)),
                    (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a + b as f64)),
                    _ => Err(InterpreterError::TypeMismatch {
                        expected: "number".to_string(),
                        found: "other".to_string(),
                    }),
                }
            }
            Expr::Sub(left, right) => {
                let l = self.evaluate_expr(left)?;
                let r = self.evaluate_expr(right)?;
                match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
                    (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 - b)),
                    (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a - b as f64)),
                    _ => Err(InterpreterError::TypeMismatch {
                        expected: "number".to_string(),
                        found: "other".to_string(),
                    }),
                }
            }
            Expr::Mul(left, right) => {
                let l = self.evaluate_expr(left)?;
                let r = self.evaluate_expr(right)?;
                match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
                    (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 * b)),
                    (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a * b as f64)),
                    _ => Err(InterpreterError::TypeMismatch {
                        expected: "number".to_string(),
                        found: "other".to_string(),
                    }),
                }
            }
            Expr::Div(left, right) => {
                let l = self.evaluate_expr(left)?;
                let r = self.evaluate_expr(right)?;
                match (l, r) {
                    (Value::Int(a), Value::Int(b)) => {
                        if b == 0 {
                            return Err(InterpreterError::RuntimeError("Division by zero".to_string()));
                        }
                        Ok(Value::Int(a / b))
                    }
                    (Value::Float(a), Value::Float(b)) => {
                        if b == 0.0 {
                            return Err(InterpreterError::RuntimeError("Division by zero".to_string()));
                        }
                        Ok(Value::Float(a / b))
                    }
                    _ => Err(InterpreterError::TypeMismatch {
                        expected: "number".to_string(),
                        found: "other".to_string(),
                    }),
                }
            }
            Expr::Mod(left, right) => {
                let l = self.evaluate_expr(left)?;
                let r = self.evaluate_expr(right)?;
                match (l, r) {
                    (Value::Int(a), Value::Int(b)) => {
                        if b == 0 {
                            return Err(InterpreterError::RuntimeError("Modulo by zero".to_string()));
                        }
                        Ok(Value::Int(a % b))
                    }
                    (Value::Float(a), Value::Float(b)) => {
                        if b == 0.0 {
                            return Err(InterpreterError::RuntimeError("Modulo by zero".to_string()));
                        }
                        Ok(Value::Float(a % b))
                    }
                    _ => Err(InterpreterError::TypeMismatch {
                        expected: "number".to_string(),
                        found: "other".to_string(),
                    }),
                }
            }
            Expr::Eq(left, right) => {
                let l = self.evaluate_expr(left)?;
                let r = self.evaluate_expr(right)?;
                Ok(Value::Bool(l == r))
            }
            Expr::Ne(left, right) => {
                let l = self.evaluate_expr(left)?;
                let r = self.evaluate_expr(right)?;
                Ok(Value::Bool(l != r))
            }
            Expr::Lt(left, right) => {
                let l = self.evaluate_expr(left)?;
                let r = self.evaluate_expr(right)?;
                let result = match (l, r) {
                    (Value::Int(a), Value::Int(b)) => a < b,
                    (Value::Float(a), Value::Float(b)) => a < b,
                    (Value::Int(a), Value::Float(b)) => (a as f64) < b,
                    (Value::Float(a), Value::Int(b)) => a < (b as f64),
                    _ => return Err(InterpreterError::TypeMismatch {
                        expected: "number".to_string(),
                        found: "other".to_string(),
                    }),
                };
                Ok(Value::Bool(result))
            }
            Expr::Gt(left, right) => {
                let l = self.evaluate_expr(left)?;
                let r = self.evaluate_expr(right)?;
                let result = match (l, r) {
                    (Value::Int(a), Value::Int(b)) => a > b,
                    (Value::Float(a), Value::Float(b)) => a > b,
                    (Value::Int(a), Value::Float(b)) => (a as f64) > b,
                    (Value::Float(a), Value::Int(b)) => a > (b as f64),
                    _ => return Err(InterpreterError::TypeMismatch {
                        expected: "number".to_string(),
                        found: "other".to_string(),
                    }),
                };
                Ok(Value::Bool(result))
            }
            Expr::Le(left, right) => {
                let l = self.evaluate_expr(left)?;
                let r = self.evaluate_expr(right)?;
                let result = match (l, r) {
                    (Value::Int(a), Value::Int(b)) => a <= b,
                    (Value::Float(a), Value::Float(b)) => a <= b,
                    (Value::Int(a), Value::Float(b)) => (a as f64) <= b,
                    (Value::Float(a), Value::Int(b)) => a <= (b as f64),
                    _ => return Err(InterpreterError::TypeMismatch {
                        expected: "number".to_string(),
                        found: "other".to_string(),
                    }),
                };
                Ok(Value::Bool(result))
            }
            Expr::Ge(left, right) => {
                let l = self.evaluate_expr(left)?;
                let r = self.evaluate_expr(right)?;
                let result = match (l, r) {
                    (Value::Int(a), Value::Int(b)) => a >= b,
                    (Value::Float(a), Value::Float(b)) => a >= b,
                    (Value::Int(a), Value::Float(b)) => (a as f64) >= b,
                    (Value::Float(a), Value::Int(b)) => a >= (b as f64),
                    _ => return Err(InterpreterError::TypeMismatch {
                        expected: "number".to_string(),
                        found: "other".to_string(),
                    }),
                };
                Ok(Value::Bool(result))
            }
            Expr::And(left, right) => {
                let l = self.evaluate_expr(left)?;
                let r = self.evaluate_expr(right)?;
                let l_bool = match l {
                    Value::Bool(b) => b,
                    _ => return Err(InterpreterError::TypeMismatch {
                        expected: "bool".to_string(),
                        found: "other".to_string(),
                    }),
                };
                let r_bool = match r {
                    Value::Bool(b) => b,
                    _ => return Err(InterpreterError::TypeMismatch {
                        expected: "bool".to_string(),
                        found: "other".to_string(),
                    }),
                };
                Ok(Value::Bool(l_bool && r_bool))
            }
            Expr::Or(left, right) => {
                let l = self.evaluate_expr(left)?;
                let r = self.evaluate_expr(right)?;
                let l_bool = match l {
                    Value::Bool(b) => b,
                    _ => return Err(InterpreterError::TypeMismatch {
                        expected: "bool".to_string(),
                        found: "other".to_string(),
                    }),
                };
                let r_bool = match r {
                    Value::Bool(b) => b,
                    _ => return Err(InterpreterError::TypeMismatch {
                        expected: "bool".to_string(),
                        found: "other".to_string(),
                    }),
                };
                Ok(Value::Bool(l_bool || r_bool))
            }
            Expr::Not(operand) => {
                let op = self.evaluate_expr(operand)?;
                let op_bool = match op {
                    Value::Bool(b) => b,
                    _ => return Err(InterpreterError::TypeMismatch {
                        expected: "bool".to_string(),
                        found: "other".to_string(),
                    }),
                };
                Ok(Value::Bool(!op_bool))
            }
            Expr::Neg(operand) => {
                let op = self.evaluate_expr(operand)?;
                match op {
                    Value::Int(n) => Ok(Value::Int(-n)),
                    Value::Float(f) => Ok(Value::Float(-f)),
                    _ => Err(InterpreterError::TypeMismatch {
                        expected: "number".to_string(),
                        found: "other".to_string(),
                    }),
                }
            }
            Expr::Call { callee, args } => {
                let callee_name = match callee.as_ref() {
                    Expr::Ident(name) => name.clone(),
                    _ => return Err(InterpreterError::RuntimeError("Invalid function call".to_string())),
                };

                // Check for stdlib functions
                if is_stdlib_function(&callee_name) {
                    return self.call_stdlib_function(&callee_name, args);
                } else {
                    let func = self.functions.get(&callee_name)
                        .ok_or_else(|| InterpreterError::UndefinedFunction(callee_name.clone()))?
                        .clone();
                    let evaluated_args: Result<Vec<Value>, InterpreterError> = args.iter()
                        .map(|arg| self.evaluate_expr(arg))
                        .collect();
                    let args = evaluated_args?;
                    self.execute_function(&func, &args)
                }
            }
            _ => Err(InterpreterError::RuntimeError(format!("Unsupported expression: {:?}", expr))),
        }
    }

    fn call_stdlib_function(&mut self, name: &str, args: &[Expr]) -> Result<Value, InterpreterError> {
        // Evaluate arguments
        let arg_values: Result<Vec<Value>, InterpreterError> = args.iter()
            .map(|arg| self.evaluate_expr(arg))
            .collect();
        let arg_values = arg_values?;

        match name {
            "print" => {
                for value in &arg_values {
                    println!("{}", format_value(value));
                }
                Ok(Value::None)
            }
            // Math functions
            "abs" => {
                if arg_values.len() != 1 {
                    return Err(InterpreterError::RuntimeError("abs requires 1 argument".to_string()));
                }
                match &arg_values[0] {
                    Value::Int(n) => Ok(Value::Int(n.abs())),
                    Value::Float(f) => Ok(Value::Float(f.abs())),
                    _ => Err(InterpreterError::TypeMismatch {
                        expected: "number".to_string(),
                        found: "other".to_string(),
                    }),
                }
            }
            "min" => {
                if arg_values.len() != 2 {
                    return Err(InterpreterError::RuntimeError("min requires 2 arguments".to_string()));
                }
                match (&arg_values[0], &arg_values[1]) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(*a.min(b))),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a.min(*b))),
                    _ => Err(InterpreterError::TypeMismatch {
                        expected: "number".to_string(),
                        found: "other".to_string(),
                    }),
                }
            }
            "max" => {
                if arg_values.len() != 2 {
                    return Err(InterpreterError::RuntimeError("max requires 2 arguments".to_string()));
                }
                match (&arg_values[0], &arg_values[1]) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(*a.max(b))),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a.max(*b))),
                    _ => Err(InterpreterError::TypeMismatch {
                        expected: "number".to_string(),
                        found: "other".to_string(),
                    }),
                }
            }
            "sqrt" => {
                if arg_values.len() != 1 {
                    return Err(InterpreterError::RuntimeError("sqrt requires 1 argument".to_string()));
                }
                match &arg_values[0] {
                    Value::Float(f) => Ok(Value::Float(f.sqrt())),
                    Value::Int(n) => Ok(Value::Float((*n as f64).sqrt())),
                    _ => Err(InterpreterError::TypeMismatch {
                        expected: "number".to_string(),
                        found: "other".to_string(),
                    }),
                }
            }
            "pow" => {
                if arg_values.len() != 2 {
                    return Err(InterpreterError::RuntimeError("pow requires 2 arguments".to_string()));
                }
                let base = match &arg_values[0] {
                    Value::Float(f) => *f,
                    Value::Int(n) => *n as f64,
                    _ => return Err(InterpreterError::TypeMismatch {
                        expected: "number".to_string(),
                        found: "other".to_string(),
                    }),
                };
                let exp = match &arg_values[1] {
                    Value::Float(f) => *f,
                    Value::Int(n) => *n as f64,
                    _ => return Err(InterpreterError::TypeMismatch {
                        expected: "number".to_string(),
                        found: "other".to_string(),
                    }),
                };
                Ok(Value::Float(base.powf(exp)))
            }
            "sin" => {
                if arg_values.len() != 1 {
                    return Err(InterpreterError::RuntimeError("sin requires 1 argument".to_string()));
                }
                let x = match &arg_values[0] {
                    Value::Float(f) => *f,
                    Value::Int(n) => *n as f64,
                    _ => return Err(InterpreterError::TypeMismatch {
                        expected: "number".to_string(),
                        found: "other".to_string(),
                    }),
                };
                Ok(Value::Float(x.sin()))
            }
            "cos" => {
                if arg_values.len() != 1 {
                    return Err(InterpreterError::RuntimeError("cos requires 1 argument".to_string()));
                }
                let x = match &arg_values[0] {
                    Value::Float(f) => *f,
                    Value::Int(n) => *n as f64,
                    _ => return Err(InterpreterError::TypeMismatch {
                        expected: "number".to_string(),
                        found: "other".to_string(),
                    }),
                };
                Ok(Value::Float(x.cos()))
            }
            "floor" => {
                if arg_values.len() != 1 {
                    return Err(InterpreterError::RuntimeError("floor requires 1 argument".to_string()));
                }
                match &arg_values[0] {
                    Value::Float(f) => Ok(Value::Float(f.floor())),
                    Value::Int(n) => Ok(Value::Float(*n as f64)),
                    _ => Err(InterpreterError::TypeMismatch {
                        expected: "number".to_string(),
                        found: "other".to_string(),
                    }),
                }
            }
            "ceil" => {
                if arg_values.len() != 1 {
                    return Err(InterpreterError::RuntimeError("ceil requires 1 argument".to_string()));
                }
                match &arg_values[0] {
                    Value::Float(f) => Ok(Value::Float(f.ceil())),
                    Value::Int(n) => Ok(Value::Float(*n as f64)),
                    _ => Err(InterpreterError::TypeMismatch {
                        expected: "number".to_string(),
                        found: "other".to_string(),
                    }),
                }
            }
            // String functions
            "len" => {
                if arg_values.len() != 1 {
                    return Err(InterpreterError::RuntimeError("len requires 1 argument".to_string()));
                }
                match &arg_values[0] {
                    Value::String(s) => Ok(Value::Int(s.len() as i64)),
                    _ => Err(InterpreterError::TypeMismatch {
                        expected: "str".to_string(),
                        found: "other".to_string(),
                    }),
                }
            }
            "concat" => {
                if arg_values.len() != 2 {
                    return Err(InterpreterError::RuntimeError("concat requires 2 arguments".to_string()));
                }
                match (&arg_values[0], &arg_values[1]) {
                    (Value::String(a), Value::String(b)) => Ok(Value::String(format!("{}{}", a, b))),
                    _ => Err(InterpreterError::TypeMismatch {
                        expected: "str".to_string(),
                        found: "other".to_string(),
                    }),
                }
            }
            "substring" => {
                if arg_values.len() != 3 {
                    return Err(InterpreterError::RuntimeError("substring requires 3 arguments".to_string()));
                }
                match (&arg_values[0], &arg_values[1], &arg_values[2]) {
                    (Value::String(s), Value::Int(start), Value::Int(end)) => {
                        let start = *start as usize;
                        let end = *end as usize;
                        if start <= end && end <= s.len() {
                            Ok(Value::String(s[start..end].to_string()))
                        } else {
                            Err(InterpreterError::RuntimeError("Invalid substring indices".to_string()))
                        }
                    }
                    _ => Err(InterpreterError::TypeMismatch {
                        expected: "str, int, int".to_string(),
                        found: "other".to_string(),
                    }),
                }
            }
            "contains" => {
                if arg_values.len() != 2 {
                    return Err(InterpreterError::RuntimeError("contains requires 2 arguments".to_string()));
                }
                match (&arg_values[0], &arg_values[1]) {
                    (Value::String(s), Value::String(substr)) => {
                        Ok(Value::Bool(s.contains(substr)))
                    }
                    _ => Err(InterpreterError::TypeMismatch {
                        expected: "str, str".to_string(),
                        found: "other".to_string(),
                    }),
                }
            }
            "starts_with" => {
                if arg_values.len() != 2 {
                    return Err(InterpreterError::RuntimeError("starts_with requires 2 arguments".to_string()));
                }
                match (&arg_values[0], &arg_values[1]) {
                    (Value::String(s), Value::String(prefix)) => {
                        Ok(Value::Bool(s.starts_with(prefix)))
                    }
                    _ => Err(InterpreterError::TypeMismatch {
                        expected: "str, str".to_string(),
                        found: "other".to_string(),
                    }),
                }
            }
            "ends_with" => {
                if arg_values.len() != 2 {
                    return Err(InterpreterError::RuntimeError("ends_with requires 2 arguments".to_string()));
                }
                match (&arg_values[0], &arg_values[1]) {
                    (Value::String(s), Value::String(suffix)) => {
                        Ok(Value::Bool(s.ends_with(suffix)))
                    }
                    _ => Err(InterpreterError::TypeMismatch {
                        expected: "str, str".to_string(),
                        found: "other".to_string(),
                    }),
                }
            }
            "trim" => {
                if arg_values.len() != 1 {
                    return Err(InterpreterError::RuntimeError("trim requires 1 argument".to_string()));
                }
                match &arg_values[0] {
                    Value::String(s) => Ok(Value::String(s.trim().to_string())),
                    _ => Err(InterpreterError::TypeMismatch {
                        expected: "str".to_string(),
                        found: "other".to_string(),
                    }),
                }
            }
            "to_int" => {
                if arg_values.len() != 1 {
                    return Err(InterpreterError::RuntimeError("to_int requires 1 argument".to_string()));
                }
                match &arg_values[0] {
                    Value::String(s) => {
                        s.parse::<i64>()
                            .map(Value::Int)
                            .map_err(|_| InterpreterError::RuntimeError(format!("Cannot convert '{}' to int", s)))
                    }
                    _ => Err(InterpreterError::TypeMismatch {
                        expected: "str".to_string(),
                        found: "other".to_string(),
                    }),
                }
            }
            "to_float" => {
                if arg_values.len() != 1 {
                    return Err(InterpreterError::RuntimeError("to_float requires 1 argument".to_string()));
                }
                match &arg_values[0] {
                    Value::String(s) => {
                        s.parse::<f64>()
                            .map(Value::Float)
                            .map_err(|_| InterpreterError::RuntimeError(format!("Cannot convert '{}' to float", s)))
                    }
                    _ => Err(InterpreterError::TypeMismatch {
                        expected: "str".to_string(),
                        found: "other".to_string(),
                    }),
                }
            }
            "to_string" => {
                if arg_values.len() != 1 {
                    return Err(InterpreterError::RuntimeError("to_string requires 1 argument".to_string()));
                }
                Ok(Value::String(format_value(&arg_values[0])))
            }
            _ => Err(InterpreterError::UndefinedFunction(name.to_string())),
        }
    }
}

fn format_value(value: &Value) -> String {
    match value {
        Value::Int(n) => n.to_string(),
        Value::Float(f) => f.to_string(),
        Value::Bool(b) => b.to_string(),
        Value::String(s) => s.clone(),
        Value::None => "None".to_string(),
    }
}

