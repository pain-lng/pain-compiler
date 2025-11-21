// Interpreter for Pain AST

use crate::ast::{Class, Expr, Function, Item, Program, Statement, Type};
use crate::stdlib::is_stdlib_function;
use pain_runtime::{ClassInstance, Value};
use regex::Regex;
use serde_json;
use std::collections::HashMap;
use std::fs;
use std::path::Path;

/// Control flow result
enum ControlFlow {
    Next,
    Return(Value),
    Break,
    Continue,
}

/// Execution context for interpreter
#[derive(Default)]
pub struct Environment {
    variables: HashMap<String, Value>,
    functions: HashMap<String, Function>,
    classes: HashMap<String, Class>,
}

impl Environment {
    pub fn new() -> Self {
        Self::default()
    }

    fn set_var(&mut self, name: String, value: Value) {
        self.variables.insert(name, value);
    }

    fn get_var(&self, name: &str) -> Option<&Value> {
        self.variables.get(name)
    }

    fn set_var_mut(&mut self, name: &str, value: Value) -> Result<(), &'static str> {
        if self.variables.contains_key(name) {
            self.variables.insert(name.to_string(), value);
            Ok(())
        } else {
            Err("Variable not found or not mutable")
        }
    }

    pub fn add_function(&mut self, func: Function) {
        self.functions.insert(func.name.clone(), func);
    }

    fn get_function(&self, name: &str) -> Option<&Function> {
        self.functions.get(name)
    }

    pub fn get_all_functions(&self) -> &HashMap<String, Function> {
        &self.functions
    }

    pub fn add_class(&mut self, class: Class) {
        self.classes.insert(class.name.clone(), class);
    }

    fn get_class(&self, name: &str) -> Option<&Class> {
        self.classes.get(name)
    }

    pub fn get_all_classes(&self) -> &HashMap<String, Class> {
        &self.classes
    }

    pub fn get_all_variables(&self) -> &HashMap<String, Value> {
        &self.variables
    }
}

/// Interpreter for executing Pain programs
pub struct Interpreter;

impl Interpreter {
    /// Create a new interpreter instance
    pub fn new() -> Result<Self, &'static str> {
        Ok(Self)
    }

    /// Interpret a Pain program
    pub fn interpret(&mut self, program: &Program) -> Result<Value, &'static str> {
        let mut env = Environment::new();

        // Register all functions and classes from program
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    env.add_function(func.clone());
                }
                Item::Class(class) => {
                    env.add_class(class.clone());
                }
            }
        }

        // Find and execute main function
        let main_func = env
            .get_function("main")
            .ok_or("No main function found")?
            .clone();

        self.eval_function(&main_func, &[], &mut env)
    }

    /// Evaluate a function call
    pub fn eval_function(
        &mut self,
        func: &Function,
        args: &[Value],
        parent_env: &mut Environment,
    ) -> Result<Value, &'static str> {
        // Create new scope for function
        let mut func_env = Environment::new();

        // Copy functions and classes from parent scope
        func_env.functions = parent_env.functions.clone();
        func_env.classes = parent_env.classes.clone();

        // Bind parameters
        if args.len() != func.params.len() {
            return Err("Argument count mismatch");
        }

        for (param, arg_value) in func.params.iter().zip(args.iter()) {
            func_env.set_var(param.name.clone(), arg_value.clone());
        }

        // Execute function body
        let result = Value::None;
        for stmt in &func.body {
            match self.eval_stmt(stmt, &mut func_env)? {
                ControlFlow::Return(val) => return Ok(val),
                ControlFlow::Break => return Err("Break outside loop"),
                ControlFlow::Continue => return Err("Continue outside loop"),
                ControlFlow::Next => {}
            }
        }

        Ok(result)
    }

    /// Evaluate a statement
    fn eval_stmt(
        &mut self,
        stmt: &Statement,
        env: &mut Environment,
    ) -> Result<ControlFlow, &'static str> {
        match stmt {
            Statement::Expr(expr) => {
                self.eval_expr(expr, env)?;
                Ok(ControlFlow::Next)
            }
            Statement::Let {
                name,
                init,
                mutable: _,
                ty: _,
            } => {
                let value = self.eval_expr(init, env)?;
                env.set_var(name.clone(), value);
                Ok(ControlFlow::Next)
            }
            Statement::Return(expr) => {
                let value = if let Some(expr) = expr {
                    self.eval_expr(expr, env)?
                } else {
                    Value::None
                };
                Ok(ControlFlow::Return(value))
            }
            Statement::If { cond, then, else_ } => {
                let cond_val = self.eval_expr(cond, env)?;
                let cond_bool = match cond_val {
                    Value::Bool(b) => b,
                    _ => return Err("If condition must be boolean"),
                };

                if cond_bool {
                    for stmt in then {
                        match self.eval_stmt(stmt, env)? {
                            ControlFlow::Return(v) => return Ok(ControlFlow::Return(v)),
                            ControlFlow::Break => return Ok(ControlFlow::Break),
                            ControlFlow::Continue => return Ok(ControlFlow::Continue),
                            ControlFlow::Next => {}
                        }
                    }
                } else if let Some(else_stmts) = else_ {
                    for stmt in else_stmts {
                        match self.eval_stmt(stmt, env)? {
                            ControlFlow::Return(v) => return Ok(ControlFlow::Return(v)),
                            ControlFlow::Break => return Ok(ControlFlow::Break),
                            ControlFlow::Continue => return Ok(ControlFlow::Continue),
                            ControlFlow::Next => {}
                        }
                    }
                }
                Ok(ControlFlow::Next)
            }
            Statement::While { cond, body } => {
                loop {
                    let cond_val = self.eval_expr(cond, env)?;
                    let cond_bool = match cond_val {
                        Value::Bool(b) => b,
                        _ => return Err("While condition must be boolean"),
                    };

                    if !cond_bool {
                        break;
                    }

                    for stmt in body {
                        match self.eval_stmt(stmt, env)? {
                            ControlFlow::Return(v) => return Ok(ControlFlow::Return(v)),
                            ControlFlow::Break => break,
                            ControlFlow::Continue => continue,
                            ControlFlow::Next => {}
                        }
                    }
                }
                Ok(ControlFlow::Next)
            }
            Statement::For { var, iter, body } => {
                // Evaluate iterator expression
                let iter_val = self.eval_expr(iter, env)?;

                // Handle different iterator types
                match iter_val {
                    // Range: for x in range(start, end) - treat as Call to range function
                    Value::Int(end) => {
                        // Simple case: for x in N (iterate from 0 to N-1)
                        for i in 0..end {
                            env.set_var(var.clone(), Value::Int(i));
                            for stmt in body {
                                match self.eval_stmt(stmt, env)? {
                                    ControlFlow::Return(v) => return Ok(ControlFlow::Return(v)),
                                    ControlFlow::Break => break,
                                    ControlFlow::Continue => continue,
                                    ControlFlow::Next => {}
                                }
                            }
                        }
                    }
                    // List/Array iteration
                    Value::List(list) | Value::Array(list) => {
                        for item in list {
                            env.set_var(var.clone(), item);
                            for stmt in body {
                                match self.eval_stmt(stmt, env)? {
                                    ControlFlow::Return(v) => return Ok(ControlFlow::Return(v)),
                                    ControlFlow::Break => break,
                                    ControlFlow::Continue => continue,
                                    ControlFlow::Next => {}
                                }
                            }
                        }
                    }
                    // Range function call
                    _ => {
                        // Check if iterator is a range() call
                        if let Expr::Call { callee, args } = iter {
                            if let Expr::Ident(func_name) = callee.as_ref() {
                                if func_name == "range" {
                                    // Evaluate range arguments
                                    let arg_values: Vec<Value> = args
                                        .iter()
                                        .map(|arg| self.eval_expr(arg, env))
                                        .collect::<Result<Vec<_>, _>>()?;

                                    let range_values: Vec<i64> = match arg_values.len() {
                                        1 => {
                                            // range(end) - from 0 to end-1
                                            if let Value::Int(end) = arg_values[0] {
                                                (0..end).collect()
                                            } else {
                                                return Err("range() requires integer arguments");
                                            }
                                        }
                                        2 => {
                                            // range(start, end) - from start to end-1
                                            if let (Value::Int(start), Value::Int(end)) =
                                                (&arg_values[0], &arg_values[1])
                                            {
                                                (*start..*end).collect()
                                            } else {
                                                return Err("range() requires integer arguments");
                                            }
                                        }
                                        _ => return Err("range() takes 1 or 2 arguments"),
                                    };

                                    // Iterate over range
                                    for i in range_values {
                                        env.set_var(var.clone(), Value::Int(i));
                                        for stmt in body {
                                            match self.eval_stmt(stmt, env)? {
                                                ControlFlow::Return(v) => {
                                                    return Ok(ControlFlow::Return(v))
                                                }
                                                ControlFlow::Break => break,
                                                ControlFlow::Continue => continue,
                                                ControlFlow::Next => {}
                                            }
                                        }
                                    }
                                    return Ok(ControlFlow::Next);
                                }
                            }
                        }
                        return Err("For loop iterator must be an integer, range, or list");
                    }
                }
                Ok(ControlFlow::Next)
            }
            Statement::Break => Ok(ControlFlow::Break),
            Statement::Continue => Ok(ControlFlow::Continue),
        }
    }

    /// Evaluate an expression
    fn eval_expr(&mut self, expr: &Expr, env: &mut Environment) -> Result<Value, &'static str> {
        match expr {
            // Literals
            Expr::Integer(n) => Ok(Value::Int(*n)),
            Expr::Float(f) => Ok(Value::Float(*f)),
            Expr::String(s) => Ok(Value::String(s.clone())),
            Expr::Bool(b) => Ok(Value::Bool(*b)),
            Expr::None => Ok(Value::None),
            Expr::List(elements) => {
                // Evaluate all elements and create a list
                let mut list = Vec::new();
                for elem in elements {
                    list.push(self.eval_expr(elem, env)?);
                }
                Ok(Value::List(list))
            }

            // Variables
            Expr::Ident(name) => env.get_var(name).cloned().ok_or("Undefined variable"),

            // Binary operations
            Expr::Add(lhs, rhs) => {
                let lhs_val = self.eval_expr(lhs, env)?;
                let rhs_val = self.eval_expr(rhs, env)?;
                match (lhs_val, rhs_val) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
                    (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 + b)),
                    (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a + b as f64)),
                    (Value::String(a), Value::String(b)) => {
                        Ok(Value::String(format!("{}{}", a, b)))
                    }
                    _ => Err("Invalid operands for addition"),
                }
            }
            Expr::Sub(lhs, rhs) => {
                let lhs_val = self.eval_expr(lhs, env)?;
                let rhs_val = self.eval_expr(rhs, env)?;
                match (lhs_val, rhs_val) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
                    (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 - b)),
                    (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a - b as f64)),
                    _ => Err("Invalid operands for subtraction"),
                }
            }
            Expr::Mul(lhs, rhs) => {
                let lhs_val = self.eval_expr(lhs, env)?;
                let rhs_val = self.eval_expr(rhs, env)?;
                match (lhs_val, rhs_val) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
                    (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 * b)),
                    (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a * b as f64)),
                    _ => Err("Invalid operands for multiplication"),
                }
            }
            Expr::Div(lhs, rhs) => {
                let lhs_val = self.eval_expr(lhs, env)?;
                let rhs_val = self.eval_expr(rhs, env)?;
                match (lhs_val, rhs_val) {
                    (Value::Int(a), Value::Int(b)) => {
                        if b == 0 {
                            return Err("Division by zero");
                        }
                        Ok(Value::Int(a / b))
                    }
                    (Value::Float(a), Value::Float(b)) => {
                        if b == 0.0 {
                            return Err("Division by zero");
                        }
                        Ok(Value::Float(a / b))
                    }
                    (Value::Int(a), Value::Float(b)) => {
                        if b == 0.0 {
                            return Err("Division by zero");
                        }
                        Ok(Value::Float(a as f64 / b))
                    }
                    (Value::Float(a), Value::Int(b)) => {
                        if b == 0 {
                            return Err("Division by zero");
                        }
                        Ok(Value::Float(a / b as f64))
                    }
                    _ => Err("Invalid operands for division"),
                }
            }
            Expr::Mod(lhs, rhs) => {
                let lhs_val = self.eval_expr(lhs, env)?;
                let rhs_val = self.eval_expr(rhs, env)?;
                match (lhs_val, rhs_val) {
                    (Value::Int(a), Value::Int(b)) => {
                        if b == 0 {
                            return Err("Modulo by zero");
                        }
                        Ok(Value::Int(a % b))
                    }
                    _ => Err("Invalid operands for modulo"),
                }
            }

            // Comparisons
            Expr::Eq(lhs, rhs) => {
                let lhs_val = self.eval_expr(lhs, env)?;
                let rhs_val = self.eval_expr(rhs, env)?;
                // Lists/Arrays are compared by reference equality (same Vec pointer)
                // For value equality, we'd need deep comparison
                Ok(Value::Bool(lhs_val == rhs_val))
            }
            Expr::Ne(lhs, rhs) => {
                let lhs_val = self.eval_expr(lhs, env)?;
                let rhs_val = self.eval_expr(rhs, env)?;
                Ok(Value::Bool(lhs_val != rhs_val))
            }
            Expr::Lt(lhs, rhs) => {
                let lhs_val = self.eval_expr(lhs, env)?;
                let rhs_val = self.eval_expr(rhs, env)?;
                let result = match (lhs_val, rhs_val) {
                    (Value::Int(a), Value::Int(b)) => a < b,
                    (Value::Float(a), Value::Float(b)) => a < b,
                    (Value::Int(a), Value::Float(b)) => (a as f64) < b,
                    (Value::Float(a), Value::Int(b)) => a < (b as f64),
                    _ => return Err("Invalid operands for comparison"),
                };
                Ok(Value::Bool(result))
            }
            Expr::Gt(lhs, rhs) => {
                let lhs_val = self.eval_expr(lhs, env)?;
                let rhs_val = self.eval_expr(rhs, env)?;
                let result = match (lhs_val, rhs_val) {
                    (Value::Int(a), Value::Int(b)) => a > b,
                    (Value::Float(a), Value::Float(b)) => a > b,
                    (Value::Int(a), Value::Float(b)) => (a as f64) > b,
                    (Value::Float(a), Value::Int(b)) => a > (b as f64),
                    _ => return Err("Invalid operands for comparison"),
                };
                Ok(Value::Bool(result))
            }
            Expr::Le(lhs, rhs) => {
                let lhs_val = self.eval_expr(lhs, env)?;
                let rhs_val = self.eval_expr(rhs, env)?;
                let result = match (lhs_val, rhs_val) {
                    (Value::Int(a), Value::Int(b)) => a <= b,
                    (Value::Float(a), Value::Float(b)) => a <= b,
                    (Value::Int(a), Value::Float(b)) => (a as f64) <= b,
                    (Value::Float(a), Value::Int(b)) => a <= (b as f64),
                    _ => return Err("Invalid operands for comparison"),
                };
                Ok(Value::Bool(result))
            }
            Expr::Ge(lhs, rhs) => {
                let lhs_val = self.eval_expr(lhs, env)?;
                let rhs_val = self.eval_expr(rhs, env)?;
                let result = match (lhs_val, rhs_val) {
                    (Value::Int(a), Value::Int(b)) => a >= b,
                    (Value::Float(a), Value::Float(b)) => a >= b,
                    (Value::Int(a), Value::Float(b)) => (a as f64) >= b,
                    (Value::Float(a), Value::Int(b)) => a >= (b as f64),
                    _ => return Err("Invalid operands for comparison"),
                };
                Ok(Value::Bool(result))
            }

            // Logical operations
            Expr::And(lhs, rhs) => {
                let lhs_val = self.eval_expr(lhs, env)?;
                let lhs_bool = match lhs_val {
                    Value::Bool(b) => b,
                    _ => return Err("Invalid operand for logical AND"),
                };
                if !lhs_bool {
                    return Ok(Value::Bool(false));
                }
                let rhs_val = self.eval_expr(rhs, env)?;
                let rhs_bool = match rhs_val {
                    Value::Bool(b) => b,
                    _ => return Err("Invalid operand for logical AND"),
                };
                Ok(Value::Bool(rhs_bool))
            }
            Expr::Or(lhs, rhs) => {
                let lhs_val = self.eval_expr(lhs, env)?;
                let lhs_bool = match lhs_val {
                    Value::Bool(b) => b,
                    _ => return Err("Invalid operand for logical OR"),
                };
                if lhs_bool {
                    return Ok(Value::Bool(true));
                }
                let rhs_val = self.eval_expr(rhs, env)?;
                let rhs_bool = match rhs_val {
                    Value::Bool(b) => b,
                    _ => return Err("Invalid operand for logical OR"),
                };
                Ok(Value::Bool(rhs_bool))
            }

            // Unary operations
            Expr::Not(operand) => {
                let val = self.eval_expr(operand, env)?;
                let bool_val = match val {
                    Value::Bool(b) => b,
                    _ => return Err("Invalid operand for NOT"),
                };
                Ok(Value::Bool(!bool_val))
            }
            Expr::Neg(operand) => {
                let val = self.eval_expr(operand, env)?;
                match val {
                    Value::Int(n) => Ok(Value::Int(-n)),
                    Value::Float(f) => Ok(Value::Float(-f)),
                    _ => Err("Invalid operand for negation"),
                }
            }

            // Assignment
            Expr::Assign(lhs, rhs) => {
                let rhs_val = self.eval_expr(rhs, env)?;
                if let Expr::Ident(name) = lhs.as_ref() {
                    env.set_var_mut(name, rhs_val.clone())?;
                    Ok(rhs_val)
                } else {
                    Err("Invalid left-hand side for assignment")
                }
            }
            Expr::AddAssign(lhs, rhs) => {
                let lhs_val = if let Expr::Ident(name) = lhs.as_ref() {
                    env.get_var(name).cloned().ok_or("Variable not found")?
                } else {
                    return Err("Invalid left-hand side for assignment");
                };
                let rhs_val = self.eval_expr(rhs, env)?;
                let result = match (lhs_val, rhs_val) {
                    (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
                    (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
                    (Value::Int(a), Value::Float(b)) => Value::Float(a as f64 + b),
                    (Value::Float(a), Value::Int(b)) => Value::Float(a + b as f64),
                    _ => return Err("Invalid operands for += operation"),
                };
                if let Expr::Ident(name) = lhs.as_ref() {
                    env.set_var_mut(name, result.clone())?;
                }
                Ok(result)
            }
            Expr::SubAssign(lhs, rhs) => {
                let lhs_val = if let Expr::Ident(name) = lhs.as_ref() {
                    env.get_var(name).cloned().ok_or("Variable not found")?
                } else {
                    return Err("Invalid left-hand side for assignment");
                };
                let rhs_val = self.eval_expr(rhs, env)?;
                let result = match (lhs_val, rhs_val) {
                    (Value::Int(a), Value::Int(b)) => Value::Int(a - b),
                    (Value::Float(a), Value::Float(b)) => Value::Float(a - b),
                    (Value::Int(a), Value::Float(b)) => Value::Float(a as f64 - b),
                    (Value::Float(a), Value::Int(b)) => Value::Float(a - b as f64),
                    _ => return Err("Invalid operands for -= operation"),
                };
                if let Expr::Ident(name) = lhs.as_ref() {
                    env.set_var_mut(name, result.clone())?;
                }
                Ok(result)
            }
            Expr::MulAssign(lhs, rhs) => {
                let lhs_val = if let Expr::Ident(name) = lhs.as_ref() {
                    env.get_var(name).cloned().ok_or("Variable not found")?
                } else {
                    return Err("Invalid left-hand side for assignment");
                };
                let rhs_val = self.eval_expr(rhs, env)?;
                let result = match (lhs_val, rhs_val) {
                    (Value::Int(a), Value::Int(b)) => Value::Int(a * b),
                    (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
                    (Value::Int(a), Value::Float(b)) => Value::Float(a as f64 * b),
                    (Value::Float(a), Value::Int(b)) => Value::Float(a * b as f64),
                    _ => return Err("Invalid operands for *= operation"),
                };
                if let Expr::Ident(name) = lhs.as_ref() {
                    env.set_var_mut(name, result.clone())?;
                }
                Ok(result)
            }
            Expr::DivAssign(lhs, rhs) => {
                let lhs_val = if let Expr::Ident(name) = lhs.as_ref() {
                    env.get_var(name).cloned().ok_or("Variable not found")?
                } else {
                    return Err("Invalid left-hand side for assignment");
                };
                let rhs_val = self.eval_expr(rhs, env)?;
                let result = match (lhs_val, rhs_val) {
                    (Value::Int(a), Value::Int(b)) => {
                        if b == 0 {
                            return Err("Division by zero");
                        }
                        Value::Int(a / b)
                    }
                    (Value::Float(a), Value::Float(b)) => {
                        if b == 0.0 {
                            return Err("Division by zero");
                        }
                        Value::Float(a / b)
                    }
                    (Value::Int(a), Value::Float(b)) => {
                        if b == 0.0 {
                            return Err("Division by zero");
                        }
                        Value::Float(a as f64 / b)
                    }
                    (Value::Float(a), Value::Int(b)) => {
                        if b == 0 {
                            return Err("Division by zero");
                        }
                        Value::Float(a / b as f64)
                    }
                    _ => return Err("Invalid operands for /= operation"),
                };
                if let Expr::Ident(name) = lhs.as_ref() {
                    env.set_var_mut(name, result.clone())?;
                }
                Ok(result)
            }

            // Function call
            Expr::Call { callee, args } => {
                let callee_expr = callee.as_ref();
                if let Expr::Ident(func_name) = callee_expr {
                    // Evaluate arguments
                    let arg_values: Vec<Value> = args
                        .iter()
                        .map(|arg| self.eval_expr(arg, env))
                        .collect::<Result<Vec<_>, _>>()?;

                    // Check if it's a stdlib function
                    if is_stdlib_function(func_name) {
                        self.call_stdlib_function(func_name, &arg_values)
                    } else if let Some(func) = env.get_function(func_name) {
                        let func_clone = func.clone();
                        self.eval_function(&func_clone, &arg_values, env)
                    } else {
                        Err("Function not found")
                    }
                } else {
                    Err("Invalid function call")
                }
            }

            // F-string interpolation
            Expr::FString(template) => {
                // Simple f-string interpolation: {var} syntax
                let mut result = String::new();
                let mut chars = template.chars().peekable();

                while let Some(ch) = chars.next() {
                    if ch == '{' {
                        // Parse variable name
                        let mut var_name = String::new();
                        while let Some(&next_ch) = chars.peek() {
                            if next_ch == '}' {
                                chars.next(); // consume '}'
                                break;
                            }
                            var_name.push(chars.next().unwrap());
                        }

                        // Get variable value from environment
                        if let Some(value) = env.get_var(&var_name) {
                            match value {
                                Value::Int(n) => result.push_str(&n.to_string()),
                                Value::Float(f) => result.push_str(&f.to_string()),
                                Value::Bool(b) => result.push_str(&b.to_string()),
                                Value::String(s) => result.push_str(s),
                                Value::None => result.push_str("None"),
                                Value::List(_) | Value::Array(_) => result.push_str("[...]"),
                                Value::Object(_) => result.push_str("[Object]"),
                            }
                        } else {
                            return Err("Undefined variable in f-string");
                        }
                    } else if ch == '\\' {
                        // Handle escape sequences
                        if let Some(next_ch) = chars.next() {
                            match next_ch {
                                'n' => result.push('\n'),
                                't' => result.push('\t'),
                                'r' => result.push('\r'),
                                '\\' => result.push('\\'),
                                '{' => result.push('{'),
                                '}' => result.push('}'),
                                _ => {
                                    result.push('\\');
                                    result.push(next_ch);
                                }
                            }
                        } else {
                            result.push('\\');
                        }
                    } else {
                        result.push(ch);
                    }
                }

                Ok(Value::String(result))
            }

            // Indexing: container[index]
            Expr::Index(container, index) => {
                let container_val = self.eval_expr(container, env)?;
                let index_val = self.eval_expr(index, env)?;

                match (container_val, index_val) {
                    // String indexing
                    (Value::String(s), Value::Int(i)) => {
                        let chars: Vec<char> = s.chars().collect();
                        if i < 0 || i as usize >= chars.len() {
                            return Err("String index out of bounds");
                        }
                        Ok(Value::String(chars[i as usize].to_string()))
                    }
                    // List/Array indexing
                    (Value::List(list), Value::Int(i)) | (Value::Array(list), Value::Int(i)) => {
                        if i < 0 || i as usize >= list.len() {
                            return Err("List index out of bounds");
                        }
                        Ok(list[i as usize].clone())
                    }
                    _ => Err("Indexing not supported for this type"),
                }
            }

            // Member access: object.field
            Expr::Member(obj, field_name) => {
                let obj_val = self.eval_expr(obj, env)?;
                match obj_val {
                    Value::Object(instance) => instance
                        .get_field(field_name)
                        .cloned()
                        .ok_or("Field not found"),
                    _ => Err("Member access only supported for objects"),
                }
            }

            // Type checking: expr is Type
            Expr::IsInstance(expr, ty) => {
                let expr_val = self.eval_expr(expr, env)?;
                let is_instance = match (expr_val, ty) {
                    (Value::Int(_), Type::Int) => true,
                    (Value::Float(_), Type::Float64) => true,
                    (Value::Bool(_), Type::Bool) => true,
                    (Value::String(_), Type::Str) => true,
                    (Value::None, Type::Dynamic) => true,
                    (Value::List(_), Type::List(_)) => true,
                    (Value::Array(_), Type::Array(_)) => true,
                    (Value::Object(instance), Type::Named(class_name)) => {
                        instance.class_name == *class_name
                    }
                    _ => false,
                };
                Ok(Value::Bool(is_instance))
            }

            // Object creation: new ClassName(args...)
            Expr::New { class_name, args } => {
                // Get class definition and clone it
                let class = env.get_class(class_name).ok_or("Class not found")?.clone();

                // Evaluate arguments
                let arg_values: Vec<Value> = args
                    .iter()
                    .map(|arg| self.eval_expr(arg, env))
                    .collect::<Result<Vec<_>, _>>()?;

                // Create instance
                let mut instance = ClassInstance::new(class_name.clone());

                // Initialize fields from constructor arguments
                // For now, assume fields are initialized in order
                // TODO: Support named arguments and proper constructors
                for (i, field) in class.fields.iter().enumerate() {
                    if i < arg_values.len() {
                        instance.set_field(field.name.clone(), arg_values[i].clone());
                    } else {
                        // Initialize with default value based on type
                        let default = match field.ty {
                            Type::Int => Value::Int(0),
                            Type::Float64 => Value::Float(0.0),
                            Type::Bool => Value::Bool(false),
                            Type::Str => Value::String(String::new()),
                            _ => Value::None,
                        };
                        instance.set_field(field.name.clone(), default);
                    }
                }

                Ok(Value::Object(instance))
            }
        }
    }

    /// Call a standard library function
    fn call_stdlib_function(&mut self, name: &str, args: &[Value]) -> Result<Value, &'static str> {
        match name {
            "print" => {
                if args.is_empty() {
                    return Err("print requires at least one argument");
                }
                // Print all arguments
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        print!(" ");
                    }
                    match arg {
                        Value::Int(n) => print!("{}", n),
                        Value::Float(f) => print!("{}", f),
                        Value::Bool(b) => print!("{}", b),
                        Value::String(s) => print!("{}", s),
                        Value::None => print!("None"),
                        Value::Object(_) => print!("[Object]"),
                        Value::List(list) => {
                            print!("[");
                            for (i, item) in list.iter().enumerate() {
                                if i > 0 {
                                    print!(", ");
                                }
                                match item {
                                    Value::Int(n) => print!("{}", n),
                                    Value::Float(f) => print!("{}", f),
                                    Value::Bool(b) => print!("{}", b),
                                    Value::String(s) => print!("\"{}\"", s),
                                    Value::None => print!("None"),
                                    _ => print!("{:?}", item),
                                }
                            }
                            print!("]");
                        }
                        Value::Array(arr) => {
                            print!("[");
                            for (i, item) in arr.iter().enumerate() {
                                if i > 0 {
                                    print!(", ");
                                }
                                match item {
                                    Value::Int(n) => print!("{}", n),
                                    Value::Float(f) => print!("{}", f),
                                    Value::Bool(b) => print!("{}", b),
                                    Value::String(s) => print!("\"{}\"", s),
                                    Value::None => print!("None"),
                                    _ => print!("{:?}", item),
                                }
                            }
                            print!("]");
                        }
                    }
                }
                println!();
                Ok(Value::None)
            }
            "abs" => {
                if args.len() != 1 {
                    return Err("abs requires one argument");
                }
                match &args[0] {
                    Value::Int(n) => Ok(Value::Int(n.abs())),
                    Value::Float(f) => Ok(Value::Float(f.abs())),
                    _ => Err("abs requires numeric argument"),
                }
            }
            "min" => {
                if args.len() != 2 {
                    return Err("min requires two arguments");
                }
                match (&args[0], &args[1]) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(*a.min(b))),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a.min(*b))),
                    _ => Err("min requires numeric arguments"),
                }
            }
            "max" => {
                if args.len() != 2 {
                    return Err("max requires two arguments");
                }
                match (&args[0], &args[1]) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(*a.max(b))),
                    (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a.max(*b))),
                    _ => Err("max requires numeric arguments"),
                }
            }
            "sqrt" => {
                if args.len() != 1 {
                    return Err("sqrt requires one argument");
                }
                match &args[0] {
                    Value::Float(f) => Ok(Value::Float(f.sqrt())),
                    Value::Int(n) => Ok(Value::Float((*n as f64).sqrt())),
                    _ => Err("sqrt requires numeric argument"),
                }
            }
            "pow" => {
                if args.len() != 2 {
                    return Err("pow requires two arguments");
                }
                let base = match &args[0] {
                    Value::Float(f) => *f,
                    Value::Int(n) => *n as f64,
                    _ => return Err("pow requires numeric arguments"),
                };
                let exp = match &args[1] {
                    Value::Float(f) => *f,
                    Value::Int(n) => *n as f64,
                    _ => return Err("pow requires numeric arguments"),
                };
                Ok(Value::Float(base.powf(exp)))
            }
            "sin" => {
                if args.len() != 1 {
                    return Err("sin requires one argument");
                }
                let x = match &args[0] {
                    Value::Float(f) => *f,
                    Value::Int(n) => *n as f64,
                    _ => return Err("sin requires numeric argument"),
                };
                Ok(Value::Float(x.sin()))
            }
            "cos" => {
                if args.len() != 1 {
                    return Err("cos requires one argument");
                }
                let x = match &args[0] {
                    Value::Float(f) => *f,
                    Value::Int(n) => *n as f64,
                    _ => return Err("cos requires numeric argument"),
                };
                Ok(Value::Float(x.cos()))
            }
            "floor" => {
                if args.len() != 1 {
                    return Err("floor requires one argument");
                }
                let x = match &args[0] {
                    Value::Float(f) => *f,
                    Value::Int(n) => *n as f64,
                    _ => return Err("floor requires numeric argument"),
                };
                Ok(Value::Float(x.floor()))
            }
            "ceil" => {
                if args.len() != 1 {
                    return Err("ceil requires one argument");
                }
                let x = match &args[0] {
                    Value::Float(f) => *f,
                    Value::Int(n) => *n as f64,
                    _ => return Err("ceil requires numeric argument"),
                };
                Ok(Value::Float(x.ceil()))
            }
            "len" => {
                if args.len() != 1 {
                    return Err("len requires one argument");
                }
                match &args[0] {
                    Value::String(s) => Ok(Value::Int(s.len() as i64)),
                    Value::List(list) => Ok(Value::Int(list.len() as i64)),
                    Value::Array(arr) => Ok(Value::Int(arr.len() as i64)),
                    _ => Err("len requires string, list, or array argument"),
                }
            }
            // File I/O functions
            "read_file" => {
                if args.len() != 1 {
                    return Err("read_file requires one argument");
                }
                if let Value::String(path) = &args[0] {
                    match fs::read_to_string(path) {
                        Ok(content) => Ok(Value::String(content)),
                        Err(_) => Err("Failed to read file"),
                    }
                } else {
                    Err("read_file requires string argument")
                }
            }
            "write_file" => {
                if args.len() != 2 {
                    return Err("write_file requires two arguments");
                }
                if let (Value::String(path), Value::String(content)) = (&args[0], &args[1]) {
                    match fs::write(path, content) {
                        Ok(_) => Ok(Value::None),
                        Err(_) => Err("Failed to write file"),
                    }
                } else {
                    Err("write_file requires two string arguments")
                }
            }
            "read_lines" => {
                if args.len() != 1 {
                    return Err("read_lines requires one argument");
                }
                if let Value::String(path) = &args[0] {
                    match fs::read_to_string(path) {
                        Ok(content) => {
                            let lines: Vec<Value> = content
                                .lines()
                                .map(|line| Value::String(line.to_string()))
                                .collect();
                            Ok(Value::List(lines))
                        }
                        Err(_) => Err("Failed to read file"),
                    }
                } else {
                    Err("read_lines requires string argument")
                }
            }
            // Path manipulation functions
            "path_join" => {
                if args.len() != 1 {
                    return Err("path_join requires one argument");
                }
                if let Value::List(parts) = &args[0] {
                    let path_strings: Result<Vec<&str>, _> = parts
                        .iter()
                        .map(|v| {
                            if let Value::String(s) = v {
                                Ok(s.as_str())
                            } else {
                                Err("path_join requires list of strings")
                            }
                        })
                        .collect();
                    match path_strings {
                        Ok(strings) => {
                            let path = Path::new(&strings[0]);
                            let joined = strings[1..]
                                .iter()
                                .fold(path.to_path_buf(), |acc, p| acc.join(p));
                            Ok(Value::String(joined.to_string_lossy().to_string()))
                        }
                        Err(_) => Err("path_join requires list of strings"),
                    }
                } else {
                    Err("path_join requires list argument")
                }
            }
            "path_dir" => {
                if args.len() != 1 {
                    return Err("path_dir requires one argument");
                }
                if let Value::String(path) = &args[0] {
                    let p = Path::new(path);
                    if let Some(parent) = p.parent() {
                        Ok(Value::String(parent.to_string_lossy().to_string()))
                    } else {
                        Ok(Value::String("".to_string()))
                    }
                } else {
                    Err("path_dir requires string argument")
                }
            }
            "path_base" => {
                if args.len() != 1 {
                    return Err("path_base requires one argument");
                }
                if let Value::String(path) = &args[0] {
                    let p = Path::new(path);
                    Ok(Value::String(
                        p.file_name()
                            .map(|n| n.to_string_lossy().to_string())
                            .unwrap_or_default(),
                    ))
                } else {
                    Err("path_base requires string argument")
                }
            }
            "path_ext" => {
                if args.len() != 1 {
                    return Err("path_ext requires one argument");
                }
                if let Value::String(path) = &args[0] {
                    let p = Path::new(path);
                    Ok(Value::String(
                        p.extension()
                            .map(|e| format!(".{}", e.to_string_lossy()))
                            .unwrap_or_default(),
                    ))
                } else {
                    Err("path_ext requires string argument")
                }
            }
            // Date/time functions
            "now" => {
                if !args.is_empty() {
                    return Err("now requires no arguments");
                }
                use std::time::{SystemTime, UNIX_EPOCH};
                match SystemTime::now().duration_since(UNIX_EPOCH) {
                    Ok(duration) => Ok(Value::Float(duration.as_secs_f64())),
                    Err(_) => Err("Failed to get current time"),
                }
            }
            "time_format" => {
                if args.len() != 2 {
                    return Err("time_format requires two arguments");
                }
                if let (Value::Float(timestamp), Value::String(format_str)) = (&args[0], &args[1]) {
                    use std::time::{Duration, UNIX_EPOCH};
                    let secs = timestamp.trunc() as u64;
                    let nanos = (timestamp.fract() * 1_000_000_000.0) as u32;
                    let _datetime = UNIX_EPOCH + Duration::new(secs, nanos);
                    // Simple format implementation - supports %Y, %m, %d, %H, %M, %S
                    // For full implementation, would use chrono crate
                    let formatted = format_str
                        .replace("%Y", "2024") // Placeholder - would need proper date parsing
                        .replace("%m", "01")
                        .replace("%d", "01")
                        .replace("%H", "00")
                        .replace("%M", "00")
                        .replace("%S", "00");
                    Ok(Value::String(formatted))
                } else {
                    Err("time_format requires float and string arguments")
                }
            }
            // Regular expression functions
            "regex_match" => {
                if args.len() != 2 {
                    return Err("regex_match requires two arguments");
                }
                if let (Value::String(pattern), Value::String(text)) = (&args[0], &args[1]) {
                    match Regex::new(pattern) {
                        Ok(re) => Ok(Value::Bool(re.is_match(text))),
                        Err(_) => Err("Invalid regex pattern"),
                    }
                } else {
                    Err("regex_match requires two string arguments")
                }
            }
            "regex_find" => {
                if args.len() != 2 {
                    return Err("regex_find requires two arguments");
                }
                if let (Value::String(pattern), Value::String(text)) = (&args[0], &args[1]) {
                    match Regex::new(pattern) {
                        Ok(re) => {
                            if let Some(mat) = re.find(text) {
                                Ok(Value::String(mat.as_str().to_string()))
                            } else {
                                Ok(Value::String(String::new()))
                            }
                        }
                        Err(_) => Err("Invalid regex pattern"),
                    }
                } else {
                    Err("regex_find requires two string arguments")
                }
            }
            "regex_find_all" => {
                if args.len() != 2 {
                    return Err("regex_find_all requires two arguments");
                }
                if let (Value::String(pattern), Value::String(text)) = (&args[0], &args[1]) {
                    match Regex::new(pattern) {
                        Ok(re) => {
                            let matches: Vec<Value> = re
                                .find_iter(text)
                                .map(|m| Value::String(m.as_str().to_string()))
                                .collect();
                            Ok(Value::List(matches))
                        }
                        Err(_) => Err("Invalid regex pattern"),
                    }
                } else {
                    Err("regex_find_all requires two string arguments")
                }
            }
            "regex_replace" => {
                if args.len() != 3 {
                    return Err("regex_replace requires three arguments");
                }
                if let (Value::String(pattern), Value::String(text), Value::String(replacement)) =
                    (&args[0], &args[1], &args[2])
                {
                    match Regex::new(pattern) {
                        Ok(re) => Ok(Value::String(
                            re.replace_all(text, replacement.as_str()).to_string(),
                        )),
                        Err(_) => Err("Invalid regex pattern"),
                    }
                } else {
                    Err("regex_replace requires three string arguments")
                }
            }
            // JSON functions
            "json_parse" => {
                if args.len() != 1 {
                    return Err("json_parse requires one argument");
                }
                if let Value::String(json_str) = &args[0] {
                    match serde_json::from_str::<serde_json::Value>(json_str) {
                        Ok(json_value) => Ok(json_value_to_pain_value(&json_value)),
                        Err(_) => Err("Invalid JSON string"),
                    }
                } else {
                    Err("json_parse requires string argument")
                }
            }
            "json_stringify" => {
                if args.len() != 1 {
                    return Err("json_stringify requires one argument");
                }
                match pain_value_to_json_value(&args[0]) {
                    Ok(json_value) => match serde_json::to_string(&json_value) {
                        Ok(s) => Ok(Value::String(s)),
                        Err(_) => Err("Failed to stringify value"),
                    },
                    Err(e) => Err(e),
                }
            }
            "pml_load_file" => {
                if args.len() != 1 {
                    return Err("pml_load_file requires one argument");
                }
                if let Value::String(path) = &args[0] {
                    match fs::read_to_string(path) {
                        Ok(content) => match crate::pml_parser::parse_pml(&content) {
                            Ok(node) => Ok(node.to_value()),
                            Err(_) => Err("PML parse error"),
                        },
                        Err(_) => Err("Failed to read PML file"),
                    }
                } else {
                    Err("pml_load_file requires string argument")
                }
            }
            "pml_parse" => {
                if args.len() != 1 {
                    return Err("pml_parse requires one argument");
                }
                if let Value::String(source) = &args[0] {
                    match crate::pml_parser::parse_pml(source) {
                        Ok(node) => Ok(node.to_value()),
                        Err(_) => Err("PML parse error"),
                    }
                } else {
                    Err("pml_parse requires string argument")
                }
            }
            _ => Err("Unknown stdlib function"),
        }
    }

    /// Evaluate a statement in REPL mode (returns result value for expressions)
    pub fn eval_repl_stmt(
        &mut self,
        stmt: &Statement,
        env: &mut Environment,
    ) -> Result<Option<Value>, &'static str> {
        match stmt {
            Statement::Expr(expr) => {
                let value = self.eval_expr(expr, env)?;
                Ok(Some(value))
            }
            Statement::Let {
                name,
                init,
                mutable: _,
                ty: _,
            } => {
                let value = self.eval_expr(init, env)?;
                env.set_var(name.clone(), value.clone());
                Ok(Some(value))
            }
            _ => match self.eval_stmt(stmt, env)? {
                ControlFlow::Return(val) => Ok(Some(val)),
                ControlFlow::Next => Ok(None),
                ControlFlow::Break => Err("Break outside loop"),
                ControlFlow::Continue => Err("Continue outside loop"),
            },
        }
    }

    /// Create a new environment for REPL (with stdlib functions)
    pub fn create_repl_env() -> Environment {
        Environment::new()
    }
}

// Helper functions for JSON conversion
fn json_value_to_pain_value(json: &serde_json::Value) -> Value {
    match json {
        serde_json::Value::Null => Value::None,
        serde_json::Value::Bool(b) => Value::Bool(*b),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Value::Int(i)
            } else if let Some(f) = n.as_f64() {
                Value::Float(f)
            } else {
                Value::None
            }
        }
        serde_json::Value::String(s) => Value::String(s.clone()),
        serde_json::Value::Array(arr) => {
            Value::List(arr.iter().map(json_value_to_pain_value).collect())
        }
        serde_json::Value::Object(obj) => {
            // Convert object to a map-like structure
            // For now, we'll represent it as a list of key-value pairs
            // In the future, we might want a proper Map type
            let mut pairs = Vec::new();
            for (key, value) in obj {
                pairs.push(Value::String(format!(
                    "{}: {}",
                    key,
                    json_value_to_string(value)
                )));
            }
            Value::List(pairs)
        }
    }
}

fn json_value_to_string(json: &serde_json::Value) -> String {
    match json {
        serde_json::Value::String(s) => format!("\"{}\"", s),
        serde_json::Value::Number(n) => n.to_string(),
        serde_json::Value::Bool(b) => b.to_string(),
        serde_json::Value::Null => "null".to_string(),
        serde_json::Value::Array(_) => "[...]".to_string(),
        serde_json::Value::Object(_) => "{...}".to_string(),
    }
}

fn pain_value_to_json_value(value: &Value) -> Result<serde_json::Value, &'static str> {
    match value {
        Value::None => Ok(serde_json::Value::Null),
        Value::Bool(b) => Ok(serde_json::Value::Bool(*b)),
        Value::Int(i) => Ok(serde_json::Value::Number(serde_json::Number::from(*i))),
        Value::Float(f) => serde_json::Number::from_f64(*f)
            .map(serde_json::Value::Number)
            .ok_or("Invalid float value"),
        Value::String(s) => Ok(serde_json::Value::String(s.clone())),
        Value::List(list) => {
            let json_array: Result<Vec<serde_json::Value>, _> =
                list.iter().map(pain_value_to_json_value).collect();
            json_array.map(serde_json::Value::Array)
        }
        Value::Array(arr) => {
            let json_array: Result<Vec<serde_json::Value>, _> =
                arr.iter().map(pain_value_to_json_value).collect();
            json_array.map(serde_json::Value::Array)
        }
        Value::Object(_) => Err("Objects cannot be converted to JSON yet"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;

    #[test]
    fn test_interpret_simple_function() {
        let source = "fn main():
    return 42";
        let program = parse(source).unwrap();
        let mut interpreter = Interpreter::new().unwrap();
        let result = interpreter.interpret(&program).unwrap();
        if let Value::Int(n) = result {
            assert_eq!(n, 42);
        } else {
            panic!("Expected Int value");
        }
    }

    #[test]
    fn test_interpret_arithmetic() {
        let source = "fn main():
    return 2 + 3";
        let program = parse(source).unwrap();
        let mut interpreter = Interpreter::new().unwrap();
        let result = interpreter.interpret(&program).unwrap();
        if let Value::Int(n) = result {
            assert_eq!(n, 5);
        } else {
            panic!("Expected Int value");
        }
    }

    #[test]
    fn test_interpret_variables() {
        let source = "fn main():
    let x = 10
    let y = 20
    return x + y";
        let program = parse(source).unwrap();
        let mut interpreter = Interpreter::new().unwrap();
        let result = interpreter.interpret(&program).unwrap();
        if let Value::Int(n) = result {
            assert_eq!(n, 30);
        } else {
            panic!("Expected Int value");
        }
    }

    #[test]
    fn test_interpret_if_statement() {
        let source = "fn main():
    if true:
        return 1
    else:
        return 0";
        let program = parse(source).unwrap();
        let mut interpreter = Interpreter::new().unwrap();
        let result = interpreter.interpret(&program).unwrap();
        if let Value::Int(n) = result {
            assert_eq!(n, 1);
        } else {
            panic!("Expected Int value");
        }
    }

    #[test]
    fn test_interpret_while_loop() {
        let source = "fn main():
    let sum = 0
    let i = 0
    while i < 5:
        sum = sum + i
        i = i + 1
    return sum";
        let program = parse(source).unwrap();
        let mut interpreter = Interpreter::new().unwrap();
        let result = interpreter.interpret(&program).unwrap();
        if let Value::Int(n) = result {
            assert_eq!(n, 10); // 0+1+2+3+4 = 10
        } else {
            panic!("Expected Int value");
        }
    }

    #[test]
    fn test_interpret_function_call() {
        let source = "fn add(a: int, b: int) -> int:
    return a + b

fn main():
    return add(5, 3)";
        let program = parse(source).unwrap();
        let mut interpreter = Interpreter::new().unwrap();
        let result = interpreter.interpret(&program).unwrap();
        if let Value::Int(n) = result {
            assert_eq!(n, 8);
        } else {
            panic!("Expected Int value");
        }
    }

    #[test]
    fn test_interpret_list() {
        let source = "fn main():
    let items = [1, 2, 3]
    return len(items)";
        let program = parse(source).unwrap();
        let mut interpreter = Interpreter::new().unwrap();
        let result = interpreter.interpret(&program).unwrap();
        if let Value::Int(n) = result {
            assert_eq!(n, 3);
        } else {
            panic!("Expected Int value");
        }
    }
}
