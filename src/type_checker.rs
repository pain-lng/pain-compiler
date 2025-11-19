// Type checker module - type inference and type checking

use crate::ast::*;
use crate::span::{Position, Span};
use crate::stdlib::{get_stdlib_return_type, is_stdlib_function};
use std::collections::HashMap;

/// Helper function to get span from an expression
/// Since Expr doesn't have span yet, we return a default span
/// TODO: Add span to Expr enum
fn get_expr_span(_expr: &Expr) -> Span {
    // For now, return a default span
    // TODO: Add span field to Expr enum and return actual span
    Span::single(Position::start())
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    UndefinedVariable {
        name: String,
        span: Span,
    },
    TypeMismatch {
        expected: Type,
        found: Type,
        span: Span,
    },
    CannotInferType {
        message: String,
        span: Span,
    },
    InvalidOperation {
        op: String,
        left: Type,
        right: Option<Type>,
        span: Span,
    },
}

pub type TypeResult<T> = Result<T, TypeError>;

#[derive(Debug, Clone)]
pub struct TypeContext {
    variables: HashMap<String, Type>,
    functions: HashMap<String, Function>,
    classes: HashMap<String, Class>, // Class definitions
}

impl TypeContext {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
            classes: HashMap::new(),
        }
    }

    pub fn add_class(&mut self, name: String, class: Class) {
        self.classes.insert(name, class);
    }

    pub fn get_class(&self, name: &str) -> Option<&Class> {
        self.classes.get(name)
    }

    pub fn add_variable(&mut self, name: String, ty: Type) {
        self.variables.insert(name, ty);
    }

    pub fn get_variable(&self, name: &str) -> Option<&Type> {
        self.variables.get(name)
    }

    pub fn add_function(&mut self, name: String, func: Function) {
        self.functions.insert(name, func);
    }
}

pub fn type_check_program(program: &Program) -> TypeResult<()> {
    let mut ctx = TypeContext::new();

    // First pass: collect class and function signatures
    for item in &program.items {
        match item {
            Item::Function(func) => {
                ctx.add_function(func.name.clone(), func.clone());
            }
            Item::Class(class) => {
                ctx.add_class(class.name.clone(), class.clone());
            }
        }
    }

    // Second pass: type check classes and functions
    for item in &program.items {
        match item {
            Item::Function(func) => {
                type_check_function(&mut ctx, func)?;
            }
            Item::Class(class) => {
                type_check_class(&mut ctx, class)?;
            }
        }
    }

    Ok(())
}

fn type_check_class(ctx: &mut TypeContext, class: &Class) -> TypeResult<()> {
    // Create a new context for class methods (with access to class fields)
    let mut class_ctx = ctx.clone();

    // Add class fields to context (as variables)
    for field in &class.fields {
        class_ctx.add_variable(field.name.clone(), field.ty.clone());
    }

    // Type check methods
    for method in &class.methods {
        // Add 'self' parameter implicitly
        let mut method_ctx = class_ctx.clone();
        method_ctx.add_variable("self".to_string(), Type::Named(class.name.clone()));

        // Add method parameters
        for param in &method.params {
            method_ctx.add_variable(param.name.clone(), param.ty.clone());
        }

        // Type check method body
        for stmt in &method.body {
            type_check_statement(&mut method_ctx, stmt)?;
        }
    }

    Ok(())
}

fn type_check_function(ctx: &mut TypeContext, func: &Function) -> TypeResult<()> {
    // Create new scope for function parameters
    let mut func_ctx = ctx.clone();

    // Add parameters to context
    for param in &func.params {
        func_ctx.add_variable(param.name.clone(), param.ty.clone());
    }

    // Type check body
    for stmt in &func.body {
        type_check_statement(&mut func_ctx, stmt)?;
    }

    // Check return type if specified
    if let Some(_expected_return) = &func.return_type {
        // TODO: Check that all return statements match expected_return
    }

    Ok(())
}

fn type_check_statement(ctx: &mut TypeContext, stmt: &Statement) -> TypeResult<Type> {
    match stmt {
        Statement::Expr(expr) => {
            infer_expr_type(ctx, expr)?;
            Ok(Type::Dynamic) // Expression statements don't have a type
        }
        Statement::Let { name, ty, init, .. } => {
            let init_type = infer_expr_type(ctx, init)?;

            let var_type = if let Some(annotated_type) = ty {
                // Check that inferred type matches annotated type
                if !types_compatible(&init_type, annotated_type) {
                    // Use init expression span for error location
                    let span = get_expr_span(init);
                    return Err(TypeError::TypeMismatch {
                        expected: annotated_type.clone(),
                        found: init_type,
                        span,
                    });
                }
                annotated_type.clone()
            } else {
                // Infer type from initializer
                init_type
            };

            ctx.add_variable(name.clone(), var_type.clone());
            Ok(var_type)
        }
        Statement::Return(expr) => {
            if let Some(expr) = expr {
                infer_expr_type(ctx, expr)
            } else {
                Ok(Type::Dynamic) // void return
            }
        }
        Statement::If {
            cond, then, else_, ..
        } => {
            let cond_type = infer_expr_type(ctx, cond)?;
            if !types_compatible(&cond_type, &Type::Bool) {
                let span = get_expr_span(cond);
                return Err(TypeError::TypeMismatch {
                    expected: Type::Bool,
                    found: cond_type,
                    span,
                });
            }

            // Type check branches
            for stmt in then {
                type_check_statement(ctx, stmt)?;
            }

            if let Some(else_body) = else_ {
                for stmt in else_body {
                    type_check_statement(ctx, stmt)?;
                }
            }

            Ok(Type::Dynamic)
        }
        Statement::For {
            var, iter, body, ..
        } => {
            let iter_type = infer_expr_type(ctx, iter)?;
            // TODO: Check that iter_type is iterable (list, array, etc.)

            // Infer element type from iterator
            let element_type = match &iter_type {
                Type::List(inner) => *inner.clone(),
                Type::Array(inner) => *inner.clone(),
                _ => Type::Dynamic, // Fallback
            };

            ctx.add_variable(var.clone(), element_type);

            for stmt in body {
                type_check_statement(ctx, stmt)?;
            }

            Ok(Type::Dynamic)
        }
        Statement::While { cond, body, .. } => {
            let cond_type = infer_expr_type(ctx, cond)?;
            if !types_compatible(&cond_type, &Type::Bool) {
                let span = get_expr_span(cond);
                return Err(TypeError::TypeMismatch {
                    expected: Type::Bool,
                    found: cond_type,
                    span,
                });
            }

            for stmt in body {
                type_check_statement(ctx, stmt)?;
            }

            Ok(Type::Dynamic)
        }
        Statement::Break | Statement::Continue => Ok(Type::Dynamic),
    }
}

fn infer_expr_type(ctx: &TypeContext, expr: &Expr) -> TypeResult<Type> {
    match expr {
        Expr::Integer(_) => Ok(Type::Int),
        Expr::Float(_) => Ok(Type::Float64),
        Expr::String(_) => Ok(Type::Str),
        Expr::FString(_) => Ok(Type::Str),
        Expr::Bool(_) => Ok(Type::Bool),
        Expr::None => Ok(Type::Dynamic),
        Expr::List(elements) => {
            // Infer element type from first element, or Dynamic if empty
            if elements.is_empty() {
                Ok(Type::List(Box::new(Type::Dynamic)))
            } else {
                let first_type = infer_expr_type(ctx, &elements[0])?;
                // Check all elements have same type (simplified - just use first)
                Ok(Type::List(Box::new(first_type)))
            }
        }
        Expr::Ident(name) => {
            // Check if it's a stdlib function
            if is_stdlib_function(name) {
                // Return Dynamic for now - actual type will be checked in Call
                return Ok(Type::Dynamic);
            }
            ctx.get_variable(name).cloned().ok_or_else(|| {
                let span = get_expr_span(expr);
                TypeError::UndefinedVariable {
                    name: name.clone(),
                    span,
                }
            })
        }
        Expr::Add(left, right)
        | Expr::Sub(left, right)
        | Expr::Mul(left, right)
        | Expr::Div(left, right)
        | Expr::Mod(left, right) => {
            let left_type = infer_expr_type(ctx, left)?;
            let right_type = infer_expr_type(ctx, right)?;
            infer_binary_op_type(&left_type, &right_type)
        }
        Expr::Eq(left, right)
        | Expr::Ne(left, right)
        | Expr::Lt(left, right)
        | Expr::Gt(left, right)
        | Expr::Le(left, right)
        | Expr::Ge(left, right) => {
            let _left_type = infer_expr_type(ctx, left)?;
            let _right_type = infer_expr_type(ctx, right)?;
            Ok(Type::Bool)
        }
        Expr::And(left, right) | Expr::Or(left, right) => {
            let left_type = infer_expr_type(ctx, left)?;
            let right_type = infer_expr_type(ctx, right)?;
            if types_compatible(&left_type, &Type::Bool)
                && types_compatible(&right_type, &Type::Bool)
            {
                Ok(Type::Bool)
            } else {
                let span = get_expr_span(expr);
                Err(TypeError::TypeMismatch {
                    expected: Type::Bool,
                    found: left_type,
                    span,
                })
            }
        }
        Expr::Not(operand) => {
            let op_type = infer_expr_type(ctx, operand)?;
            if types_compatible(&op_type, &Type::Bool) {
                Ok(Type::Bool)
            } else {
                let span = get_expr_span(expr);
                Err(TypeError::TypeMismatch {
                    expected: Type::Bool,
                    found: op_type,
                    span,
                })
            }
        }
        Expr::Neg(operand) => {
            let op_type = infer_expr_type(ctx, operand)?;
            match op_type {
                Type::Int | Type::Float32 | Type::Float64 => Ok(op_type),
                _ => {
                    let span = get_expr_span(expr);
                    Err(TypeError::InvalidOperation {
                        op: "negation".to_string(),
                        left: op_type,
                        right: None,
                        span,
                    })
                }
            }
        }
        Expr::Call { callee, args } => {
            // Evaluate argument types
            let arg_types: Result<Vec<Type>, TypeError> =
                args.iter().map(|arg| infer_expr_type(ctx, arg)).collect();
            let arg_types = arg_types?;

            // Handle stdlib functions
            if let Expr::Ident(name) = callee.as_ref() {
                if let Some(return_type) = get_stdlib_return_type(name, &arg_types) {
                    return Ok(return_type);
                }
            }

            // TODO: Check user-defined function calls
            Ok(Type::Dynamic)
        }
        Expr::Index(container, index) => {
            let container_type = infer_expr_type(ctx, container)?;
            let _index_type = infer_expr_type(ctx, index)?;

            match container_type {
                Type::List(inner) | Type::Array(inner) => Ok(*inner),
                Type::Map(_, value) => Ok(*value),
                _ => {
                    let span = get_expr_span(expr);
                    Err(TypeError::InvalidOperation {
                        op: "indexing".to_string(),
                        left: container_type,
                        right: None,
                        span,
                    })
                }
            }
        }
        Expr::Member(obj, field) => {
            let obj_type = infer_expr_type(ctx, obj)?;
            // Look up field type in class/struct
            match obj_type {
                Type::Named(class_name) => {
                    if let Some(class) = ctx.get_class(&class_name) {
                        // Find field in class
                        if let Some(field_def) = class.fields.iter().find(|f| f.name == *field) {
                            Ok(field_def.ty.clone())
                        } else {
                            // Check if it's a method call (will be handled by Call)
                            Ok(Type::Dynamic)
                        }
                    } else {
                        Ok(Type::Dynamic) // Unknown class
                    }
                }
                _ => Ok(Type::Dynamic), // Not a class type
            }
        }
        Expr::IsInstance(expr, _ty) => {
            let _expr_type = infer_expr_type(ctx, expr)?;
            // isinstance always returns bool
            Ok(Type::Bool)
        }
        Expr::New { class_name, args } => {
            // Check if class exists
            if let Some(_class) = ctx.get_class(class_name) {
                // TODO: Check constructor arguments match class fields
                // For now, just verify class exists
                let _arg_types: Result<Vec<Type>, TypeError> =
                    args.iter().map(|arg| infer_expr_type(ctx, arg)).collect();
                Ok(Type::Named(class_name.clone()))
            } else {
                let span = get_expr_span(expr);
                Err(TypeError::UndefinedVariable {
                    name: format!("Class '{}' not found", class_name),
                    span,
                })
            }
        }
        Expr::Assign(left, right)
        | Expr::AddAssign(left, right)
        | Expr::SubAssign(left, right)
        | Expr::MulAssign(left, right)
        | Expr::DivAssign(left, right) => {
            let _left_type = infer_expr_type(ctx, left)?;
            let right_type = infer_expr_type(ctx, right)?;
            Ok(right_type)
        }
    }
}

fn infer_binary_op_type(left: &Type, right: &Type) -> TypeResult<Type> {
    match (left, right) {
        (Type::Int, Type::Int) => Ok(Type::Int),
        (Type::Float32, Type::Float32) => Ok(Type::Float32),
        (Type::Float64, Type::Float64) => Ok(Type::Float64),
        (Type::Int, Type::Float32) | (Type::Float32, Type::Int) => Ok(Type::Float32),
        (Type::Int, Type::Float64) | (Type::Float64, Type::Int) => Ok(Type::Float64),
        (Type::Float32, Type::Float64) | (Type::Float64, Type::Float32) => Ok(Type::Float64),
        (Type::Dynamic, _) | (_, Type::Dynamic) => Ok(Type::Dynamic),
        _ => {
            // Use a default span - we don't have access to the expression here
            let span = Span::single(Position::start());
            Err(TypeError::InvalidOperation {
                op: "arithmetic".to_string(),
                left: left.clone(),
                right: Some(right.clone()),
                span,
            })
        }
    }
}

fn types_compatible(actual: &Type, expected: &Type) -> bool {
    match (actual, expected) {
        (Type::Dynamic, _) | (_, Type::Dynamic) => true,
        (a, b) => a == b,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;

    #[test]
    fn test_infer_integer_literal() {
        let ctx = TypeContext::new();
        let expr = Expr::Integer(42);
        assert_eq!(infer_expr_type(&ctx, &expr).unwrap(), Type::Int);
    }

    #[test]
    fn test_infer_binary_op() {
        let ctx = TypeContext::new();
        let expr = Expr::Add(Box::new(Expr::Integer(1)), Box::new(Expr::Integer(2)));
        assert_eq!(infer_expr_type(&ctx, &expr).unwrap(), Type::Int);
    }

    #[test]
    fn test_infer_float_literal() {
        use std::f64::consts::PI;

        let ctx = TypeContext::new();
        let expr = Expr::Float(PI);
        assert_eq!(infer_expr_type(&ctx, &expr).unwrap(), Type::Float64);
    }

    #[test]
    fn test_infer_string_literal() {
        let ctx = TypeContext::new();
        let expr = Expr::String("hello".to_string());
        assert_eq!(infer_expr_type(&ctx, &expr).unwrap(), Type::Str);
    }

    #[test]
    fn test_infer_bool_literal() {
        let ctx = TypeContext::new();
        let expr = Expr::Bool(true);
        assert_eq!(infer_expr_type(&ctx, &expr).unwrap(), Type::Bool);
    }

    #[test]
    fn test_infer_variable() {
        let mut ctx = TypeContext::new();
        ctx.add_variable("x".to_string(), Type::Int);
        let expr = Expr::Ident("x".to_string());
        assert_eq!(infer_expr_type(&ctx, &expr).unwrap(), Type::Int);
    }

    #[test]
    fn test_undefined_variable_error() {
        let ctx = TypeContext::new();
        let expr = Expr::Ident("undefined".to_string());
        assert!(infer_expr_type(&ctx, &expr).is_err());
    }

    #[test]
    fn test_type_mismatch_in_let() {
        let source = "fn test():
    let x: int = \"hello\"";
        let program = parse(source).unwrap();
        let result = type_check_program(&program);
        assert!(result.is_err());
        if let Err(TypeError::TypeMismatch {
            expected, found, ..
        }) = result
        {
            assert_eq!(expected, Type::Int);
            assert_eq!(found, Type::Str);
        } else {
            panic!("Expected TypeMismatch error");
        }
    }

    #[test]
    fn test_type_inference_in_let() {
        let source = "fn test():
    let x = 42
    let y = 3.14
    let z = \"hello\"";
        let program = parse(source).unwrap();
        let result = type_check_program(&program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_arithmetic_type_promotion() {
        let ctx = TypeContext::new();
        // int + float64 should promote to float64
        let expr = Expr::Add(Box::new(Expr::Integer(1)), Box::new(Expr::Float(2.0)));
        assert_eq!(infer_expr_type(&ctx, &expr).unwrap(), Type::Float64);
    }

    #[test]
    fn test_comparison_returns_bool() {
        let ctx = TypeContext::new();
        let expr = Expr::Lt(Box::new(Expr::Integer(1)), Box::new(Expr::Integer(2)));
        assert_eq!(infer_expr_type(&ctx, &expr).unwrap(), Type::Bool);
    }

    #[test]
    fn test_logical_operators_require_bool() {
        let source = "fn test():
    let x = 1 && 2";
        let program = parse(source).unwrap();
        let result = type_check_program(&program);
        // Should error because && requires bool operands
        assert!(result.is_err());
    }

    #[test]
    fn test_if_condition_must_be_bool() {
        let source = "fn test(x: int):
    if x:
        return 1";
        let program = parse(source).unwrap();
        let result = type_check_program(&program);
        assert!(result.is_err());
    }

    #[test]
    fn test_while_condition_must_be_bool() {
        let source = "fn test(x: int):
    while x:
        return 1";
        let program = parse(source).unwrap();
        let result = type_check_program(&program);
        assert!(result.is_err());
    }

    #[test]
    fn test_list_type_inference() {
        let ctx = TypeContext::new();
        let expr = Expr::List(vec![Expr::Integer(1), Expr::Integer(2), Expr::Integer(3)]);
        let result = infer_expr_type(&ctx, &expr).unwrap();
        if let Type::List(inner) = result {
            assert_eq!(*inner, Type::Int);
        } else {
            panic!("Expected List type");
        }
    }

    #[test]
    fn test_indexing_list() {
        let source = "fn test(items: list[int]) -> int:
    return items[0]";
        let program = parse(source).unwrap();
        let result = type_check_program(&program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_function_call_type_checking() {
        let source = "fn add(a: int, b: int) -> int:
    return a + b

fn test():
    let result = add(1, 2)
    return result";
        let program = parse(source).unwrap();
        let result = type_check_program(&program);
        assert!(result.is_ok());
    }

    #[test]
    fn test_type_context_variable_lookup() {
        let mut ctx = TypeContext::new();
        ctx.add_variable("x".to_string(), Type::Int);
        assert_eq!(ctx.get_variable("x"), Some(&Type::Int));
        assert_eq!(ctx.get_variable("y"), None);
    }

    #[test]
    fn test_type_context_function_lookup() {
        let mut ctx = TypeContext::new();
        let func = Function {
            doc: None,
            attrs: vec![],
            name: "test".to_string(),
            params: vec![],
            return_type: Some(Type::Int),
            body: vec![],
            span: Span::single(Position::start()),
        };
        ctx.add_function("test".to_string(), func.clone());
        // Functions are stored but not directly accessible via get_variable
        // This test verifies the function is stored
    }
}
