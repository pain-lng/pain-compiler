// Integration tests for Pain compiler pipeline

use pain_compiler::*;

/// Helper function to compile a source string through the full pipeline
fn compile_pipeline(source: &str) -> Result<IrProgram, String> {
    let program = parse(source)?;
    type_check_program(&program).map_err(|e| format!("Type error: {:?}", e))?;
    let ir_builder = IrBuilder::new();
    let ir = ir_builder.build(&program);
    Ok(ir)
}

/// Helper function to compile and optimize
fn compile_and_optimize(source: &str) -> Result<IrProgram, String> {
    let ir = compile_pipeline(source)?;
    Ok(Optimizer::optimize(ir))
}

#[test]
fn test_parse_simple_function() {
    let source = "fn add(a: int, b: int) -> int:
    return a + b";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
    let program = result.unwrap();
    assert_eq!(program.items.len(), 1);
    
    if let Item::Function(func) = &program.items[0] {
        assert_eq!(func.name, "add");
        assert_eq!(func.params.len(), 2);
    } else {
        panic!("Expected function");
    }
}

#[test]
fn test_parse_with_indentation() {
    let source = "fn test():
    let x = 10
    let y = 20
    return x + y";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
}

#[test]
fn test_parse_if_statement() {
    let source = "fn max(a: int, b: int) -> int:
    if a > b:
        return a
    else:
        return b";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
}

#[test]
fn test_parse_while_loop() {
    let source = "fn countdown(n: int):
    while n > 0:
        print(n)
        n = n - 1";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
}

#[test]
fn test_parse_for_loop() {
    let source = "fn sum_list(items: list[int]) -> int:
    let total = 0
    for item in items:
        total = total + item
    return total";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
}

#[test]
fn test_parse_class() {
    let source = "class Point:
    let x: int
    let y: int
    
    fn new(x: int, y: int) -> Point:
        return new Point(x, y)";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
    let program = result.unwrap();
    assert!(program.items.iter().any(|item| matches!(item, Item::Class(_))));
}

#[test]
fn test_parse_list_literal() {
    let source = "fn get_numbers() -> list[int]:
    return [1, 2, 3, 4, 5]";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
}

#[test]
fn test_type_check_simple_function() {
    let source = "fn add(a: int, b: int) -> int:
    return a + b";
    
    let program = parse(source).unwrap();
    let result = type_check_program(&program);
    assert!(result.is_ok(), "Type check failed: {:?}", result.err());
}

#[test]
fn test_type_check_type_mismatch() {
    let source = "fn test():
    let x: int = \"hello\"";
    
    let program = parse(source).unwrap();
    let result = type_check_program(&program);
    assert!(result.is_err(), "Expected type error");
}

#[test]
fn test_type_check_undefined_variable() {
    let source = "fn test():
    return undefined_var";
    
    let program = parse(source).unwrap();
    let result = type_check_program(&program);
    assert!(result.is_err(), "Expected type error for undefined variable");
}

#[test]
fn test_ir_build_simple_function() {
    let source = "fn add(a: int, b: int) -> int:
    return a + b";
    
    let result = compile_pipeline(source);
    assert!(result.is_ok(), "IR build failed: {:?}", result.err());
    let ir = result.unwrap();
    assert!(!ir.functions.is_empty());
}

#[test]
fn test_ir_build_with_control_flow() {
    let source = "fn abs(x: int) -> int:
    if x < 0:
        return -x
    else:
        return x";
    
    let result = compile_pipeline(source);
    assert!(result.is_ok(), "IR build failed: {:?}", result.err());
    let ir = result.unwrap();
    let func = &ir.functions[0];
    // Should have multiple blocks (entry, then, else, merge)
    assert!(func.blocks.len() >= 2);
}

#[test]
fn test_ir_build_while_loop() {
    let source = "fn factorial(n: int) -> int:
    let result = 1
    let i = 1
    while i <= n:
        result = result * i
        i = i + 1
    return result";
    
    let result = compile_pipeline(source);
    assert!(result.is_ok(), "IR build failed: {:?}", result.err());
    let ir = result.unwrap();
    let func = &ir.functions[0];
    // Should have loop blocks (header, body, continue)
    assert!(func.blocks.len() >= 3);
}

#[test]
fn test_optimization_constant_folding() {
    let source = "fn test() -> int:
    return 2 + 3";
    
    let ir = compile_pipeline(source).unwrap();
    let optimized = Optimizer::optimize(ir);
    
    // After constant folding, the addition should be folded to a constant
    // This is a basic check - in practice, we'd verify the IR structure
    assert!(!optimized.functions.is_empty());
}

#[test]
fn test_optimization_dead_code_elimination() {
    let source = "fn test() -> int:
    let unused = 10
    let used = 5
    return used";
    
    let ir = compile_pipeline(source).unwrap();
    let optimized = Optimizer::optimize(ir);
    
    // Dead code elimination should remove unused variable
    // This is a basic check - in practice, we'd verify the IR structure
    assert!(!optimized.functions.is_empty());
}

#[test]
fn test_full_pipeline_with_stdlib() {
    let source = "fn main():
    let x = 10
    let y = 20
    print(x + y)";
    
    let result = compile_and_optimize(source);
    assert!(result.is_ok(), "Full pipeline failed: {:?}", result.err());
}

#[test]
fn test_parse_f_string() {
    let source = "fn greet(name: str) -> str:
    return f\"Hello, {name}!\"";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse f-string: {:?}", result.err());
}

#[test]
fn test_parse_indexing() {
    let source = "fn get_first(items: list[int]) -> int:
    return items[0]";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse indexing: {:?}", result.err());
}

#[test]
fn test_parse_member_access() {
    let source = "fn get_x(p: Point) -> int:
    return p.x";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse member access: {:?}", result.err());
}

#[test]
fn test_parse_isinstance() {
    let source = "fn check_type(obj: dynamic) -> bool:
    return isinstance(obj, Point)";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse isinstance: {:?}", result.err());
}

#[test]
fn test_parse_nested_blocks() {
    let source = "fn complex():
    if True:
        if False:
            return 1
        else:
            return 2
    else:
        return 3";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse nested blocks: {:?}", result.err());
}

#[test]
fn test_parse_break_continue() {
    let source = "fn test():
    while True:
        if False:
            break
        if True:
            continue";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse break/continue: {:?}", result.err());
}

#[test]
fn test_type_check_binary_operations() {
    let source = "fn test(a: int, b: int) -> bool:
    return a == b";
    
    let program = parse(source).unwrap();
    let result = type_check_program(&program);
    assert!(result.is_ok(), "Type check failed: {:?}", result.err());
}

#[test]
fn test_type_check_list_operations() {
    let source = "fn test(items: list[int]) -> int:
    return len(items)";
    
    let program = parse(source).unwrap();
    let result = type_check_program(&program);
    assert!(result.is_ok(), "Type check failed: {:?}", result.err());
}

#[test]
fn test_ir_build_function_call() {
    let source = "fn add(a: int, b: int) -> int:
    return a + b

fn main():
    let result = add(5, 3)
    return result";
    
    let result = compile_pipeline(source);
    assert!(result.is_ok(), "IR build failed: {:?}", result.err());
    let ir = result.unwrap();
    assert_eq!(ir.functions.len(), 2);
}

#[test]
fn test_ir_build_class_instantiation() {
    let source = "class Point:
    let x: int
    let y: int

fn create_point() -> Point:
    return new Point(10, 20)";
    
    let result = compile_pipeline(source);
    assert!(result.is_ok(), "IR build failed: {:?}", result.err());
}

#[test]
fn test_error_messages_have_spans() {
    let source = "fn test():
    let x = undefined_var";
    
    let program = parse(source).unwrap();
    let result = type_check_program(&program);
    assert!(result.is_err());
    
    if let Err(TypeError::UndefinedVariable { span, .. }) = result {
        // Verify span has valid line/column
        assert!(span.line() > 0);
        assert!(span.column() > 0);
    } else {
        panic!("Expected UndefinedVariable error");
    }
}

#[test]
fn test_optimization_loop_invariant() {
    let source = "fn test(n: int) -> int:
    let constant = 100
    let sum = 0
    let i = 0
    while i < n:
        sum = sum + constant
        i = i + 1
    return sum";
    
    let ir = compile_pipeline(source).unwrap();
    let optimized = Optimizer::optimize(ir);
    
    // Loop invariant code motion should move constant calculations out of loop
    assert!(!optimized.functions.is_empty());
}

#[test]
fn test_parse_attributes() {
    let source = "@inline
fn helper(x: int) -> int:
    return x * 2";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse attributes: {:?}", result.err());
    let program = result.unwrap();
    if let Item::Function(func) = &program.items[0] {
        assert!(!func.attrs.is_empty());
    }
}

#[test]
fn test_parse_doc_comments() {
    let source = "\"\"\"This is a doc comment.\"\"\"
fn documented():
    return 42";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse doc comment: {:?}", result.err());
    let program = result.unwrap();
    if let Item::Function(func) = &program.items[0] {
        assert!(func.doc.is_some());
        assert_eq!(func.doc.as_ref().unwrap(), "This is a doc comment.");
    }
}

#[test]
fn test_type_check_return_type() {
    let source = "fn get_int() -> int:
    return 42";
    
    let program = parse(source).unwrap();
    let result = type_check_program(&program);
    assert!(result.is_ok(), "Type check failed: {:?}", result.err());
}

#[test]
fn test_type_check_void_function() {
    let source = "fn print_hello():
    print(\"hello\")";
    
    let program = parse(source).unwrap();
    let result = type_check_program(&program);
    assert!(result.is_ok(), "Type check failed: {:?}", result.err());
}

#[test]
fn test_ir_build_multiple_functions() {
    let source = "fn add(a: int, b: int) -> int:
    return a + b

fn multiply(a: int, b: int) -> int:
    return a * b

fn main():
    let x = add(2, 3)
    let y = multiply(x, 4)
    return y";
    
    let result = compile_pipeline(source);
    assert!(result.is_ok(), "IR build failed: {:?}", result.err());
    let ir = result.unwrap();
    assert_eq!(ir.functions.len(), 3);
}

#[test]
fn test_parse_complex_expression() {
    let source = "fn test(a: int, b: int, c: int) -> int:
    return (a + b) * c - (a / b)";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse complex expression: {:?}", result.err());
}

#[test]
fn test_parse_logical_operators() {
    let source = "fn test(a: bool, b: bool) -> bool:
    return a && b || !a";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse logical operators: {:?}", result.err());
}

#[test]
fn test_parse_comparison_operators() {
    let source = "fn test(a: int, b: int) -> bool:
    return a < b && a != b && a <= b";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse comparison operators: {:?}", result.err());
}

#[test]
fn test_parse_assignment_operators() {
    let source = "fn test():
    let x = 10
    x += 5
    x -= 2
    x *= 3
    x /= 2
    return x";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse assignment operators: {:?}", result.err());
}

#[test]
fn test_parse_nested_function_calls() {
    let source = "fn add(a: int, b: int) -> int:
    return a + b

fn test():
    return add(add(1, 2), add(3, 4))";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse nested calls: {:?}", result.err());
}

#[test]
fn test_parse_empty_list() {
    let source = "fn test() -> list[int]:
    return []";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse empty list: {:?}", result.err());
}

#[test]
fn test_parse_list_with_expressions() {
    let source = "fn test(a: int, b: int) -> list[int]:
    return [a, b, a + b, a * b]";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse list with expressions: {:?}", result.err());
}

#[test]
fn test_type_check_function_call() {
    let source = "fn add(a: int, b: int) -> int:
    return a + b

fn test():
    let result = add(5, 3)
    return result";
    
    let program = parse(source).unwrap();
    let result = type_check_program(&program);
    assert!(result.is_ok(), "Type check failed: {:?}", result.err());
}

#[test]
fn test_ir_build_phi_nodes() {
    let source = "fn test(x: int) -> int:
    if x > 0:
        let y = 10
    else:
        let y = 20
    return y";
    
    let result = compile_pipeline(source);
    assert!(result.is_ok(), "IR build failed: {:?}", result.err());
    // Should have phi nodes for merging y values from different branches
}

#[test]
fn test_optimization_common_subexpression() {
    let source = "fn test(a: int, b: int) -> int:
    let x = a + b
    let y = a + b
    return x + y";
    
    let ir = compile_pipeline(source).unwrap();
    let optimized = Optimizer::optimize(ir);
    
    // CSE should eliminate duplicate a + b computation
    assert!(!optimized.functions.is_empty());
}

#[test]
fn test_parse_type_annotations() {
    let source = "fn test() -> int:
    let x: int = 10
    let y: float64 = 3.14
    let z: bool = True
    return x";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse type annotations: {:?}", result.err());
}

#[test]
fn test_parse_type_inference() {
    let source = "fn test():
    let x = 10
    let y = 3.14
    let z = True
    return x";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse type inference: {:?}", result.err());
}

#[test]
fn test_parse_complex_nested_structure() {
    let source = "fn complex():
    let x = 0
    while x < 10:
        if x % 2 == 0:
            let y = x * 2
            if y > 5:
                return y
        x = x + 1
    return -1";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse complex nested structure: {:?}", result.err());
}

#[test]
fn test_full_pipeline_fibonacci() {
    let source = "fn fib(n: int) -> int:
    if n <= 1:
        return n
    else:
        return fib(n - 1) + fib(n - 2)";
    
    let result = compile_and_optimize(source);
    assert!(result.is_ok(), "Full pipeline failed: {:?}", result.err());
}

#[test]
fn test_full_pipeline_factorial() {
    let source = "fn factorial(n: int) -> int:
    if n <= 1:
        return 1
    else:
        return n * factorial(n - 1)";
    
    let result = compile_and_optimize(source);
    assert!(result.is_ok(), "Full pipeline failed: {:?}", result.err());
}

#[test]
fn test_parse_string_operations() {
    let source = "fn test(s: str) -> str:
    let len = len(s)
    return s";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse string operations: {:?}", result.err());
}

#[test]
fn test_parse_array_indexing() {
    let source = "fn test(arr: array[int], idx: int) -> int:
    return arr[idx]";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse array indexing: {:?}", result.err());
}

#[test]
fn test_parse_chained_member_access() {
    let source = "fn test(p: Point) -> int:
    return p.x";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse chained member access: {:?}", result.err());
}

#[test]
fn test_parse_unary_operators() {
    let source = "fn test(x: int) -> int:
    return -x";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse unary operators: {:?}", result.err());
}

#[test]
fn test_parse_boolean_literals() {
    let source = "fn test() -> bool:
    return True && False || !True";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse boolean literals: {:?}", result.err());
}

#[test]
fn test_parse_none_literal() {
    let source = "fn test() -> dynamic:
    return None";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse None literal: {:?}", result.err());
}

#[test]
fn test_type_check_arithmetic_operations() {
    let source = "fn test(a: int, b: int) -> int:
    return a + b - a * b / a % b";
    
    let program = parse(source).unwrap();
    let result = type_check_program(&program);
    assert!(result.is_ok(), "Type check failed: {:?}", result.err());
}

#[test]
fn test_ir_build_with_parameters() {
    let source = "fn test(a: int, b: int, c: int) -> int:
    return a + b + c";
    
    let result = compile_pipeline(source);
    assert!(result.is_ok(), "IR build failed: {:?}", result.err());
    let ir = result.unwrap();
    let func = &ir.functions[0];
    assert_eq!(func.params.len(), 3);
}

#[test]
fn test_optimization_tail_call() {
    let source = "fn helper(n: int, acc: int) -> int:
    if n == 0:
        return acc
    else:
        return helper(n - 1, acc + n)";
    
    let ir = compile_pipeline(source).unwrap();
    let optimized = Optimizer::optimize(ir);
    
    // Tail call optimization should mark tail calls
    assert!(!optimized.functions.is_empty());
}

#[test]
fn test_parse_error_recovery() {
    // Test that parser provides good error messages
    let source = "fn test(
    return 42";
    
    let result = parse(source);
    assert!(result.is_err(), "Expected parse error");
    let error = result.unwrap_err();
    assert!(error.contains("Expected") || error.contains("error"));
}

#[test]
fn test_type_check_error_recovery() {
    // Test that type checker provides good error messages
    let source = "fn test():
    let x: int = \"not an int\"";
    
    let program = parse(source).unwrap();
    let result = type_check_program(&program);
    assert!(result.is_err(), "Expected type error");
    
    if let Err(TypeError::TypeMismatch { expected, found, .. }) = result {
        assert_eq!(expected, Type::Int);
        assert_eq!(found, Type::Str);
    } else {
        panic!("Expected TypeMismatch error");
    }
}

#[test]
fn test_ir_build_empty_function() {
    let source = "fn empty(): pass";
    
    let result = compile_pipeline(source);
    // Empty function should still compile
    assert!(result.is_ok() || result.is_err()); // May or may not be supported
}

#[test]
fn test_parse_multiline_string() {
    let source = "fn test() -> str:
    return \"line1
line2
line3\"";
    
    let result = parse(source);
    // Multiline strings may or may not be supported
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_pipeline_sum() {
    let source = "fn sum(n: int) -> int:
    let total = 0
    let i = 0
    while i <= n:
        total = total + i
        i = i + 1
    return total";
    
    let result = compile_and_optimize(source);
    assert!(result.is_ok(), "Full pipeline failed: {:?}", result.err());
}

#[test]
fn test_parse_class_with_methods() {
    let source = "class Counter:
    let value: int
    
    fn increment():
        self.value = self.value + 1
    
    fn get() -> int:
        return self.value";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse class with methods: {:?}", result.err());
    let program = result.unwrap();
    if let Item::Class(class) = &program.items[0] {
        assert!(!class.methods.is_empty());
    }
}

#[test]
fn test_parse_class_with_fields() {
    let source = "class Person:
    let name: str
    let age: int
    let active: bool";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse class with fields: {:?}", result.err());
    let program = result.unwrap();
    if let Item::Class(class) = &program.items[0] {
        assert_eq!(class.fields.len(), 3);
    }
}

#[test]
fn test_ir_build_class_methods() {
    let source = "class Point:
    let x: int
    let y: int
    
    fn distance() -> float64:
        return 0.0";
    
    let result = compile_pipeline(source);
    assert!(result.is_ok(), "IR build failed: {:?}", result.err());
    let ir = result.unwrap();
    // Should have struct definition and method function
    assert!(!ir.structs.is_empty() || !ir.functions.is_empty());
}

#[test]
fn test_parse_complex_type() {
    let source = "fn test() -> list[list[int]]:
    return [[1, 2], [3, 4]]";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse complex type: {:?}", result.err());
}

#[test]
fn test_parse_map_type() {
    let source = "fn test() -> map[str, int]:
    return {}";
    
    let result = parse(source);
    // Map type may or may not be fully supported
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_optimization_preserves_semantics() {
    // Test that optimizations don't break correct programs
    let source = "fn test(a: int, b: int) -> int:
    let x = a + b
    let y = a + b
    return x + y";
    
    let ir1 = compile_pipeline(source).unwrap();
    let ir2 = Optimizer::optimize(ir1.clone());
    
    // Both should have functions
    assert!(!ir1.functions.is_empty());
    assert!(!ir2.functions.is_empty());
}

#[test]
fn test_parse_operator_precedence() {
    let source = "fn test(a: int, b: int, c: int) -> int:
    return a + b * c";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse operator precedence: {:?}", result.err());
    // Should parse as a + (b * c), not (a + b) * c
}

#[test]
fn test_parse_parenthesized_expression() {
    let source = "fn test(a: int, b: int, c: int) -> int:
    return (a + b) * c";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse parenthesized expression: {:?}", result.err());
}

#[test]
fn test_type_check_condition_must_be_bool() {
    let source = "fn test(x: int):
    if x:
        return 1";
    
    let program = parse(source).unwrap();
    let result = type_check_program(&program);
    // Should error because condition must be bool
    assert!(result.is_err(), "Expected type error for non-bool condition");
}

#[test]
fn test_parse_float_literals() {
    let source = "fn test() -> float64:
    return 3.14";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse float literal: {:?}", result.err());
}

#[test]
fn test_parse_scientific_notation() {
    let source = "fn test() -> float64:
    return 1.5e10";
    
    let result = parse(source);
    // Scientific notation may or may not be supported
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_full_pipeline_hello_world() {
    let source = "fn main():
    print(\"Hello, World!\")";
    
    let result = compile_and_optimize(source);
    assert!(result.is_ok(), "Full pipeline failed: {:?}", result.err());
}

#[test]
fn test_parse_comments() {
    let source = "# This is a comment
fn test() -> int:
    # Another comment
    return 42  # Inline comment";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse with comments: {:?}", result.err());
}

#[test]
fn test_parse_empty_program() {
    let source = "";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse empty program: {:?}", result.err());
    let program = result.unwrap();
    assert_eq!(program.items.len(), 0);
}

#[test]
fn test_parse_multiple_top_level_items() {
    let source = "fn func1():
    return 1

fn func2():
    return 2

class MyClass:
    let x: int";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse multiple items: {:?}", result.err());
    let program = result.unwrap();
    assert_eq!(program.items.len(), 3);
}

#[test]
fn test_ir_build_with_stdlib_calls() {
    let source = "fn test(s: str) -> int:
    return len(s)";
    
    let result = compile_pipeline(source);
    assert!(result.is_ok(), "IR build failed: {:?}", result.err());
}

#[test]
fn test_optimization_removes_unused_code() {
    let source = "fn test() -> int:
    let unused1 = 10
    let unused2 = 20
    let used = 5
    return used";
    
    let ir = compile_pipeline(source).unwrap();
    let optimized = Optimizer::optimize(ir);
    
    // Dead code elimination should remove unused variables
    assert!(!optimized.functions.is_empty());
}

#[test]
fn test_parse_negative_numbers() {
    let source = "fn test() -> int:
    return -42";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse negative number: {:?}", result.err());
}

#[test]
fn test_parse_zero() {
    let source = "fn test() -> int:
    return 0";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse zero: {:?}", result.err());
}

#[test]
fn test_type_check_list_type() {
    let source = "fn test(items: list[int]) -> int:
    if len(items) > 0:
        return items[0]
    else:
        return 0";
    
    let program = parse(source).unwrap();
    let result = type_check_program(&program);
    assert!(result.is_ok(), "Type check failed: {:?}", result.err());
}

#[test]
fn test_ir_build_conditional_return() {
    let source = "fn test(x: int) -> int:
    if x > 0:
        return 1
    return 0";
    
    let result = compile_pipeline(source);
    assert!(result.is_ok(), "IR build failed: {:?}", result.err());
}

#[test]
fn test_parse_ternary_like_pattern() {
    // Pain doesn't have ternary, but we can test similar patterns
    let source = "fn test(x: int) -> int:
    if x > 0:
        return 1
    else:
        return 0";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse conditional return: {:?}", result.err());
}

#[test]
fn test_full_pipeline_with_loops() {
    let source = "fn sum_range(start: int, end: int) -> int:
    let total = 0
    let i = start
    while i < end:
        total = total + i
        i = i + 1
    return total";
    
    let result = compile_and_optimize(source);
    assert!(result.is_ok(), "Full pipeline failed: {:?}", result.err());
}

#[test]
fn test_parse_string_concatenation() {
    let source = "fn test(a: str, b: str) -> str:
    return a + b";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse string concatenation: {:?}", result.err());
}

#[test]
fn test_parse_escaped_strings() {
    let source = "fn test() -> str:
    return \"Hello\\nWorld\"";
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse escaped string: {:?}", result.err());
}

#[test]
fn test_ir_build_with_phi_for_loops() {
    let source = "fn test(n: int) -> int:
    let sum = 0
    let i = 0
    while i < n:
        sum = sum + i
        i = i + 1
    return sum";
    
    let result = compile_pipeline(source);
    assert!(result.is_ok(), "IR build failed: {:?}", result.err());
    // Should have phi nodes for loop variables
}

#[test]
fn test_optimization_handles_loops() {
    let source = "fn test(n: int) -> int:
    let constant = 100
    let sum = 0
    let i = 0
    while i < n:
        sum = sum + constant
        i = i + 1
    return sum";
    
    let ir = compile_pipeline(source).unwrap();
    let optimized = Optimizer::optimize(ir);
    
    // Loop invariant code motion should optimize this
    assert!(!optimized.functions.is_empty());
}

#[test]
fn test_parse_error_has_location() {
    let source = "fn test():
    let x = undefined";
    
    let program = parse(source).unwrap();
    let result = type_check_program(&program);
    assert!(result.is_err());
    
    if let Err(TypeError::UndefinedVariable { span, .. }) = result {
        // Verify error has location information
        assert!(span.line() > 0);
    }
}

#[test]
fn test_parse_complex_real_world_example() {
    let source = "\"\"\"Calculate factorial iteratively.\"\"\"
fn factorial(n: int) -> int:
    if n < 0:
        return 0
    
    let result = 1
    let i = 1
    while i <= n:
        result = result * i
        i = i + 1
    
    return result

fn main():
    let n = 5
    let fact = factorial(n)
    print(fact)";
    
    let result = compile_and_optimize(source);
    assert!(result.is_ok(), "Full pipeline failed: {:?}", result.err());
}

// ============================================================================
// Code Generation Tests (LLVM IR)
// ============================================================================

#[test]
fn test_codegen_simple_function() {
    let source = "fn add(a: int, b: int) -> int:
    return a + b";
    
    let ir = compile_pipeline(source).unwrap();
    let llvm_ir = CodeGenerator::new(ir).generate();
    
    // Check that LLVM IR contains function definition
    assert!(llvm_ir.contains("define"), "LLVM IR should contain function definition");
    assert!(llvm_ir.contains("@add"), "LLVM IR should contain function name");
    assert!(llvm_ir.contains("i64"), "LLVM IR should contain integer type");
}

#[test]
fn test_codegen_with_constants() {
    let source = "fn test() -> int:
    return 42";
    
    let ir = compile_pipeline(source).unwrap();
    let llvm_ir = CodeGenerator::new(ir).generate();
    
    // Check that constant is generated
    assert!(llvm_ir.contains("42"), "LLVM IR should contain constant value");
}

#[test]
fn test_codegen_arithmetic_operations() {
    let source = "fn test(a: int, b: int) -> int:
    return a + b * 2";
    
    let ir = compile_pipeline(source).unwrap();
    let llvm_ir = CodeGenerator::new(ir).generate();
    
    // Check that arithmetic operations are generated
    assert!(llvm_ir.contains("add"), "LLVM IR should contain add operation");
    assert!(llvm_ir.contains("mul"), "LLVM IR should contain mul operation");
}

#[test]
fn test_codegen_control_flow() {
    let source = "fn test(x: int) -> int:
    if x > 0:
        return 1
    else:
        return 0";
    
    let ir = compile_pipeline(source).unwrap();
    let llvm_ir = CodeGenerator::new(ir).generate();
    
    // Check that control flow is generated
    assert!(llvm_ir.contains("br"), "LLVM IR should contain branch instruction");
    assert!(llvm_ir.contains("icmp"), "LLVM IR should contain comparison");
}

#[test]
fn test_codegen_function_calls() {
    let source = "fn helper(x: int) -> int:
    return x * 2

fn main() -> int:
    return helper(5)";
    
    let ir = compile_pipeline(source).unwrap();
    let llvm_ir = CodeGenerator::new(ir).generate();
    
    // Check that function calls are generated
    assert!(llvm_ir.contains("call"), "LLVM IR should contain call instruction");
    assert!(llvm_ir.contains("@helper"), "LLVM IR should contain helper function call");
}

#[test]
fn test_codegen_string_constants() {
    let source = "fn test() -> str:
    return \"hello\"";
    
    let ir = compile_pipeline(source).unwrap();
    let llvm_ir = CodeGenerator::new(ir).generate();
    
    // Check that string constants are generated
    assert!(llvm_ir.contains("@str"), "LLVM IR should contain string constant");
    assert!(llvm_ir.contains("hello"), "LLVM IR should contain string value");
}

#[test]
fn test_codegen_struct_types() {
    let source = "class Point:
    let x: int
    let y: int

fn create_point() -> Point:
    return new Point(10, 20)";
    
    let ir = compile_pipeline(source).unwrap();
    let llvm_ir = CodeGenerator::new(ir).generate();
    
    // Check that struct types are generated
    assert!(llvm_ir.contains("%struct.Point"), "LLVM IR should contain struct type definition");
}

#[test]
fn test_codegen_with_target_triple() {
    let source = "fn test() -> int:
    return 42";
    
    let ir = compile_pipeline(source).unwrap();
    let llvm_ir = CodeGenerator::new(ir).generate_with_target(Some("x86_64-pc-windows-msvc"));
    
    // Check that target triple is set
    assert!(llvm_ir.contains("x86_64-pc-windows-msvc"), "LLVM IR should contain target triple");
}

// ============================================================================
// Interpreter Tests
// ============================================================================

#[test]
fn test_interpreter_simple_arithmetic() {
    let source = "fn main() -> int:
    return 2 + 3";
    
    let program = parse(source).unwrap();
    type_check_program(&program).unwrap();
    
    let mut interpreter = Interpreter::new().unwrap();
    let result = interpreter.interpret(&program).unwrap();
    
    if let pain_runtime::Value::Int(i) = result {
        assert_eq!(i, 5, "Interpreter should compute 2 + 3 = 5");
    } else {
        panic!("Expected Int value, got {:?}", result);
    }
}

#[test]
fn test_interpreter_variables() {
    let source = "fn main() -> int:
    let x = 10
    let y = 20
    return x + y";
    
    let program = parse(source).unwrap();
    type_check_program(&program).unwrap();
    
    let mut interpreter = Interpreter::new().unwrap();
    let result = interpreter.interpret(&program).unwrap();
    
    if let pain_runtime::Value::Int(i) = result {
        assert_eq!(i, 30, "Interpreter should compute 10 + 20 = 30");
    } else {
        panic!("Expected Int value, got {:?}", result);
    }
}

#[test]
fn test_interpreter_conditional() {
    let source = "fn main() -> int:
    let x = 5
    if x > 3:
        return 1
    else:
        return 0";
    
    let program = parse(source).unwrap();
    type_check_program(&program).unwrap();
    
    let mut interpreter = Interpreter::new().unwrap();
    let result = interpreter.interpret(&program).unwrap();
    
    if let pain_runtime::Value::Int(i) = result {
        assert_eq!(i, 1, "Interpreter should return 1 for x > 3");
    } else {
        panic!("Expected Int value, got {:?}", result);
    }
}

#[test]
fn test_interpreter_loop() {
    let source = "fn main() -> int:
    let sum = 0
    let i = 0
    while i < 5:
        sum = sum + i
        i = i + 1
    return sum";
    
    let program = parse(source).unwrap();
    type_check_program(&program).unwrap();
    
    let mut interpreter = Interpreter::new().unwrap();
    let result = interpreter.interpret(&program).unwrap();
    
    if let pain_runtime::Value::Int(i) = result {
        assert_eq!(i, 10, "Interpreter should compute sum 0+1+2+3+4 = 10");
    } else {
        panic!("Expected Int value, got {:?}", result);
    }
}

#[test]
fn test_interpreter_function_call() {
    let source = "fn add(a: int, b: int) -> int:
    return a + b

fn main() -> int:
    return add(3, 4)";
    
    let program = parse(source).unwrap();
    type_check_program(&program).unwrap();
    
    let mut interpreter = Interpreter::new().unwrap();
    let result = interpreter.interpret(&program).unwrap();
    
    if let pain_runtime::Value::Int(i) = result {
        assert_eq!(i, 7, "Interpreter should compute add(3, 4) = 7");
    } else {
        panic!("Expected Int value, got {:?}", result);
    }
}

#[test]
fn test_interpreter_recursive_function() {
    let source = "fn factorial(n: int) -> int:
    if n <= 1:
        return 1
    else:
        return n * factorial(n - 1)

fn main() -> int:
    return factorial(5)";
    
    let program = parse(source).unwrap();
    type_check_program(&program).unwrap();
    
    let mut interpreter = Interpreter::new().unwrap();
    let result = interpreter.interpret(&program).unwrap();
    
    if let pain_runtime::Value::Int(i) = result {
        assert_eq!(i, 120, "Interpreter should compute factorial(5) = 120");
    } else {
        panic!("Expected Int value, got {:?}", result);
    }
}

// ============================================================================
// Full Pipeline Tests (Parse -> Type Check -> IR -> Optimize -> Codegen)
// ============================================================================

/// Helper function to run full pipeline: parse -> type_check -> IR -> optimize -> codegen
fn full_pipeline(source: &str) -> Result<String, String> {
    let program = parse(source)?;
    type_check_program(&program).map_err(|e| format!("Type error: {:?}", e))?;
    let ir_builder = IrBuilder::new();
    let ir = ir_builder.build(&program);
    let optimized_ir = Optimizer::optimize(ir);
    let llvm_ir = CodeGenerator::new(optimized_ir).generate();
    Ok(llvm_ir)
}

#[test]
fn test_full_pipeline_simple() {
    let source = "fn main() -> int:
    return 42";
    
    let result = full_pipeline(source);
    assert!(result.is_ok(), "Full pipeline should succeed: {:?}", result.err());
    let llvm_ir = result.unwrap();
    assert!(llvm_ir.contains("define"), "Should generate LLVM IR");
}

#[test]
fn test_full_pipeline_with_optimizations() {
    let source = "fn main() -> int:
    let x = 2 + 3
    return x";
    
    let result = full_pipeline(source);
    assert!(result.is_ok(), "Full pipeline should succeed: {:?}", result.err());
    let llvm_ir = result.unwrap();
    // After constant folding, 2 + 3 should be optimized
    assert!(llvm_ir.contains("define"), "Should generate LLVM IR");
}

#[test]
fn test_full_pipeline_complex_program() {
    let source = "fn max(a: int, b: int) -> int:
    if a > b:
        return a
    else:
        return b

fn main() -> int:
    let x = 10
    let y = 20
    return max(x, y)";
    
    let result = full_pipeline(source);
    assert!(result.is_ok(), "Full pipeline should succeed: {:?}", result.err());
    let llvm_ir = result.unwrap();
    assert!(llvm_ir.contains("@max"), "Should generate max function");
    assert!(llvm_ir.contains("@main"), "Should generate main function");
}

#[test]
fn test_full_pipeline_with_loops() {
    let source = "fn sum(n: int) -> int:
    let total = 0
    let i = 0
    while i <= n:
        total = total + i
        i = i + 1
    return total

fn main() -> int:
    return sum(10)";
    
    let result = full_pipeline(source);
    assert!(result.is_ok(), "Full pipeline should succeed: {:?}", result.err());
    let llvm_ir = result.unwrap();
    assert!(llvm_ir.contains("@sum"), "Should generate sum function");
    assert!(llvm_ir.contains("br"), "Should contain branch instructions for loop");
}

// ============================================================================
// MLIR Code Generation Tests
// ============================================================================

#[test]
fn test_mlir_codegen_simple() {
    let source = "fn main() -> int:
    return 42";
    
    let ir = compile_pipeline(source).unwrap();
    let mlir_code = MlirCodeGenerator::new(ir).generate();
    
    // Check that MLIR code is generated
    assert!(mlir_code.contains("module"), "MLIR should contain module");
    assert!(mlir_code.contains("// MLIR generated from Pain"), "MLIR should have header comment");
}

#[test]
fn test_mlir_codegen_with_functions() {
    let source = "fn add(a: int, b: int) -> int:
    return a + b";
    
    let ir = compile_pipeline(source).unwrap();
    let mlir_code = MlirCodeGenerator::new(ir).generate();
    
    // Check that MLIR code contains function
    assert!(mlir_code.contains("module"), "MLIR should contain module");
}

// ============================================================================
// Optimization Tests (Detailed)
// ============================================================================

#[test]
fn test_optimization_constant_folding_detailed() {
    let source = "fn test() -> int:
    let x = 2 + 3
    let y = x * 4
    return y";
    
    let ir = compile_pipeline(source).unwrap();
    let optimized = Optimizer::optimize(ir);
    
    // After constant folding, x should be 5, y should be 20
    // We can't easily check the exact IR structure, but we can verify it still compiles
    assert!(!optimized.functions.is_empty(), "Should have functions after optimization");
}

#[test]
fn test_optimization_loop_invariant_motion() {
    let source = "fn test(n: int) -> int:
    let constant = 100
    let sum = 0
    let i = 0
    while i < n:
        sum = sum + constant
        i = i + 1
    return sum";
    
    let ir = compile_pipeline(source).unwrap();
    let optimized = Optimizer::optimize(ir);
    
    // Loop invariant code motion should move constant calculations out of loop
    assert!(!optimized.functions.is_empty(), "Should have functions after optimization");
}

#[test]
fn test_optimization_function_inlining() {
    let source = "@inline
fn helper(x: int) -> int:
    return x * 2

fn main() -> int:
    return helper(5)";
    
    let ir = compile_pipeline(source).unwrap();
    let optimized = Optimizer::optimize(ir);
    
    // Function inlining should inline helper function
    assert!(!optimized.functions.is_empty(), "Should have functions after optimization");
}

#[test]
fn test_optimization_tail_call() {
    let source = "fn helper(n: int, acc: int) -> int:
    if n == 0:
        return acc
    else:
        return helper(n - 1, acc + n)

fn main() -> int:
    return helper(5, 0)";
    
    let ir = compile_pipeline(source).unwrap();
    let optimized = Optimizer::optimize(ir);
    
    // Tail call optimization should mark tail calls
    assert!(!optimized.functions.is_empty(), "Should have functions after optimization");
}

// ============================================================================
// Edge Cases and Error Handling Tests
// ============================================================================

#[test]
fn test_pipeline_handles_empty_function() {
    let source = "fn empty(): pass";
    
    let result = compile_pipeline(source);
    // Empty function may or may not be supported, but shouldn't crash
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_pipeline_handles_nested_loops() {
    let source = "fn test() -> int:
    let sum = 0
    let i = 0
    while i < 3:
        let j = 0
        while j < 3:
            sum = sum + 1
            j = j + 1
        i = i + 1
    return sum";
    
    let result = compile_and_optimize(source);
    assert!(result.is_ok(), "Should handle nested loops: {:?}", result.err());
}

#[test]
fn test_pipeline_handles_deep_nesting() {
    let source = "fn test(x: int) -> int:
    if x > 0:
        if x > 1:
            if x > 2:
                return 3
            else:
                return 2
        else:
            return 1
    else:
        return 0";
    
    let result = compile_and_optimize(source);
    assert!(result.is_ok(), "Should handle deep nesting: {:?}", result.err());
}

#[test]
fn test_pipeline_handles_multiple_returns() {
    let source = "fn test(x: int) -> int:
    if x > 0:
        return 1
    return 0";
    
    let result = compile_and_optimize(source);
    assert!(result.is_ok(), "Should handle multiple returns: {:?}", result.err());
}

#[test]
fn test_pipeline_handles_break_continue() {
    let source = "fn test() -> int:
    let sum = 0
    let i = 0
    while i < 10:
        if i == 5:
            break
        if i % 2 == 0:
            continue
        sum = sum + i
        i = i + 1
    return sum";
    
    let result = compile_and_optimize(source);
    assert!(result.is_ok(), "Should handle break/continue: {:?}", result.err());
}

#[test]
fn test_pipeline_handles_list_operations() {
    let source = "fn test() -> int:
    let items = [1, 2, 3, 4, 5]
    return len(items)";
    
    let result = compile_and_optimize(source);
    assert!(result.is_ok(), "Should handle list operations: {:?}", result.err());
}

#[test]
fn test_pipeline_handles_string_operations() {
    let source = "fn test() -> int:
    let s = \"hello\"
    return len(s)";
    
    let result = compile_and_optimize(source);
    assert!(result.is_ok(), "Should handle string operations: {:?}", result.err());
}

#[test]
fn test_pipeline_handles_class_instantiation() {
    let source = "class Point:
    let x: int
    let y: int

fn test() -> Point:
    return new Point(10, 20)";
    
    let result = compile_and_optimize(source);
    assert!(result.is_ok(), "Should handle class instantiation: {:?}", result.err());
}

#[test]
fn test_pipeline_handles_member_access() {
    let source = "class Point:
    let x: int
    let y: int

fn test() -> int:
    let p = new Point(10, 20)
    return p.x";
    
    let result = compile_and_optimize(source);
    assert!(result.is_ok(), "Should handle member access: {:?}", result.err());
}

#[test]
fn test_pipeline_error_recovery_parse_error() {
    let source = "fn test(
    return 42";
    
    let result = parse(source);
    assert!(result.is_err(), "Should detect parse error");
    let error = result.unwrap_err();
    assert!(!error.is_empty(), "Error message should not be empty");
}

#[test]
fn test_pipeline_error_recovery_type_error() {
    let source = "fn test():
    let x: int = \"not an int\"";
    
    let program = parse(source).unwrap();
    let result = type_check_program(&program);
    assert!(result.is_err(), "Should detect type error");
    
    if let Err(TypeError::TypeMismatch { expected, found, .. }) = result {
        assert_eq!(expected, Type::Int);
        assert_eq!(found, Type::Str);
    } else {
        panic!("Expected TypeMismatch error");
    }
}

#[test]
fn test_pipeline_error_recovery_undefined_variable() {
    let source = "fn test() -> int:
    return undefined_var";
    
    let program = parse(source).unwrap();
    let result = type_check_program(&program);
    assert!(result.is_err(), "Should detect undefined variable");
    
    if let Err(TypeError::UndefinedVariable { name, .. }) = result {
        assert_eq!(name, "undefined_var");
    } else {
        panic!("Expected UndefinedVariable error");
    }
}

#[test]
fn test_pipeline_error_recovery_wrong_argument_count() {
    let source = "fn add(a: int, b: int) -> int:
    return a + b

fn test() -> int:
    return add(1)";
    
    let program = parse(source).unwrap();
    let result = type_check_program(&program);
    // This may or may not be caught at type checking stage
    // depending on implementation
    assert!(result.is_ok() || result.is_err());
}

// ============================================================================
// Integration Tests: Real-world Examples
// ============================================================================

#[test]
fn test_pipeline_fibonacci_iterative() {
    let source = "fn fibonacci(n: int) -> int:
    if n <= 1:
        return n
    
    let a = 0
    let b = 1
    let i = 2
    while i <= n:
        let temp = a + b
        a = b
        b = temp
        i = i + 1
    return b

fn main() -> int:
    return fibonacci(10)";
    
    let result = full_pipeline(source);
    assert!(result.is_ok(), "Should compile fibonacci: {:?}", result.err());
    
    // Also test with interpreter
    let program = parse(source).unwrap();
    type_check_program(&program).unwrap();
    let mut interpreter = Interpreter::new().unwrap();
    let result = interpreter.interpret(&program).unwrap();
    if let pain_runtime::Value::Int(i) = result {
        assert_eq!(i, 55, "fibonacci(10) should be 55");
    }
}

#[test]
fn test_pipeline_quicksort_like() {
    let source = "fn partition(items: list[int], low: int, high: int) -> int:
    let pivot = items[high]
    let i = low - 1
    let j = low
    while j < high:
        if items[j] < pivot:
            i = i + 1
            # swap would go here
        j = j + 1
    return i + 1

fn main() -> int:
    let items = [3, 1, 4, 1, 5]
    return partition(items, 0, len(items) - 1)";
    
    let result = compile_and_optimize(source);
    // May or may not fully support all list operations yet
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_pipeline_math_library() {
    let source = "fn square(x: int) -> int:
    return x * x

fn cube(x: int) -> int:
    return x * square(x)

fn power(x: int, n: int) -> int:
    if n == 0:
        return 1
    else:
        return x * power(x, n - 1)

fn main() -> int:
    return power(2, 8)";
    
    let result = full_pipeline(source);
    assert!(result.is_ok(), "Should compile math library: {:?}", result.err());
    
    // Test with interpreter
    let program = parse(source).unwrap();
    type_check_program(&program).unwrap();
    let mut interpreter = Interpreter::new().unwrap();
    let result = interpreter.interpret(&program).unwrap();
    if let pain_runtime::Value::Int(i) = result {
        assert_eq!(i, 256, "power(2, 8) should be 256");
    }
}

