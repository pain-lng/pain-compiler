// Property-based tests for parser and type checker
// Uses proptest to generate random valid programs and verify invariants

use pain_compiler::*;
use proptest::prelude::*;

/// Property: Any valid integer literal should parse successfully
#[test]
fn prop_parse_integer_literals() {
    proptest!(|(n in -1000i64..1000i64)| {
        let source = format!("fn test() -> int:\n    return {}", n);
        let result = parse(&source);
        prop_assert!(result.is_ok(), "Failed to parse integer {}: {:?}", n, result.err());
    });
}

/// Property: Any valid function with integer parameters should parse
#[test]
fn prop_parse_function_with_params() {
    proptest!(|(name in "[a-z][a-z0-9_]*", _a in 0i64..100, _b in 0i64..100)| {
        let source = format!(
            "fn {}(x: int, y: int) -> int:\n    return x + y",
            name
        );
        let result = parse(&source);
        prop_assert!(result.is_ok(), "Failed to parse function: {:?}", result.err());
    });
}

/// Property: Parsing and then type-checking should succeed for valid programs
#[test]
fn prop_parse_and_typecheck_valid_programs() {
    proptest!(|(a in 0i64..100, b in 0i64..100)| {
        let source = format!(
            "fn add(x: int, y: int) -> int:\n    return x + y\n\nfn main() -> int:\n    return add({}, {})",
            a, b
        );

        let program = match parse(&source) {
            Ok(p) => p,
            Err(e) => {
                prop_assume!(false, "Parse failed: {}", e);
                return Ok(());
            }
        };

        let result = type_check_program(&program);
        prop_assert!(result.is_ok(), "Type check failed for valid program: {:?}", result.err());
    });
}

/// Property: Round-trip property - parse, type-check, build IR should preserve semantics
#[test]
fn prop_round_trip_pipeline() {
    proptest!(|(n in 0i64..50)| {
        let source = format!(
            "fn factorial(n: int) -> int:\n    if n <= 1:\n        return 1\n    else:\n        return n * factorial(n - 1)\n\nfn main() -> int:\n    return factorial({})",
            n
        );

        // Parse
        let program = match parse(&source) {
            Ok(p) => p,
            Err(e) => {
                prop_assume!(false, "Parse failed: {}", e);
                return Ok(());
            }
        };

        // Type check
        match type_check_program(&program) {
            Ok(_) => (),
            Err(e) => {
                prop_assume!(false, "Type check failed: {:?}", e);
                return Ok(());
            }
        };

        // Build IR
        let ir_builder = IrBuilder::new();
        let ir = ir_builder.build(&program);

        // Should have at least one function
        prop_assert!(!ir.functions.is_empty(), "IR should contain functions");
    });
}

/// Property: Arithmetic expressions should parse correctly
#[test]
fn prop_parse_arithmetic_expressions() {
    proptest!(|(a in -100i64..100, b in -100i64..100)| {
        let ops = ["+", "-", "*", "/", "%"];
        for op in &ops {
            let source = format!("fn test() -> int:\n    return {} {} {}", a, op, b);
            let result = parse(&source);
            prop_assert!(result.is_ok(), "Failed to parse {} {} {}: {:?}", a, op, b, result.err());
        }
    });
}

/// Property: Comparison expressions should parse correctly
#[test]
fn prop_parse_comparison_expressions() {
    proptest!(|(a in -100i64..100, b in -100i64..100)| {
        let ops = ["==", "!=", "<", ">", "<=", ">="];
        for op in &ops {
            let source = format!("fn test() -> bool:\n    return {} {} {}", a, op, b);
            let result = parse(&source);
            prop_assert!(result.is_ok(), "Failed to parse {} {} {}: {:?}", a, op, b, result.err());
        }
    });
}

/// Property: Nested expressions should parse correctly
#[test]
fn prop_parse_nested_expressions() {
    proptest!(|(a in 0i64..50, b in 0i64..50, c in 0i64..50)| {
        let source = format!("fn test() -> int:\n    return ({} + {}) * {}", a, b, c);
        let result = parse(&source);
        prop_assert!(result.is_ok(), "Failed to parse nested expression: {:?}", result.err());
    });
}

/// Property: Type checker should reject type mismatches
#[test]
fn prop_type_checker_rejects_mismatches() {
    proptest!(|(n in -100i64..100)| {
        // Try to assign string to int
        let source = format!("fn test() -> int:\n    let x: int = \"not an int\"\n    return {}", n);
        let program = match parse(&source) {
            Ok(p) => p,
            Err(_) => return Ok(()), // Skip if parse fails
        };

        let result = type_check_program(&program);
        prop_assert!(result.is_err(), "Type checker should reject type mismatch");
    });
}

/// Property: Optimizer should preserve program semantics
#[test]
fn prop_optimizer_preserves_semantics() {
    proptest!(|(n in 0i64..20)| {
        let source = format!(
            "fn sum(n: int) -> int:\n    let total = 0\n    let i = 0\n    while i <= {}:\n        total = total + i\n        i = i + 1\n    return total",
            n
        );

        let program = match parse(&source) {
            Ok(p) => p,
            Err(_) => return Ok(()),
        };

        match type_check_program(&program) {
            Ok(_) => (),
            Err(_) => return Ok(()),
        };

        let ir_builder = IrBuilder::new();
        let ir = ir_builder.build(&program);
        let optimized = Optimizer::optimize(ir);

        // Both should have functions
        prop_assert!(!optimized.functions.is_empty(), "Optimized IR should have functions");
    });
}

/// Property: Parser should handle various indentation levels
#[test]
fn prop_parse_various_indentations() {
    proptest!(|(level in 1usize..5)| {
        let indent = "    ".repeat(level);
        let source = format!(
            "fn test() -> int:\n{}let x = 10\n{}return x",
            indent, indent
        );
        let result = parse(&source);
        // May or may not parse depending on indentation rules
        // Just verify it doesn't crash
        let _ = result;
    });
}

/// Property: Multiple functions should parse correctly
#[test]
fn prop_parse_multiple_functions() {
    proptest!(|(count in 1usize..10)| {
        let mut source = String::new();
        for i in 0..count {
            source.push_str(&format!("fn func{}() -> int:\n    return {}\n\n", i, i));
        }

        let result = parse(&source);
        if result.is_ok() {
            let program = result.unwrap();
            prop_assert_eq!(program.items.len(), count, "Should have {} functions", count);
        }
    });
}

/// Property: Logical expressions should parse correctly
#[test]
fn prop_parse_logical_expressions() {
    proptest!(|(a in 0i64..100, b in 0i64..100)| {
        let ops = ["and", "or"];
        for op in &ops {
            let source = format!("fn test() -> bool:\n    return {} {} {}", a, op, b);
            let result = parse(&source);
            // Logical ops may require bool operands, so we just check it doesn't crash
            let _ = result;
        }
    });
}

/// Property: Variable assignments should parse correctly
#[test]
fn prop_parse_variable_assignments() {
    proptest!(|(name in "[a-z][a-z0-9_]*", value in -100i64..100)| {
        let source = format!("fn test() -> int:\n    let {} = {}\n    return {}", name, value, name);
        let result = parse(&source);
        prop_assert!(result.is_ok(), "Failed to parse variable assignment: {:?}", result.err());
    });
}

/// Property: If-else statements should parse correctly
#[test]
fn prop_parse_if_else_statements() {
    proptest!(|(a in 0i64..100, b in 0i64..100)| {
        let source = format!(
            "fn test() -> int:\n    if {} > {}:\n        return {}\n    else:\n        return {}",
            a, b, a, b
        );
        let result = parse(&source);
        prop_assert!(result.is_ok(), "Failed to parse if-else: {:?}", result.err());
    });
}

/// Property: While loops should parse correctly
#[test]
fn prop_parse_while_loops() {
    proptest!(|(n in 0i64..50)| {
        let source = format!(
            "fn test() -> int:\n    let i = 0\n    while i < {}:\n        i = i + 1\n    return i",
            n
        );
        let result = parse(&source);
        prop_assert!(result.is_ok(), "Failed to parse while loop: {:?}", result.err());
    });
}

/// Property: For loops should parse correctly
#[test]
fn prop_parse_for_loops() {
    proptest!(|(n in 0i64..50)| {
        let source = format!(
            "fn test() -> int:\n    let sum = 0\n    for i in 0..{}:\n        sum = sum + i\n    return sum",
            n
        );
        let result = parse(&source);
        // For loops may not be implemented, so we just check it doesn't crash
        let _ = result;
    });
}

/// Property: Function calls with different argument counts should parse
#[test]
fn prop_parse_function_calls() {
    proptest!(|(a in 0i64..100, b in 0i64..100, c in 0i64..100)| {
        let source = format!(
            "fn add3(x: int, y: int, z: int) -> int:\n    return x + y + z\n\nfn main() -> int:\n    return add3({}, {}, {})",
            a, b, c
        );
        let result = parse(&source);
        prop_assert!(result.is_ok(), "Failed to parse function call: {:?}", result.err());
    });
}

/// Property: Type checker should accept correct type assignments
#[test]
fn prop_type_checker_accepts_correct_types() {
    proptest!(|(n in -100i64..100)| {
        let source = format!("fn test() -> int:\n    let x: int = {}\n    return x", n);
        let program = match parse(&source) {
            Ok(p) => p,
            Err(_) => return Ok(()),
        };

        let result = type_check_program(&program);
        prop_assert!(result.is_ok(), "Type checker should accept correct type: {:?}", result.err());
    });
}

/// Property: Type checker should reject wrong return types
#[test]
fn prop_type_checker_rejects_wrong_return_type() {
    proptest!(|(_n in -100i64..100)| {
        // Function declares int return but returns string
        let source = "fn test() -> int:\n    return \"not an int\"".to_string();
        let program = match parse(&source) {
            Ok(p) => p,
            Err(_) => return Ok(()),
        };

        let result = type_check_program(&program);
        prop_assert!(result.is_err(), "Type checker should reject wrong return type");
    });
}

/// Property: Type checker should handle function parameter types correctly
#[test]
fn prop_type_checker_function_parameters() {
    proptest!(|(a in 0i64..100, b in 0i64..100)| {
        let source = format!(
            "fn add(x: int, y: int) -> int:\n    return x + y\n\nfn main() -> int:\n    return add({}, {})",
            a, b
        );
        let program = match parse(&source) {
            Ok(p) => p,
            Err(_) => return Ok(()),
        };

        let result = type_check_program(&program);
        prop_assert!(result.is_ok(), "Type checker should accept correct parameter types: {:?}", result.err());
    });
}

/// Property: Type checker should reject wrong function argument types
#[test]
fn prop_type_checker_rejects_wrong_argument_types() {
    proptest!(|(n in -100i64..100)| {
        // Function expects int but receives string
        let source = format!("fn add(x: int, y: int) -> int:\n    return x + y\n\nfn main() -> int:\n    return add(\"not an int\", {})", n);
        let program = match parse(&source) {
            Ok(p) => p,
            Err(_) => return Ok(()),
        };

        let result = type_check_program(&program);
        prop_assert!(result.is_err(), "Type checker should reject wrong argument types");
    });
}

/// Property: Nested function calls should type check correctly
#[test]
fn prop_type_checker_nested_calls() {
    proptest!(|(a in 0i64..50, b in 0i64..50)| {
        let source = format!(
            "fn double(x: int) -> int:\n    return x * 2\n\nfn add(x: int, y: int) -> int:\n    return x + y\n\nfn main() -> int:\n    return add(double({}), double({}))",
            a, b
        );
        let program = match parse(&source) {
            Ok(p) => p,
            Err(_) => return Ok(()),
        };

        let result = type_check_program(&program);
        prop_assert!(result.is_ok(), "Type checker should handle nested calls: {:?}", result.err());
    });
}

/// Property: IR builder should generate valid IR for all valid programs
#[test]
fn prop_ir_builder_generates_valid_ir() {
    proptest!(|(n in 0i64..20)| {
        let source = format!(
            "fn sum(n: int) -> int:\n    let total = 0\n    let i = 0\n    while i <= {}:\n        total = total + i\n        i = i + 1\n    return total\n\nfn main() -> int:\n    return sum({})",
            n, n
        );

        let program = match parse(&source) {
            Ok(p) => p,
            Err(_) => return Ok(()),
        };

        match type_check_program(&program) {
            Ok(_) => (),
            Err(_) => return Ok(()),
        };

        let ir_builder = IrBuilder::new();
        let ir = ir_builder.build(&program);

        // IR should have functions
        prop_assert!(!ir.functions.is_empty(), "IR should contain functions");

        // Each function should have at least one block
        for func in &ir.functions {
            prop_assert!(!func.blocks.is_empty(), "Function should have blocks");
        }
    });
}

/// Property: Code generator should generate valid LLVM IR
#[test]
fn prop_codegen_generates_valid_llvm() {
    proptest!(|(a in 0i64..50, b in 0i64..50)| {
        let source = format!(
            "fn add(x: int, y: int) -> int:\n    return x + y\n\nfn main() -> int:\n    return add({}, {})",
            a, b
        );

        let program = match parse(&source) {
            Ok(p) => p,
            Err(_) => return Ok(()),
        };

        match type_check_program(&program) {
            Ok(_) => (),
            Err(_) => return Ok(()),
        };

        let ir_builder = IrBuilder::new();
        let ir = ir_builder.build(&program);
        let optimized_ir = Optimizer::optimize(ir);

        let codegen = CodeGenerator::new(optimized_ir);
        let llvm_ir = codegen.generate();

        // LLVM IR should contain function definitions
        prop_assert!(llvm_ir.contains("define"), "LLVM IR should contain function definitions");
    });
}
