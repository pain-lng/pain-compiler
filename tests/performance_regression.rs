// Performance regression tests
// These tests ensure that compiler performance doesn't degrade over time

use pain_compiler::*;
use std::time::{Duration, Instant};

/// Number of iterations for performance measurements (to average out noise)
const PERF_ITERATIONS: u32 = 5;

/// Helper to measure compilation time with averaging
fn measure_compile_time(source: &str) -> Duration {
    let mut total = Duration::ZERO;
    for _ in 0..PERF_ITERATIONS {
        let start = Instant::now();
        let _ = parse(source);
        total += start.elapsed();
    }
    total / PERF_ITERATIONS
}

/// Helper to measure full pipeline time with averaging
fn measure_full_pipeline_time(source: &str) -> Duration {
    let mut total = Duration::ZERO;
    let mut success_count = 0;

    for _ in 0..PERF_ITERATIONS {
        let start = Instant::now();
        let program = match parse(source) {
            Ok(p) => p,
            Err(_) => continue,
        };
        let _ = type_check_program(&program);
        let ir_builder = IrBuilder::new();
        let ir = ir_builder.build(&program);
        let _ = Optimizer::optimize(ir);
        total += start.elapsed();
        success_count += 1;
    }

    if success_count == 0 {
        Duration::ZERO
    } else {
        total / success_count
    }
}

/// Helper to measure a specific operation with averaging
fn measure_operation<F>(mut op: F, iterations: u32) -> Duration
where
    F: FnMut(),
{
    let mut total = Duration::ZERO;
    for _ in 0..iterations {
        let start = Instant::now();
        op();
        total += start.elapsed();
    }
    total / iterations
}

#[test]
fn test_parse_performance_simple() {
    let source = "fn add(a: int, b: int) -> int:\n    return a + b";
    let duration = measure_compile_time(source);

    // Simple parse should be very fast (< 10ms average)
    // Allow 2x margin for CI environments
    assert!(
        duration < Duration::from_millis(20),
        "Simple parse took too long: {:?} (average over {} iterations)",
        duration,
        PERF_ITERATIONS
    );
}

#[test]
fn test_parse_performance_large_function() {
    // Generate a large function with many statements
    let mut source = String::from("fn large() -> int:\n");
    for i in 0..100 {
        source.push_str(&format!("    let x{} = {}\n", i, i));
    }
    source.push_str("    return 0");

    let duration = measure_compile_time(&source);

    // Large function should still parse reasonably fast (< 200ms with margin)
    assert!(
        duration < Duration::from_millis(200),
        "Large function parse took too long: {:?} (average over {} iterations)",
        duration,
        PERF_ITERATIONS
    );
}

#[test]
fn test_type_check_performance() {
    let mut source = String::from("fn test() -> int:\n");
    for i in 0..50 {
        source.push_str(&format!("    let x{}: int = {}\n", i, i));
    }
    source.push_str("    return 0");

    let program = parse(source).unwrap();
    let duration = measure_operation(
        || {
            let _ = type_check_program(&program);
        },
        PERF_ITERATIONS,
    );

    // Type checking should be fast (< 100ms for 50 variables with margin)
    assert!(
        duration < Duration::from_millis(100),
        "Type check took too long: {:?} (average over {} iterations)",
        duration,
        PERF_ITERATIONS
    );
}

#[test]
fn test_full_pipeline_performance_simple() {
    let source = "fn main() -> int:\n    return 42";
    let duration = measure_full_pipeline_time(source);

    // Full pipeline for simple program should be fast (< 100ms with margin)
    assert!(
        duration < Duration::from_millis(100),
        "Full pipeline took too long: {:?} (average over {} iterations)",
        duration,
        PERF_ITERATIONS
    );
}

#[test]
fn test_full_pipeline_performance_fibonacci() {
    let source = "fn fib(n: int) -> int:
    if n <= 1:
        return n
    else:
        return fib(n - 1) + fib(n - 2)

fn main() -> int:
    return fib(10)";

    let duration = measure_full_pipeline_time(source);

    // Fibonacci should compile reasonably fast (< 400ms with margin)
    assert!(
        duration < Duration::from_millis(400),
        "Fibonacci compilation took too long: {:?} (average over {} iterations)",
        duration,
        PERF_ITERATIONS
    );
}

#[test]
fn test_ir_build_performance() {
    let mut source = String::from("fn test() -> int:\n");
    for i in 0..100 {
        source.push_str(&format!("    let x{} = {}\n", i, i));
    }
    source.push_str("    return 0");

    let program = parse(source).unwrap();
    type_check_program(&program).unwrap();

    let duration = measure_operation(
        || {
            let ir_builder = IrBuilder::new();
            let _ = ir_builder.build(&program);
        },
        PERF_ITERATIONS,
    );

    // IR build should be fast (< 200ms with margin)
    assert!(
        duration < Duration::from_millis(200),
        "IR build took too long: {:?} (average over {} iterations)",
        duration,
        PERF_ITERATIONS
    );
}

#[test]
fn test_optimization_performance() {
    let source = "fn test(n: int) -> int:
    let sum = 0
    let i = 0
    while i < n:
        sum = sum + i
        i = i + 1
    return sum";

    let program = parse(source).unwrap();
    type_check_program(&program).unwrap();
    let ir_builder = IrBuilder::new();
    let ir = ir_builder.build(&program);

    let duration = measure_operation(
        || {
            let _ = Optimizer::optimize(ir.clone());
        },
        PERF_ITERATIONS,
    );

    // Optimization should be fast (< 200ms with margin)
    assert!(
        duration < Duration::from_millis(200),
        "Optimization took too long: {:?} (average over {} iterations)",
        duration,
        PERF_ITERATIONS
    );
}

#[test]
fn test_codegen_performance() {
    let source = "fn add(a: int, b: int) -> int:
    return a + b

fn main() -> int:
    return add(1, 2)";

    let program = parse(source).unwrap();
    type_check_program(&program).unwrap();
    let ir_builder = IrBuilder::new();
    let ir = ir_builder.build(&program);
    let optimized = Optimizer::optimize(ir);

    let duration = measure_operation(
        || {
            let _ = CodeGenerator::new(optimized.clone()).generate();
        },
        PERF_ITERATIONS,
    );

    // Code generation should be fast (< 200ms with margin)
    assert!(
        duration < Duration::from_millis(200),
        "Code generation took too long: {:?} (average over {} iterations)",
        duration,
        PERF_ITERATIONS
    );
}

#[test]
fn test_parse_performance_nested_blocks() {
    let mut source = String::from("fn test() -> int:\n");
    for i in 0..10 {
        let indent = "    ".repeat(i + 1);
        source.push_str(&format!("{}if True:\n", indent));
    }
    source.push_str("                return 0");

    let duration = measure_compile_time(&source);

    // Nested blocks should parse reasonably fast (< 100ms with margin)
    assert!(
        duration < Duration::from_millis(100),
        "Nested blocks parse took too long: {:?} (average over {} iterations)",
        duration,
        PERF_ITERATIONS
    );
}

#[test]
fn test_memory_usage_simple() {
    // Test that simple programs don't use excessive memory
    let source = "fn main() -> int:\n    return 42";

    let program = parse(source).unwrap();
    let _ = type_check_program(&program);
    let ir_builder = IrBuilder::new();
    let ir = ir_builder.build(&program);

    // Check that IR doesn't have excessive functions/blocks
    assert!(
        ir.functions.len() <= 10,
        "Too many functions in IR: {}",
        ir.functions.len()
    );
}

#[test]
fn test_parse_performance_string_literals() {
    // Test parsing with many string literals
    let mut source = String::from("fn test() -> str:\n");
    for i in 0..50 {
        source.push_str(&format!("    let s{} = \"string{}\"\n", i, i));
    }
    source.push_str("    return \"done\"");

    let duration = measure_compile_time(&source);

    // String literals should parse fast (< 200ms with margin)
    assert!(
        duration < Duration::from_millis(200),
        "String literals parse took too long: {:?} (average over {} iterations)",
        duration,
        PERF_ITERATIONS
    );
}

/// Test that performance doesn't degrade significantly with many functions
#[test]
fn test_parse_performance_many_functions() {
    let mut source = String::new();
    for i in 0..50 {
        source.push_str(&format!("fn func{}() -> int:\n    return {}\n\n", i, i));
    }

    let duration = measure_compile_time(&source);

    // Many functions should parse reasonably fast (< 300ms with margin)
    assert!(
        duration < Duration::from_millis(300),
        "Many functions parse took too long: {:?} (average over {} iterations)",
        duration,
        PERF_ITERATIONS
    );
}

/// Test that full pipeline scales reasonably with program size
#[test]
fn test_full_pipeline_scalability() {
    let mut source = String::from("fn helper(x: int) -> int:\n    return x * 2\n\n");
    source.push_str("fn main() -> int:\n    let sum = 0\n    let i = 0\n");
    source.push_str("    while i < 100:\n        sum = sum + helper(i)\n        i = i + 1\n");
    source.push_str("    return sum");

    let duration = measure_full_pipeline_time(&source);

    // Larger program should still compile reasonably fast (< 500ms with margin)
    assert!(
        duration < Duration::from_millis(500),
        "Scalability test took too long: {:?} (average over {} iterations)",
        duration,
        PERF_ITERATIONS
    );
}
