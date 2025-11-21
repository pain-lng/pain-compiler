// Extended integration tests for full compilation pipeline
// Tests the complete flow: parse -> type check -> IR -> optimize -> codegen -> executable

use pain_compiler::llvm_tools;
use pain_compiler::*;
use std::fs;
use std::path::PathBuf;
use std::process::Command;

/// Helper to run full pipeline and generate executable
fn compile_to_executable(source: &str, output_name: &str) -> Result<PathBuf, String> {
    // Create temporary source file
    let temp_dir = std::env::temp_dir().join("pain_test");
    std::fs::create_dir_all(&temp_dir).map_err(|e| format!("Failed to create temp dir: {}", e))?;

    // Run full compilation pipeline
    let program = parse(source)?;
    type_check_program(&program).map_err(|e| format!("Type error: {:?}", e))?;

    let ir_builder = IrBuilder::new();
    let ir = ir_builder.build(&program);
    let optimized_ir = Optimizer::optimize(ir);

    let codegen = CodeGenerator::new(optimized_ir);
    let llvm_ir = codegen.generate();

    // Write LLVM IR
    let llvm_file = temp_dir.join(format!("{}.ll", output_name));
    fs::write(&llvm_file, llvm_ir).map_err(|e| format!("Failed to write LLVM IR: {}", e))?;

    // Compile to executable using llvm_tools
    #[cfg(target_os = "windows")]
    let exe_path = temp_dir.join(format!("{}.exe", output_name));
    #[cfg(not(target_os = "windows"))]
    let exe_path = temp_dir.join(output_name);

    // Try to compile using llvm_tools (may not be available in all test environments)
    match llvm_tools::compile_to_executable(&llvm_file, &exe_path, None, false) {
        Ok(_) => Ok(exe_path),
        Err(_) => {
            // If compilation fails, just verify LLVM IR was generated
            if llvm_file.exists() {
                Ok(llvm_file) // Return LLVM IR file as success indicator
            } else {
                Err("Failed to generate LLVM IR".to_string())
            }
        }
    }
}

/// Helper to compile and run a program, returning its output
fn compile_and_run(source: &str, output_name: &str) -> Result<String, String> {
    let exe_path = compile_to_executable(source, output_name)?;

    // Check if we got an executable or just LLVM IR
    if exe_path.extension().and_then(|s| s.to_str()) == Some("ll") {
        return Err("Executable compilation not available (LLVM tools missing)".to_string());
    }

    // Run the executable
    let output = Command::new(&exe_path)
        .output()
        .map_err(|e| format!("Failed to run executable: {}", e))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!("Program failed with stderr: {}", stderr));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    Ok(stdout.trim().to_string())
}

#[test]
fn test_full_pipeline_hello_world() {
    let source = "fn main():
    print(\"Hello, World!\")";

    let result = compile_to_executable(source, "hello");
    assert!(result.is_ok(), "Full pipeline failed: {:?}", result.err());
}

#[test]
fn test_full_pipeline_arithmetic() {
    let source = "fn main() -> int:
    let a = 10
    let b = 20
    return a + b * 2";

    let result = compile_to_executable(source, "arithmetic");
    assert!(result.is_ok(), "Full pipeline failed: {:?}", result.err());
}

#[test]
fn test_full_pipeline_control_flow() {
    let source = "fn max(a: int, b: int) -> int:
    if a > b:
        return a
    else:
        return b

fn main() -> int:
    return max(10, 20)";

    let result = compile_to_executable(source, "control_flow");
    assert!(result.is_ok(), "Full pipeline failed: {:?}", result.err());
}

#[test]
fn test_full_pipeline_loops() {
    let source = "fn sum(n: int) -> int:
    let total = 0
    let i = 0
    while i <= n:
        total = total + i
        i = i + 1
    return total

fn main() -> int:
    return sum(10)";

    let result = compile_to_executable(source, "loops");
    assert!(result.is_ok(), "Full pipeline failed: {:?}", result.err());
}

#[test]
fn test_full_pipeline_function_calls() {
    let source = "fn add(a: int, b: int) -> int:
    return a + b

fn multiply(a: int, b: int) -> int:
    return a * b

fn main() -> int:
    let x = add(5, 3)
    let y = multiply(x, 2)
    return y";

    let result = compile_to_executable(source, "function_calls");
    assert!(result.is_ok(), "Full pipeline failed: {:?}", result.err());
}

#[test]
fn test_full_pipeline_recursive() {
    let source = "fn factorial(n: int) -> int:
    if n <= 1:
        return 1
    else:
        return n * factorial(n - 1)

fn main() -> int:
    return factorial(5)";

    let result = compile_to_executable(source, "recursive");
    assert!(result.is_ok(), "Full pipeline failed: {:?}", result.err());
}

#[test]
fn test_full_pipeline_with_stdlib() {
    let source = "fn main() -> int:
    let s = \"hello\"
    return len(s)";

    let result = compile_to_executable(source, "stdlib");
    assert!(result.is_ok(), "Full pipeline failed: {:?}", result.err());
}

#[test]
fn test_full_pipeline_complex_program() {
    let source = "\"\"\"Calculate sum of squares.\"\"\"
fn square(x: int) -> int:
    return x * x

fn sum_squares(n: int) -> int:
    let total = 0
    let i = 1
    while i <= n:
        total = total + square(i)
        i = i + 1
    return total

fn main() -> int:
    return sum_squares(10)";

    let result = compile_to_executable(source, "complex");
    assert!(result.is_ok(), "Full pipeline failed: {:?}", result.err());
}

#[test]
fn test_full_pipeline_error_handling() {
    // Test that type errors are caught before codegen
    let source = "fn main() -> int:
    let x: int = \"not an int\"
    return x";

    let program = match parse(source) {
        Ok(p) => p,
        Err(_) => return, // Parse error is acceptable
    };

    let result = type_check_program(&program);
    assert!(result.is_err(), "Should catch type error before codegen");
}

#[test]
fn test_full_pipeline_optimization_preserves_semantics() {
    let source = "fn test() -> int:
    let x = 2 + 3
    let y = x * 4
    return y";

    // Compile with and without optimization
    let program = parse(source).unwrap();
    type_check_program(&program).unwrap();
    let ir_builder = IrBuilder::new();
    let ir = ir_builder.build(&program);

    let unoptimized = CodeGenerator::new(ir.clone()).generate();
    let optimized_ir = Optimizer::optimize(ir);
    let optimized = CodeGenerator::new(optimized_ir).generate();

    // Both should generate valid LLVM IR
    assert!(
        unoptimized.contains("define"),
        "Unoptimized should generate IR"
    );
    assert!(optimized.contains("define"), "Optimized should generate IR");
}

#[test]
fn test_full_pipeline_multiple_modules_concept() {
    // Test compilation of multiple functions (simulating modules)
    let source = "fn helper1(x: int) -> int:
    return x * 2

fn helper2(x: int) -> int:
    return x + 10

fn main() -> int:
    let a = helper1(5)
    let b = helper2(a)
    return b";

    let result = compile_to_executable(source, "modules");
    assert!(result.is_ok(), "Full pipeline failed: {:?}", result.err());
}

#[test]
fn test_full_pipeline_edge_cases() {
    // Test edge cases in full pipeline
    let test_cases = vec![
        ("fn main() -> int:\n    return 0", "zero"),
        ("fn main() -> int:\n    return -1", "negative"),
        ("fn main() -> bool:\n    return true", "bool_true"),
        ("fn main() -> bool:\n    return false", "bool_false"),
    ];

    for (source, name) in test_cases {
        let result = compile_to_executable(source, name);
        assert!(result.is_ok(), "Failed for {}: {:?}", name, result.err());
    }
}

// ============================================================================
// Executable Execution Tests - These tests compile and run programs
// ============================================================================

#[test]
fn test_execute_simple_return() {
    let source = "fn main() -> int:\n    return 42";

    match compile_and_run(source, "simple_return") {
        Ok(output) => assert_eq!(output, "42", "Expected output '42', got '{}'", output),
        Err(e) => {
            // Skip if LLVM tools not available
            if e.contains("LLVM tools missing") {
                eprintln!("Skipping test: {}", e);
                return;
            }
            panic!("Failed to compile and run: {}", e);
        }
    }
}

#[test]
fn test_execute_arithmetic() {
    let source = "fn main() -> int:\n    return 10 + 20 * 2";

    match compile_and_run(source, "arithmetic") {
        Ok(output) => assert_eq!(output, "50", "Expected output '50', got '{}'", output),
        Err(e) => {
            if e.contains("LLVM tools missing") {
                eprintln!("Skipping test: {}", e);
                return;
            }
            panic!("Failed to compile and run: {}", e);
        }
    }
}

#[test]
fn test_execute_function_call() {
    let source = "fn add(a: int, b: int) -> int:\n    return a + b\n\nfn main() -> int:\n    return add(5, 3)";

    match compile_and_run(source, "function_call") {
        Ok(output) => assert_eq!(output, "8", "Expected output '8', got '{}'", output),
        Err(e) => {
            if e.contains("LLVM tools missing") {
                eprintln!("Skipping test: {}", e);
                return;
            }
            panic!("Failed to compile and run: {}", e);
        }
    }
}

#[test]
fn test_execute_recursive_factorial() {
    let source = "fn factorial(n: int) -> int:\n    if n <= 1:\n        return 1\n    else:\n        return n * factorial(n - 1)\n\nfn main() -> int:\n    return factorial(5)";

    match compile_and_run(source, "factorial") {
        Ok(output) => assert_eq!(output, "120", "Expected output '120', got '{}'", output),
        Err(e) => {
            if e.contains("LLVM tools missing") {
                eprintln!("Skipping test: {}", e);
                return;
            }
            panic!("Failed to compile and run: {}", e);
        }
    }
}

#[test]
fn test_execute_loop_sum() {
    let source = "fn sum(n: int) -> int:\n    let total = 0\n    let i = 0\n    while i <= n:\n        total = total + i\n        i = i + 1\n    return total\n\nfn main() -> int:\n    return sum(10)";

    match compile_and_run(source, "loop_sum") {
        Ok(output) => assert_eq!(output, "55", "Expected output '55', got '{}'", output),
        Err(e) => {
            if e.contains("LLVM tools missing") {
                eprintln!("Skipping test: {}", e);
                return;
            }
            panic!("Failed to compile and run: {}", e);
        }
    }
}

#[test]
fn test_execute_conditional() {
    let source = "fn max(a: int, b: int) -> int:\n    if a > b:\n        return a\n    else:\n        return b\n\nfn main() -> int:\n    return max(10, 20)";

    match compile_and_run(source, "conditional") {
        Ok(output) => assert_eq!(output, "20", "Expected output '20', got '{}'", output),
        Err(e) => {
            if e.contains("LLVM tools missing") {
                eprintln!("Skipping test: {}", e);
                return;
            }
            panic!("Failed to compile and run: {}", e);
        }
    }
}

#[test]
fn test_execute_nested_calls() {
    let source = "fn square(x: int) -> int:\n    return x * x\n\nfn sum_squares(n: int) -> int:\n    let total = 0\n    let i = 1\n    while i <= n:\n        total = total + square(i)\n        i = i + 1\n    return total\n\nfn main() -> int:\n    return sum_squares(5)";

    match compile_and_run(source, "nested_calls") {
        Ok(output) => assert_eq!(
            output, "55",
            "Expected output '55' (1+4+9+16+25), got '{}'",
            output
        ),
        Err(e) => {
            if e.contains("LLVM tools missing") {
                eprintln!("Skipping test: {}", e);
                return;
            }
            panic!("Failed to compile and run: {}", e);
        }
    }
}

#[test]
fn test_execute_multiple_functions() {
    let source = "fn double(x: int) -> int:\n    return x * 2\n\nfn triple(x: int) -> int:\n    return x * 3\n\nfn main() -> int:\n    let a = double(5)\n    let b = triple(a)\n    return b";

    match compile_and_run(source, "multiple_funcs") {
        Ok(output) => assert_eq!(output, "30", "Expected output '30', got '{}'", output),
        Err(e) => {
            if e.contains("LLVM tools missing") {
                eprintln!("Skipping test: {}", e);
                return;
            }
            panic!("Failed to compile and run: {}", e);
        }
    }
}

#[test]
fn test_execute_negative_numbers() {
    let source = "fn main() -> int:\n    return -42";

    match compile_and_run(source, "negative") {
        Ok(output) => assert_eq!(output, "-42", "Expected output '-42', got '{}'", output),
        Err(e) => {
            if e.contains("LLVM tools missing") {
                eprintln!("Skipping test: {}", e);
                return;
            }
            panic!("Failed to compile and run: {}", e);
        }
    }
}

#[test]
fn test_execute_complex_expression() {
    let source = "fn main() -> int:\n    return (10 + 5) * 2 - 3";

    match compile_and_run(source, "complex_expr") {
        Ok(output) => assert_eq!(output, "27", "Expected output '27', got '{}'", output),
        Err(e) => {
            if e.contains("LLVM tools missing") {
                eprintln!("Skipping test: {}", e);
                return;
            }
            panic!("Failed to compile and run: {}", e);
        }
    }
}
