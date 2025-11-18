// Pain compiler CLI entry point

use clap::{Parser, Subcommand};
use pain_compiler::{parse, type_check_program, Interpreter, IrBuilder, CodeGenerator, MlirCodeGenerator, Formatter, ErrorFormatter, DocGenerator, Optimizer, llvm_tools};
use std::fs;
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "pain")]
#[command(about = "Pain language compiler", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Build a Pain source file
    Build {
        /// Input source file
        #[arg(short, long)]
        input: PathBuf,
        /// Output file (optional, defaults to executable if --executable is set)
        #[arg(short, long)]
        output: Option<PathBuf>,
        /// Generate executable instead of IR
        #[arg(long)]
        executable: bool,
        /// Target triple (e.g., x86_64-unknown-linux-gnu)
        #[arg(long)]
        target: Option<String>,
        /// Keep intermediate files (IR, object files)
        #[arg(long)]
        keep_intermediates: bool,
        /// Backend to use: llvm (default) or mlir
        #[arg(long, default_value = "llvm")]
        backend: String,
    },
    /// Run a Pain source file
    Run {
        /// Input source file
        #[arg(short, long)]
        input: PathBuf,
    },
    /// Check a Pain source file (parse and type check)
    Check {
        /// Input source file
        #[arg(short, long)]
        input: PathBuf,
    },
    /// Format a Pain source file
    Format {
        /// Input source file
        #[arg(short, long)]
        input: PathBuf,
        /// Output file (optional, defaults to overwriting input)
        #[arg(short, long)]
        output: Option<PathBuf>,
        /// Write to stdout instead of file
        #[arg(long)]
        stdout: bool,
    },
    /// Generate documentation from Pain source file
    Doc {
        /// Input source file
        #[arg(short, long)]
        input: Option<PathBuf>,
        /// Output file (optional, defaults to stdout)
        #[arg(short, long)]
        output: Option<PathBuf>,
        /// Generate standard library documentation instead of source file
        #[arg(long)]
        stdlib: bool,
    },
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Build { input, output, executable, target, keep_intermediates, backend } => {
            build(&input, output.as_ref(), executable, target.as_deref(), keep_intermediates, &backend)?;
        }
        Commands::Run { input } => {
            run(&input)?;
        }
        Commands::Check { input } => {
            check(&input)?;
        }
        Commands::Format { input, output, stdout } => {
            format_file(&input, output.as_ref(), stdout)?;
        }
        Commands::Doc { input, output, stdlib } => {
            if stdlib {
                generate_stdlib_doc(output.as_ref())?;
            } else if let Some(input) = input {
                generate_doc(&input, output.as_ref())?;
            } else {
                return Err(anyhow::anyhow!("Either --input or --stdlib must be specified"));
            }
        }
    }

    Ok(())
}

fn build(
    input: &PathBuf,
    output: Option<&PathBuf>,
    executable: bool,
    target: Option<&str>,
    keep_intermediates: bool,
    backend: &str,
) -> anyhow::Result<()> {
    println!("Building: {:?}", input);
    
    let source = fs::read_to_string(input)?;
    let program = parse(&source).map_err(|e| anyhow::anyhow!("Parse error: {}", e))?;
    
    // Type check
    if let Err(e) = type_check_program(&program) {
        let formatter = ErrorFormatter::new(&source);
        eprintln!("{}", formatter.format_error(&e));
        return Err(anyhow::anyhow!("Type check failed"));
    }
    
    println!("✓ Parsed and type-checked successfully");
    
    // Build IR
    let ir_builder = IrBuilder::new();
    let mut ir = ir_builder.build(&program);
    
    // Optimize IR
    println!("Running optimizations...");
    ir = Optimizer::optimize(ir);
    println!("✓ Optimizations completed");
    
    // Generate code based on backend
    match backend.to_lowercase().as_str() {
        "mlir" => {
            let mlir_codegen = MlirCodeGenerator::new(ir);
            let mlir_ir = mlir_codegen.generate();
            
            if executable {
                return Err(anyhow::anyhow!("MLIR backend does not support executable generation yet. Use --backend llvm for executables."));
            }
            
            if let Some(output) = output {
                println!("Output: {:?}", output);
                fs::write(output, mlir_ir)?;
                println!("✓ MLIR generated successfully");
            } else {
                println!("⚠ No output file specified, skipping code generation");
                println!("\nGenerated MLIR:\n{}", mlir_ir);
            }
        }
        "llvm" | _ => {
            // Determine target triple
            let target_triple = target.map(|s| s.to_string()).unwrap_or_else(|| {
                let detected = llvm_tools::detect_target_triple();
                println!("Using target triple: {}", detected);
                detected
            });
            
            // Generate LLVM IR
            let codegen = CodeGenerator::new(ir);
            let llvm_ir = codegen.generate_with_target(Some(&target_triple));
            
            if executable {
                // Generate executable
                let executable_path = output
                    .map(|p| p.to_path_buf())
                    .unwrap_or_else(|| {
                        // Default to input filename without extension + .exe on Windows
                        let mut path = input.clone();
                        path.set_extension("");
                        #[cfg(target_os = "windows")]
                        path.set_extension("exe");
                        path
                    });
                
                // Create temporary LLVM IR file
                let llvm_ir_path = executable_path.with_extension("ll");
                fs::write(&llvm_ir_path, llvm_ir)?;
                
                // Compile and link
                llvm_tools::compile_to_executable(
                    &llvm_ir_path,
                    &executable_path,
                    Some(&target_triple),
                    keep_intermediates,
                )?;
                
                println!("✓ Executable built successfully: {:?}", executable_path);
            } else {
                // Just generate LLVM IR
                if let Some(output) = output {
                    println!("Output: {:?}", output);
                    fs::write(output, llvm_ir)?;
                    println!("✓ LLVM IR generated successfully");
                } else {
                    println!("⚠ No output file specified, skipping code generation");
                    println!("\nGenerated LLVM IR:\n{}", llvm_ir);
                }
            }
        }
    }
    
    Ok(())
}

fn run(input: &PathBuf) -> anyhow::Result<()> {
    let source = fs::read_to_string(input)?;
    let program = parse(&source).map_err(|e| anyhow::anyhow!("Parse error: {}", e))?;
    
    // Type check
    if let Err(e) = type_check_program(&program) {
        let formatter = ErrorFormatter::new(&source);
        eprintln!("{}", formatter.format_error(&e));
        return Err(anyhow::anyhow!("Type check failed"));
    }
    
    // Execute
    let mut interpreter = Interpreter::new()
        .map_err(|e| anyhow::anyhow!("Failed to create interpreter: {}", e))?;
    
    let result = interpreter.interpret(&program)
        .map_err(|e| anyhow::anyhow!("Runtime error: {:?}", e))?;
    
    // Print result (for benchmarks and CLI usage)
    match result {
        pain_runtime::Value::Int(i) => println!("{}", i),
        pain_runtime::Value::Float(f) => println!("{}", f),
        pain_runtime::Value::Bool(b) => println!("{}", b),
        pain_runtime::Value::String(s) => println!("{}", s),
        pain_runtime::Value::None => {},
        pain_runtime::Value::Object(_) => println!("[Object]"),
        pain_runtime::Value::List(list) => {
            print!("[");
            for (i, item) in list.iter().enumerate() {
                if i > 0 { print!(", "); }
                match item {
                    pain_runtime::Value::Int(n) => print!("{}", n),
                    pain_runtime::Value::Float(f) => print!("{}", f),
                    pain_runtime::Value::Bool(b) => print!("{}", b),
                    pain_runtime::Value::String(s) => print!("\"{}\"", s),
                    _ => print!("{:?}", item),
                }
            }
            println!("]");
        }
        pain_runtime::Value::Array(arr) => {
            print!("[");
            for (i, item) in arr.iter().enumerate() {
                if i > 0 { print!(", "); }
                match item {
                    pain_runtime::Value::Int(n) => print!("{}", n),
                    pain_runtime::Value::Float(f) => print!("{}", f),
                    pain_runtime::Value::Bool(b) => print!("{}", b),
                    pain_runtime::Value::String(s) => print!("\"{}\"", s),
                    _ => print!("{:?}", item),
                }
            }
            println!("]");
        }
    }
    
    Ok(())
}

fn check(input: &PathBuf) -> anyhow::Result<()> {
    println!("Checking: {:?}", input);
    
    let source = fs::read_to_string(input)?;
    let program = parse(&source).map_err(|e| anyhow::anyhow!("Parse error: {}", e))?;
    
    // Type check
    if let Err(e) = type_check_program(&program) {
        let formatter = ErrorFormatter::new(&source);
        eprintln!("{}", formatter.format_error(&e));
        return Err(anyhow::anyhow!("Type check failed"));
    }
    
    println!("✓ All checks passed!");
    
    Ok(())
}

fn format_file(input: &PathBuf, output: Option<&PathBuf>, stdout: bool) -> anyhow::Result<()> {
    println!("Formatting: {:?}", input);
    
    let source = fs::read_to_string(input)?;
    let program = parse(&source).map_err(|e| anyhow::anyhow!("Parse error: {}", e))?;
    
    // Format the program
    let formatted = Formatter::format(&program);
    
    if stdout {
        print!("{}", formatted);
    } else if let Some(output) = output {
        fs::write(output, formatted)?;
        println!("✓ Formatted code written to {:?}", output);
    } else {
        // Overwrite input file
        fs::write(input, formatted)?;
        println!("✓ Formatted code written to {:?}", input);
    }
    
    Ok(())
}

fn generate_doc(input: &PathBuf, output: Option<&PathBuf>) -> anyhow::Result<()> {
    println!("Generating documentation: {:?}", input);
    
    let source = fs::read_to_string(input)?;
    let program = parse(&source).map_err(|e| anyhow::anyhow!("Parse error: {}", e))?;
    
    // Generate documentation
    let doc = DocGenerator::generate(&program);
    
    if let Some(output) = output {
        fs::write(output, doc)?;
        println!("✓ Documentation written to {:?}", output);
    } else {
        print!("{}", doc);
    }
    
    Ok(())
}

fn generate_stdlib_doc(output: Option<&PathBuf>) -> anyhow::Result<()> {
    println!("Generating standard library documentation");
    
    // Generate stdlib documentation
    let doc = DocGenerator::generate_stdlib();
    
    if let Some(output) = output {
        fs::write(output, doc)?;
        println!("✓ Standard library documentation written to {:?}", output);
    } else {
        print!("{}", doc);
    }
    
    Ok(())
}
