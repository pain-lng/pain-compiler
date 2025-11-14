// Pain compiler CLI entry point

use clap::{Parser, Subcommand};
use pain_compiler::{parse, type_check_program, Interpreter, IrBuilder, CodeGenerator, Formatter, ErrorFormatter, DocGenerator};
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
        /// Output file (optional)
        #[arg(short, long)]
        output: Option<PathBuf>,
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
        input: PathBuf,
        /// Output file (optional, defaults to stdout)
        #[arg(short, long)]
        output: Option<PathBuf>,
    },
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Build { input, output } => {
            build(&input, output.as_ref())?;
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
        Commands::Doc { input, output } => {
            generate_doc(&input, output.as_ref())?;
        }
    }

    Ok(())
}

fn build(input: &PathBuf, output: Option<&PathBuf>) -> anyhow::Result<()> {
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
    let ir = ir_builder.build(&program);
    
    // Generate LLVM IR
    let codegen = CodeGenerator::new(ir);
    let llvm_ir = codegen.generate();
    
    if let Some(output) = output {
        println!("Output: {:?}", output);
        fs::write(output, llvm_ir)?;
        println!("✓ LLVM IR generated successfully");
    } else {
        println!("⚠ No output file specified, skipping code generation");
        println!("\nGenerated LLVM IR:\n{}", llvm_ir);
    }
    
    Ok(())
}

fn run(input: &PathBuf) -> anyhow::Result<()> {
    println!("Running: {:?}", input);
    
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
    
    interpreter.interpret(&program)
        .map_err(|e| anyhow::anyhow!("Runtime error: {:?}", e))?;
    
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
