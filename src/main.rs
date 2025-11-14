// Pain compiler CLI entry point

use clap::{Parser, Subcommand};
use pain_compiler::{parse, type_check_program};
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
    }

    Ok(())
}

fn build(input: &PathBuf, output: Option<&PathBuf>) -> anyhow::Result<()> {
    println!("Building: {:?}", input);
    
    let source = fs::read_to_string(input)?;
    let program = parse(&source).map_err(|e| anyhow::anyhow!("Parse error: {}", e))?;
    
    // Type check
    type_check_program(&program).map_err(|e| anyhow::anyhow!("Type error: {:?}", e))?;
    
    println!("✓ Parsed and type-checked successfully");
    
    if let Some(output) = output {
        println!("Output: {:?}", output);
        // TODO: Generate code and write to output file
        println!("⚠ Code generation not yet implemented");
    } else {
        println!("⚠ No output file specified, skipping code generation");
    }
    
    Ok(())
}

fn run(input: &PathBuf) -> anyhow::Result<()> {
    println!("Running: {:?}", input);
    
    let source = fs::read_to_string(input)?;
    let program = parse(&source).map_err(|e| anyhow::anyhow!("Parse error: {}", e))?;
    
    // Type check
    type_check_program(&program).map_err(|e| anyhow::anyhow!("Type error: {:?}", e))?;
    
    println!("✓ Parsed and type-checked successfully");
    println!("⚠ Execution not yet implemented");
    
    Ok(())
}

fn check(input: &PathBuf) -> anyhow::Result<()> {
    println!("Checking: {:?}", input);
    
    let source = fs::read_to_string(input)?;
    let program = parse(&source).map_err(|e| anyhow::anyhow!("Parse error: {}", e))?;
    
    // Type check
    type_check_program(&program).map_err(|e| anyhow::anyhow!("Type error: {:?}", e))?;
    
    println!("✓ All checks passed!");
    
    Ok(())
}
