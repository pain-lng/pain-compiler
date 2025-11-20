// Pain compiler library

pub mod ast;
pub mod codegen;
pub mod doc_generator;
pub mod error;
pub mod formatter;
pub mod interpreter;
pub mod ir;
pub mod ir_builder;
pub mod jit;
#[cfg(feature = "jit")]
pub mod jit_orc;
pub mod lexer;
pub mod llvm_tools;
pub mod mlir_codegen;
pub mod optimizations;
pub mod parser;
pub mod span;
pub mod stdlib;
pub mod type_checker;
pub mod warnings;

pub use codegen::CodeGenerator;
pub use doc_generator::DocGenerator;
pub use error::ErrorFormatter;
pub use formatter::Formatter;
pub use interpreter::Interpreter;
pub use ir::IrProgram;
pub use ir_builder::IrBuilder;
pub use jit::{JitEngine, JitFunction};
pub use lexer::Token;
pub use mlir_codegen::MlirCodeGenerator;
pub use optimizations::Optimizer;
pub use parser::{parse, parse_with_recovery};
pub use span::{Position, PositionTracker, Span};
pub use stdlib::{get_stdlib_functions, get_stdlib_return_type, is_stdlib_function};
pub use type_checker::{
    type_check_program, type_check_program_with_context, TypeContext, TypeError, TypeResult,
};
pub use warnings::{Warning, WarningCollector};

// Re-export AST types for LSP and other tools
pub use ast::*;
