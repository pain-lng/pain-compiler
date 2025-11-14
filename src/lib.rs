// Pain compiler library

pub mod lexer;
pub mod parser;
pub mod ast;
pub mod type_checker;
pub mod interpreter;
pub mod ir;
pub mod ir_builder;
pub mod codegen;
pub mod formatter;
pub mod span;
pub mod error;
pub mod stdlib;
pub mod doc_generator;

pub use lexer::Token;
pub use parser::parse;
pub use type_checker::{type_check_program, TypeError, TypeResult};
pub use interpreter::Interpreter;
pub use ir::IrProgram;
pub use ir_builder::IrBuilder;
pub use codegen::CodeGenerator;
pub use formatter::Formatter;
pub use span::{Span, Position, PositionTracker};
pub use error::ErrorFormatter;
pub use stdlib::{get_stdlib_functions, is_stdlib_function, get_stdlib_return_type};
pub use doc_generator::DocGenerator;

// Re-export AST types for LSP and other tools
pub use ast::*;

