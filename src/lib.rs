// Pain compiler library

pub mod lexer;
pub mod parser;
pub mod ast;
pub mod type_checker;

pub use lexer::Token;
pub use parser::parse;
pub use type_checker::{type_check_program, TypeError, TypeResult};

