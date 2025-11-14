// Documentation generator module

use crate::ast::*;
use crate::stdlib::{get_stdlib_functions, StdlibFunction};

/// Generate markdown documentation from a Pain program
pub struct DocGenerator;

impl DocGenerator {
    /// Generate markdown documentation for a program
    pub fn generate(program: &Program) -> String {
        let mut output = String::new();
        
        output.push_str("# API Documentation\n\n");
        output.push_str("Generated from Pain source code.\n\n");
        output.push_str("---\n\n");
        
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    output.push_str(&Self::format_function(func));
                    output.push_str("\n\n");
                }
                Item::Class(_class) => {
                    // TODO: Format classes in documentation
                }
            }
        }
        
        output
    }
    
    /// Format a function as markdown documentation
    fn format_function(func: &Function) -> String {
        let mut output = String::new();
        
        // Function signature as heading
        output.push_str(&format!("## `{}`\n\n", func.name));
        
        // Doc comment
        if let Some(ref doc) = func.doc {
            output.push_str(doc);
            output.push_str("\n\n");
        }
        
        // Attributes
        if !func.attrs.is_empty() {
            output.push_str("**Attributes:**\n");
            for attr in &func.attrs {
                output.push_str(&format!("- `@{}`\n", attr.name));
            }
            output.push_str("\n");
        }
        
        // Signature
        output.push_str("**Signature:**\n");
        output.push_str("```pain\n");
        output.push_str(&Self::format_signature(func));
        output.push_str("\n```\n\n");
        
        // Parameters
        if !func.params.is_empty() {
            output.push_str("**Parameters:**\n");
            for param in &func.params {
                output.push_str(&format!("- `{}`: {}\n", param.name, Self::format_type(&param.ty)));
            }
            output.push_str("\n");
        }
        
        // Return type
        if let Some(ref ret_ty) = func.return_type {
            output.push_str("**Returns:**\n");
            output.push_str(&format!("- `{}`\n\n", Self::format_type(ret_ty)));
        }
        
        output
    }
    
    /// Format function signature
    fn format_signature(func: &Function) -> String {
        let mut sig = String::new();
        
        // Attributes
        if !func.attrs.is_empty() {
            for attr in &func.attrs {
                sig.push_str(&format!("@{} ", attr.name));
            }
        }
        
        // Function declaration
        sig.push_str("fn ");
        sig.push_str(&func.name);
        sig.push('(');
        
        let params: Vec<String> = func.params.iter()
            .map(|p| format!("{}: {}", p.name, Self::format_type(&p.ty)))
            .collect();
        sig.push_str(&params.join(", "));
        sig.push(')');
        
        // Return type
        if let Some(ref ret_ty) = func.return_type {
            sig.push_str(" -> ");
            sig.push_str(&Self::format_type(ret_ty));
        }
        
        sig
    }
    
    /// Format type for display
    fn format_type(ty: &Type) -> String {
        match ty {
            Type::Int => "int".to_string(),
            Type::Str => "str".to_string(),
            Type::Float32 => "float32".to_string(),
            Type::Float64 => "float64".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Dynamic => "dynamic".to_string(),
            Type::List(inner) => format!("list[{}]", Self::format_type(inner)),
            Type::Array(inner) => format!("array[{}]", Self::format_type(inner)),
            Type::Map(k, v) => format!("map[{}, {}]", Self::format_type(k), Self::format_type(v)),
            Type::Tensor(inner, dims) => format!("Tensor[{}, {:?}]", Self::format_type(inner), dims),
            Type::Named(name) => name.clone(),
        }
    }
    
    /// Generate markdown documentation for standard library
    pub fn generate_stdlib() -> String {
        let mut output = String::new();
        
        output.push_str("# Pain Standard Library\n\n");
        output.push_str("This document describes all built-in functions available in the Pain language.\n\n");
        output.push_str("---\n\n");
        
        let functions = get_stdlib_functions();
        
        // Group functions by category
        let mut math_functions = Vec::new();
        let mut string_functions = Vec::new();
        let mut io_functions = Vec::new();
        
        for func in &functions {
            if func.name == "print" {
                io_functions.push(func);
            } else if matches!(func.name.as_str(), "abs" | "min" | "max" | "sqrt" | "pow" | "sin" | "cos" | "floor" | "ceil") {
                math_functions.push(func);
            } else {
                string_functions.push(func);
            }
        }
        
        // Math functions
        if !math_functions.is_empty() {
            output.push_str("## Math Functions\n\n");
            for func in &math_functions {
                output.push_str(&Self::format_stdlib_function(func));
                output.push_str("\n");
            }
            output.push_str("\n");
        }
        
        // String functions
        if !string_functions.is_empty() {
            output.push_str("## String Functions\n\n");
            for func in &string_functions {
                output.push_str(&Self::format_stdlib_function(func));
                output.push_str("\n");
            }
            output.push_str("\n");
        }
        
        // I/O functions
        if !io_functions.is_empty() {
            output.push_str("## I/O Functions\n\n");
            for func in &io_functions {
                output.push_str(&Self::format_stdlib_function(func));
                output.push_str("\n");
            }
            output.push_str("\n");
        }
        
        output
    }
    
    /// Format a standard library function as markdown
    fn format_stdlib_function(func: &StdlibFunction) -> String {
        let mut output = String::new();
        
        // Function name as heading
        output.push_str(&format!("### `{}`\n\n", func.name));
        
        // Description
        output.push_str(&func.description);
        output.push_str("\n\n");
        
        // Signature
        output.push_str("**Signature:**\n");
        output.push_str("```pain\n");
        output.push_str(&Self::format_stdlib_signature(func));
        output.push_str("\n```\n\n");
        
        // Parameters
        if !func.params.is_empty() {
            output.push_str("**Parameters:**\n");
            for (name, ty) in &func.params {
                output.push_str(&format!("- `{}`: {}\n", name, Self::format_type(ty)));
            }
            output.push_str("\n");
        }
        
        // Return type
        output.push_str("**Returns:**\n");
        output.push_str(&format!("- `{}`\n\n", Self::format_type(&func.return_type)));
        
        output
    }
    
    /// Format standard library function signature
    fn format_stdlib_signature(func: &StdlibFunction) -> String {
        let mut sig = String::new();
        
        sig.push_str("fn ");
        sig.push_str(&func.name);
        sig.push('(');
        
        let params: Vec<String> = func.params.iter()
            .map(|(name, ty)| format!("{}: {}", name, Self::format_type(ty)))
            .collect();
        sig.push_str(&params.join(", "));
        sig.push(')');
        
        sig.push_str(" -> ");
        sig.push_str(&Self::format_type(&func.return_type));
        
        sig
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::span::Span;
    use crate::span::Position;
    
    #[test]
    fn test_generate_doc() {
        let program = Program {
            items: vec![
                Item::Function(Function {
                    doc: Some("Adds two numbers together.".to_string()),
                    attrs: vec![],
                    name: "add".to_string(),
                    params: vec![
                        Parameter {
                            name: "a".to_string(),
                            ty: Type::Int,
                        },
                        Parameter {
                            name: "b".to_string(),
                            ty: Type::Int,
                        },
                    ],
                    return_type: Some(Type::Int),
                    body: vec![],
                    span: Span::single(Position::start()),
                }),
            ],
            span: Span::single(Position::start()),
        };
        
        let doc = DocGenerator::generate(&program);
        assert!(doc.contains("## `add`"));
        assert!(doc.contains("Adds two numbers together."));
        assert!(doc.contains("fn add"));
    }
}

