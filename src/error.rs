// Error formatting with source code context

use crate::span::{Span, PositionTracker};
use crate::type_checker::TypeError;

pub struct ErrorFormatter<'a> {
    source: &'a str,
    tracker: PositionTracker,
}

impl<'a> ErrorFormatter<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            tracker: PositionTracker::new(source),
        }
    }

    pub fn format_error(&self, error: &TypeError) -> String {
        match error {
            TypeError::UndefinedVariable { name, span } => {
                self.format_with_span(
                    &format!("error: undefined variable `{}`", name),
                    *span,
                )
            }
            TypeError::TypeMismatch { expected, found, span } => {
                self.format_with_span(
                    &format!(
                        "error: type mismatch: expected `{}`, found `{}`",
                        self.format_type(expected),
                        self.format_type(found)
                    ),
                    *span,
                )
            }
            TypeError::CannotInferType { message, span } => {
                self.format_with_span(
                    &format!("error: cannot infer type: {}", message),
                    *span,
                )
            }
            TypeError::InvalidOperation { op, left, right, span } => {
                let right_str = right
                    .as_ref()
                    .map(|t| format!(" and `{}`", self.format_type(t)))
                    .unwrap_or_default();
                self.format_with_span(
                    &format!(
                        "error: invalid operation `{}` for types `{}`{}",
                        op,
                        self.format_type(left),
                        right_str
                    ),
                    *span,
                )
            }
        }
    }

    fn format_with_span(&self, message: &str, span: Span) -> String {
        let mut output = String::new();
        let line = span.line();
        let column = span.column();

        // Error message
        output.push_str(&format!("{}:{}:{} {}\n", "error", line, column, message));

        // Get context lines
        let context_lines = self.tracker.get_context(line, 2);

        // Find the line with the error
        for (line_num, line_content) in &context_lines {
            // Line number and content
            let line_marker = if *line_num == line { ">" } else { " " };
            output.push_str(&format!(" {} {} | {}\n", line_marker, line_num, line_content));

            // Underline the error span on the error line
            if *line_num == line {
                if let Some(snippet) = span.snippet(self.source) {
                    // Calculate the start column in characters
                    let line_start = self.tracker.line_start(line);
                    let _span_start_in_line = span.start.offset.saturating_sub(line_start);
                    
                    // Count characters up to the span start
                    let line_text = &self.source[line_start..span.start.offset.min(self.source.len())];
                    let char_offset = line_text.chars().count();
                    
                    // Create underline
                    let underline = " ".repeat(char_offset) + &"^".repeat(snippet.chars().count().max(1));
                    output.push_str(&format!("     | {}\n", underline));
                }
            }
        }

        output
    }

    fn format_type(&self, ty: &crate::ast::Type) -> String {
        match ty {
            crate::ast::Type::Int => "int".to_string(),
            crate::ast::Type::Str => "str".to_string(),
            crate::ast::Type::Float32 => "float32".to_string(),
            crate::ast::Type::Float64 => "float64".to_string(),
            crate::ast::Type::Bool => "bool".to_string(),
            crate::ast::Type::Dynamic => "dynamic".to_string(),
            crate::ast::Type::List(elem) => format!("list[{}]", self.format_type(elem)),
            crate::ast::Type::Array(elem) => format!("array[{}]", self.format_type(elem)),
            crate::ast::Type::Map(key, val) => {
                format!("map[{}, {}]", self.format_type(key), self.format_type(val))
            }
            crate::ast::Type::Tensor(elem, dims) => {
                let dims_str = if dims.is_empty() {
                    String::new()
                } else {
                    format!(", {:?}", dims)
                };
                format!("Tensor[{}{}]", self.format_type(elem), dims_str)
            }
            crate::ast::Type::Named(name) => name.clone(),
        }
    }
}
