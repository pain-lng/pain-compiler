// Error formatting with source code context

use crate::span::{PositionTracker, Span};
use crate::type_checker::{TypeContext, TypeError};
use crate::warnings::Warning;

pub struct ErrorFormatter<'a> {
    source: &'a str,
    tracker: PositionTracker,
    type_context: Option<&'a TypeContext>,
}

// Structured parser error
#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
    pub expected: Option<String>,
    pub found: Option<String>,
}

// Helper function to calculate Levenshtein distance for fuzzy matching
fn levenshtein_distance(s1: &str, s2: &str) -> usize {
    let s1_chars: Vec<char> = s1.chars().collect();
    let s2_chars: Vec<char> = s2.chars().collect();
    let n = s1_chars.len();
    let m = s2_chars.len();

    if n == 0 {
        return m;
    }
    if m == 0 {
        return n;
    }

    let mut dp = vec![vec![0; m + 1]; n + 1];

    for (i, row) in dp.iter_mut().enumerate().take(n + 1) {
        row[0] = i;
    }
    for (j, val) in dp[0].iter_mut().enumerate().take(m + 1) {
        *val = j;
    }

    for i in 1..=n {
        for j in 1..=m {
            let cost = if s1_chars[i - 1] == s2_chars[j - 1] {
                0
            } else {
                1
            };
            dp[i][j] = (dp[i - 1][j] + 1)
                .min(dp[i][j - 1] + 1)
                .min(dp[i - 1][j - 1] + cost);
        }
    }

    dp[n][m]
}

// Find similar names using fuzzy matching
fn find_similar_names(name: &str, candidates: &[String], max_distance: usize) -> Vec<String> {
    let mut matches: Vec<(String, usize)> = candidates
        .iter()
        .filter_map(|candidate| {
            let distance = levenshtein_distance(name, candidate);
            if distance <= max_distance && distance < candidate.len() {
                Some((candidate.clone(), distance))
            } else {
                None
            }
        })
        .collect();

    matches.sort_by_key(|(_, dist)| *dist);
    matches.into_iter().take(3).map(|(name, _)| name).collect()
}

impl<'a> ErrorFormatter<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            tracker: PositionTracker::new(source),
            type_context: None,
        }
    }

    pub fn with_context(mut self, ctx: &'a TypeContext) -> Self {
        self.type_context = Some(ctx);
        self
    }

    pub fn format_error(&self, error: &TypeError) -> String {
        match error {
            TypeError::UndefinedVariable { name, span } => {
                let mut message = format!("error: undefined variable `{}`", name);

                // Add suggestion if we have context
                if let Some(ctx) = self.type_context {
                    let mut candidates = ctx.get_variable_names();
                    candidates.extend(ctx.get_function_names());

                    let suggestions = find_similar_names(name, &candidates, 3);
                    if !suggestions.is_empty() {
                        message
                            .push_str(&format!("\n     help: did you mean `{}`?", suggestions[0]));
                    }
                }

                self.format_with_span(&message, *span)
            }
            TypeError::TypeMismatch {
                expected,
                found,
                span,
            } => {
                let expected_str = Self::format_type(expected);
                let found_str = Self::format_type(found);
                let mut message = format!(
                    "error: type mismatch: expected `{}`, found `{}`",
                    expected_str, found_str
                );

                // Add helpful suggestions for common type mismatches
                match (expected_str.as_str(), found_str.as_str()) {
                    ("int", "str") => {
                        message.push_str("\n     help: did you mean to convert the string to an integer? Use `to_int()`");
                    }
                    ("str", "int") => {
                        message.push_str("\n     help: did you mean to convert the integer to a string? Use `to_string()`");
                    }
                    ("bool", _) if found_str != "bool" => {
                        message.push_str("\n     help: condition must be a boolean value");
                    }
                    ("int", "float32") | ("int", "float64") => {
                        message.push_str("\n     help: did you mean to convert the float to an integer? Use `int()` or `to_int()`");
                    }
                    ("float32", "int") | ("float64", "int") => {
                        message.push_str("\n     help: did you mean to convert the integer to a float? Use `float()` or `to_float()`");
                    }
                    (exp, fnd) if exp.starts_with("list[") && !fnd.starts_with("list[") => {
                        message.push_str("\n     help: expected a list type");
                    }
                    (exp, fnd) if exp.starts_with("array[") && !fnd.starts_with("array[") => {
                        message.push_str("\n     help: expected an array type");
                    }
                    _ => {}
                }

                self.format_with_span(&message, *span)
            }
            TypeError::CannotInferType { message, span } => {
                self.format_with_span(&format!("error: cannot infer type: {}", message), *span)
            }
            TypeError::InvalidOperation {
                op,
                left,
                right,
                span,
            } => {
                let left_str = Self::format_type(left);
                let right_str = right
                    .as_ref()
                    .map(|t| format!(" and `{}`", Self::format_type(t)))
                    .unwrap_or_default();
                let mut message = format!(
                    "error: invalid operation `{}` for types `{}`{}",
                    op, left_str, right_str
                );

                // Add helpful suggestions for invalid operations
                match op.as_str() {
                    "arithmetic" => {
                        message.push_str("\n     help: arithmetic operations require numeric types (int, float32, float64)");
                    }
                    "indexing" => {
                        message.push_str(&format!(
                            "\n     help: `{}` is not indexable. Use list, array, or map types",
                            left_str
                        ));
                    }
                    "negation" => {
                        message.push_str(
                            "\n     help: negation requires a numeric type (int, float32, float64)",
                        );
                    }
                    _ => {}
                }

                self.format_with_span(&message, *span)
            }
        }
    }

    pub fn format_parse_error(&self, error: &ParseError) -> String {
        let mut message = format!("error: {}", error.message);

        if let Some(expected) = &error.expected {
            message.push_str(&format!("\n     expected: {}", expected));
        }
        if let Some(found) = &error.found {
            message.push_str(&format!("\n     found: {}", found));
        }

        self.format_with_span(&message, error.span)
    }

    fn format_with_span(&self, message: &str, span: Span) -> String {
        let mut output = String::new();
        let line = span.line();
        let column = span.column();

        // Error message header
        output.push_str(&format!(
            "error:{}:{}: {}\n",
            line,
            column,
            message.split('\n').next().unwrap_or(message)
        ));

        // Get context lines (more context for better readability)
        let context_lines = self.tracker.get_context(line, 3);

        // Find the line with the error
        for (line_num, line_content) in &context_lines {
            // Line number and content with better formatting
            let line_marker = if *line_num == line { ">" } else { " " };
            let line_num_str = format!("{:4}", line_num);
            output.push_str(&format!(
                "{} {} | {}\n",
                line_marker, line_num_str, line_content
            ));

            // Underline the error span on the error line
            if *line_num == line {
                if let Some(snippet) = span.snippet(self.source) {
                    // Calculate the start column in characters
                    let line_start = self.tracker.line_start(line);

                    // Count characters up to the span start
                    let line_text =
                        &self.source[line_start..span.start.offset.min(self.source.len())];
                    let char_offset = line_text.chars().count();

                    // Create underline with better visual indication
                    let snippet_len = snippet.chars().count().max(1);
                    let underline = " ".repeat(char_offset) + &"^".repeat(snippet_len);
                    output.push_str(&format!("     | {}\n", underline));
                } else {
                    // Fallback: just point to the column
                    let line_text = self.tracker.get_line(line).unwrap_or("");
                    let char_offset = line_text.chars().take(column.saturating_sub(1)).count();
                    let underline = " ".repeat(char_offset) + "^";
                    output.push_str(&format!("     | {}\n", underline));
                }
            }
        }

        // Add help lines if present in message
        let help_lines: Vec<&str> = message
            .lines()
            .skip(1)
            .filter(|l| l.trim().starts_with("help:"))
            .collect();
        if !help_lines.is_empty() {
            output.push('\n');
            for help in help_lines {
                output.push_str(&format!("{}\n", help));
            }
        }

        output
    }

    fn format_type(ty: &crate::ast::Type) -> String {
        match ty {
            crate::ast::Type::Int => "int".to_string(),
            crate::ast::Type::Str => "str".to_string(),
            crate::ast::Type::Float32 => "float32".to_string(),
            crate::ast::Type::Float64 => "float64".to_string(),
            crate::ast::Type::Bool => "bool".to_string(),
            crate::ast::Type::Dynamic => "dynamic".to_string(),
            crate::ast::Type::List(elem) => format!("list[{}]", Self::format_type(elem)),
            crate::ast::Type::Array(elem) => format!("array[{}]", Self::format_type(elem)),
            crate::ast::Type::Map(key, val) => {
                format!(
                    "map[{}, {}]",
                    Self::format_type(key),
                    Self::format_type(val)
                )
            }
            crate::ast::Type::Tensor(elem, dims) => {
                let dims_str = if dims.is_empty() {
                    String::new()
                } else {
                    format!(", {:?}", dims)
                };
                format!("Tensor[{}{}]", Self::format_type(elem), dims_str)
            }
            crate::ast::Type::Named(name) => name.clone(),
        }
    }

    pub fn format_warning(&self, warning: &Warning) -> String {
        let (message, span) = match warning {
            Warning::UnusedVariable { name, span } => {
                (format!("warning: unused variable `{}`", name), *span)
            }
            Warning::UnusedFunction { name, span } => {
                (format!("warning: unused function `{}`", name), *span)
            }
            Warning::DeadCode { span, reason } => {
                (format!("warning: dead code: {}", reason), *span)
            }
            Warning::UnreachableCode { span } => ("warning: unreachable code".to_string(), *span),
        };

        self.format_warning_with_span(&message, span)
    }

    fn format_warning_with_span(&self, message: &str, span: Span) -> String {
        let mut output = String::new();
        let line = span.line();
        let column = span.column();

        // Warning message header
        output.push_str(&format!("warning:{}:{}: {}\n", line, column, message));

        // Get context lines
        let context_lines = self.tracker.get_context(line, 2);

        // Find the line with the warning
        for (line_num, line_content) in &context_lines {
            let line_marker = if *line_num == line { ">" } else { " " };
            let line_num_str = format!("{:4}", line_num);
            output.push_str(&format!(
                "{} {} | {}\n",
                line_marker, line_num_str, line_content
            ));

            // Underline the warning span on the warning line
            if *line_num == line {
                let line_text = self.tracker.get_line(line).unwrap_or("");
                let char_offset = line_text.chars().take(column.saturating_sub(1)).count();
                let underline = " ".repeat(char_offset) + "^";
                output.push_str(&format!("     | {}\n", underline));
            }
        }

        output
    }
}
