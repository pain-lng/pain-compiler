// Standard library functions for Pain language

use crate::ast::Type;

/// Standard library function definition
#[derive(Debug, Clone)]
pub struct StdlibFunction {
    pub name: String,
    pub params: Vec<(String, Type)>, // (name, type)
    pub return_type: Type,
    pub description: String,
}

/// Get all standard library functions
#[allow(clippy::vec_init_then_push)]
pub fn get_stdlib_functions() -> Vec<StdlibFunction> {
    let mut functions = Vec::new();

    // Math functions
    functions.push(StdlibFunction {
        name: "abs".to_string(),
        params: vec![("x".to_string(), Type::Int)],
        return_type: Type::Int,
        description: "Returns the absolute value of an integer".to_string(),
    });

    functions.push(StdlibFunction {
        name: "abs".to_string(),
        params: vec![("x".to_string(), Type::Float64)],
        return_type: Type::Float64,
        description: "Returns the absolute value of a float".to_string(),
    });

    functions.push(StdlibFunction {
        name: "min".to_string(),
        params: vec![("a".to_string(), Type::Int), ("b".to_string(), Type::Int)],
        return_type: Type::Int,
        description: "Returns the minimum of two integers".to_string(),
    });

    functions.push(StdlibFunction {
        name: "max".to_string(),
        params: vec![("a".to_string(), Type::Int), ("b".to_string(), Type::Int)],
        return_type: Type::Int,
        description: "Returns the maximum of two integers".to_string(),
    });

    functions.push(StdlibFunction {
        name: "sqrt".to_string(),
        params: vec![("x".to_string(), Type::Float64)],
        return_type: Type::Float64,
        description: "Returns the square root of a number".to_string(),
    });

    functions.push(StdlibFunction {
        name: "pow".to_string(),
        params: vec![
            ("base".to_string(), Type::Float64),
            ("exp".to_string(), Type::Float64),
        ],
        return_type: Type::Float64,
        description: "Returns base raised to the power of exp".to_string(),
    });

    functions.push(StdlibFunction {
        name: "sin".to_string(),
        params: vec![("x".to_string(), Type::Float64)],
        return_type: Type::Float64,
        description: "Returns the sine of x (in radians)".to_string(),
    });

    functions.push(StdlibFunction {
        name: "cos".to_string(),
        params: vec![("x".to_string(), Type::Float64)],
        return_type: Type::Float64,
        description: "Returns the cosine of x (in radians)".to_string(),
    });

    functions.push(StdlibFunction {
        name: "floor".to_string(),
        params: vec![("x".to_string(), Type::Float64)],
        return_type: Type::Float64,
        description: "Returns the floor of x".to_string(),
    });

    functions.push(StdlibFunction {
        name: "ceil".to_string(),
        params: vec![("x".to_string(), Type::Float64)],
        return_type: Type::Float64,
        description: "Returns the ceiling of x".to_string(),
    });

    // String functions
    functions.push(StdlibFunction {
        name: "len".to_string(),
        params: vec![("s".to_string(), Type::Str)],
        return_type: Type::Int,
        description: "Returns the length of a string".to_string(),
    });

    // List/Array functions
    functions.push(StdlibFunction {
        name: "len".to_string(),
        params: vec![("list".to_string(), Type::List(Box::new(Type::Dynamic)))],
        return_type: Type::Int,
        description: "Returns the length of a list".to_string(),
    });

    functions.push(StdlibFunction {
        name: "len".to_string(),
        params: vec![("arr".to_string(), Type::Array(Box::new(Type::Dynamic)))],
        return_type: Type::Int,
        description: "Returns the length of an array".to_string(),
    });

    functions.push(StdlibFunction {
        name: "concat".to_string(),
        params: vec![("a".to_string(), Type::Str), ("b".to_string(), Type::Str)],
        return_type: Type::Str,
        description: "Concatenates two strings".to_string(),
    });

    functions.push(StdlibFunction {
        name: "substring".to_string(),
        params: vec![
            ("s".to_string(), Type::Str),
            ("start".to_string(), Type::Int),
            ("end".to_string(), Type::Int),
        ],
        return_type: Type::Str,
        description: "Returns a substring from start to end (exclusive)".to_string(),
    });

    functions.push(StdlibFunction {
        name: "contains".to_string(),
        params: vec![
            ("s".to_string(), Type::Str),
            ("substr".to_string(), Type::Str),
        ],
        return_type: Type::Bool,
        description: "Returns true if string contains substring".to_string(),
    });

    functions.push(StdlibFunction {
        name: "starts_with".to_string(),
        params: vec![
            ("s".to_string(), Type::Str),
            ("prefix".to_string(), Type::Str),
        ],
        return_type: Type::Bool,
        description: "Returns true if string starts with prefix".to_string(),
    });

    functions.push(StdlibFunction {
        name: "ends_with".to_string(),
        params: vec![
            ("s".to_string(), Type::Str),
            ("suffix".to_string(), Type::Str),
        ],
        return_type: Type::Bool,
        description: "Returns true if string ends with suffix".to_string(),
    });

    functions.push(StdlibFunction {
        name: "trim".to_string(),
        params: vec![("s".to_string(), Type::Str)],
        return_type: Type::Str,
        description: "Returns string with leading and trailing whitespace removed".to_string(),
    });

    functions.push(StdlibFunction {
        name: "to_int".to_string(),
        params: vec![("s".to_string(), Type::Str)],
        return_type: Type::Int,
        description: "Converts string to integer".to_string(),
    });

    functions.push(StdlibFunction {
        name: "to_float".to_string(),
        params: vec![("s".to_string(), Type::Str)],
        return_type: Type::Float64,
        description: "Converts string to float".to_string(),
    });

    functions.push(StdlibFunction {
        name: "to_string".to_string(),
        params: vec![("x".to_string(), Type::Int)],
        return_type: Type::Str,
        description: "Converts integer to string".to_string(),
    });

    // I/O functions
    functions.push(StdlibFunction {
        name: "print".to_string(),
        params: vec![("value".to_string(), Type::Dynamic)],
        return_type: Type::Dynamic, // Actually void, but Dynamic for now
        description: "Prints a value to stdout".to_string(),
    });

    // File I/O functions
    functions.push(StdlibFunction {
        name: "read_file".to_string(),
        params: vec![("path".to_string(), Type::Str)],
        return_type: Type::Str,
        description: "Reads the entire contents of a file as a string".to_string(),
    });

    functions.push(StdlibFunction {
        name: "write_file".to_string(),
        params: vec![
            ("path".to_string(), Type::Str),
            ("content".to_string(), Type::Str),
        ],
        return_type: Type::Dynamic, // void
        description: "Writes a string to a file".to_string(),
    });

    functions.push(StdlibFunction {
        name: "read_lines".to_string(),
        params: vec![("path".to_string(), Type::Str)],
        return_type: Type::List(Box::new(Type::Str)),
        description: "Reads all lines from a file and returns them as a list of strings".to_string(),
    });

    // Path manipulation functions
    functions.push(StdlibFunction {
        name: "path_join".to_string(),
        params: vec![
            ("parts".to_string(), Type::List(Box::new(Type::Str))),
        ],
        return_type: Type::Str,
        description: "Joins path components into a single path".to_string(),
    });

    functions.push(StdlibFunction {
        name: "path_dir".to_string(),
        params: vec![("path".to_string(), Type::Str)],
        return_type: Type::Str,
        description: "Returns the directory part of a path".to_string(),
    });

    functions.push(StdlibFunction {
        name: "path_base".to_string(),
        params: vec![("path".to_string(), Type::Str)],
        return_type: Type::Str,
        description: "Returns the base name (filename) of a path".to_string(),
    });

    functions.push(StdlibFunction {
        name: "path_ext".to_string(),
        params: vec![("path".to_string(), Type::Str)],
        return_type: Type::Str,
        description: "Returns the file extension of a path".to_string(),
    });

    // Date/time functions
    functions.push(StdlibFunction {
        name: "now".to_string(),
        params: vec![],
        return_type: Type::Float64,
        description: "Returns the current Unix timestamp (seconds since epoch)".to_string(),
    });

    functions.push(StdlibFunction {
        name: "time_format".to_string(),
        params: vec![
            ("timestamp".to_string(), Type::Float64),
            ("format".to_string(), Type::Str),
        ],
        return_type: Type::Str,
        description: "Formats a timestamp according to the given format string".to_string(),
    });

    // Regular expression functions
    functions.push(StdlibFunction {
        name: "regex_match".to_string(),
        params: vec![
            ("pattern".to_string(), Type::Str),
            ("text".to_string(), Type::Str),
        ],
        return_type: Type::Bool,
        description: "Returns true if the pattern matches the text".to_string(),
    });

    functions.push(StdlibFunction {
        name: "regex_find".to_string(),
        params: vec![
            ("pattern".to_string(), Type::Str),
            ("text".to_string(), Type::Str),
        ],
        return_type: Type::Str,
        description: "Finds the first match of the pattern in the text, returns empty string if not found".to_string(),
    });

    functions.push(StdlibFunction {
        name: "regex_find_all".to_string(),
        params: vec![
            ("pattern".to_string(), Type::Str),
            ("text".to_string(), Type::Str),
        ],
        return_type: Type::List(Box::new(Type::Str)),
        description: "Finds all matches of the pattern in the text".to_string(),
    });

    functions.push(StdlibFunction {
        name: "regex_replace".to_string(),
        params: vec![
            ("pattern".to_string(), Type::Str),
            ("text".to_string(), Type::Str),
            ("replacement".to_string(), Type::Str),
        ],
        return_type: Type::Str,
        description: "Replaces all matches of the pattern in the text with the replacement".to_string(),
    });

    // JSON functions
    functions.push(StdlibFunction {
        name: "json_parse".to_string(),
        params: vec![("json_str".to_string(), Type::Str)],
        return_type: Type::Dynamic,
        description: "Parses a JSON string and returns the corresponding value".to_string(),
    });

    functions.push(StdlibFunction {
        name: "json_stringify".to_string(),
        params: vec![("value".to_string(), Type::Dynamic)],
        return_type: Type::Str,
        description: "Converts a value to its JSON string representation".to_string(),
    });

    functions
}

/// Check if a function name is a standard library function
pub fn is_stdlib_function(name: &str) -> bool {
    matches!(
        name,
        "abs"
            | "min"
            | "max"
            | "sqrt"
            | "pow"
            | "sin"
            | "cos"
            | "floor"
            | "ceil"
            | "len"
            | "concat"
            | "substring"
            | "contains"
            | "starts_with"
            | "ends_with"
            | "trim"
            | "to_int"
            | "to_float"
            | "to_string"
            | "print"
            | "read_file"
            | "write_file"
            | "read_lines"
            | "path_join"
            | "path_dir"
            | "path_base"
            | "path_ext"
            | "now"
            | "time_format"
            | "regex_match"
            | "regex_find"
            | "regex_find_all"
            | "regex_replace"
            | "json_parse"
            | "json_stringify"
            | "push"
            | "pop"
    )
}

/// Get return type for a standard library function call
pub fn get_stdlib_return_type(name: &str, arg_types: &[Type]) -> Option<Type> {
    match name {
        // Math functions
        "abs" => {
            if arg_types.len() == 1 {
                match arg_types[0] {
                    Type::Int => Some(Type::Int),
                    Type::Float64 => Some(Type::Float64),
                    _ => None,
                }
            } else {
                None
            }
        }
        "min" | "max" => {
            if arg_types.len() == 2 && arg_types[0] == arg_types[1] {
                Some(arg_types[0].clone())
            } else {
                None
            }
        }
        "sqrt" | "sin" | "cos" | "floor" | "ceil" => {
            if arg_types.len() == 1 && arg_types[0] == Type::Float64 {
                Some(Type::Float64)
            } else {
                None
            }
        }
        "pow" => {
            if arg_types.len() == 2
                && arg_types[0] == Type::Float64
                && arg_types[1] == Type::Float64
            {
                Some(Type::Float64)
            } else {
                None
            }
        }
        // String/List/Array functions
        "len" => {
            if arg_types.len() == 1 {
                match &arg_types[0] {
                    Type::Str | Type::List(_) | Type::Array(_) => Some(Type::Int),
                    _ => None,
                }
            } else {
                None
            }
        }
        "concat" => {
            if arg_types.len() == 2 && arg_types[0] == Type::Str && arg_types[1] == Type::Str {
                Some(Type::Str)
            } else {
                None
            }
        }
        "substring" => {
            if arg_types.len() == 3
                && arg_types[0] == Type::Str
                && arg_types[1] == Type::Int
                && arg_types[2] == Type::Int
            {
                Some(Type::Str)
            } else {
                None
            }
        }
        "contains" | "starts_with" | "ends_with" => {
            if arg_types.len() == 2 && arg_types[0] == Type::Str && arg_types[1] == Type::Str {
                Some(Type::Bool)
            } else {
                None
            }
        }
        "trim" => {
            if arg_types.len() == 1 && arg_types[0] == Type::Str {
                Some(Type::Str)
            } else {
                None
            }
        }
        "to_int" => {
            if arg_types.len() == 1 && arg_types[0] == Type::Str {
                Some(Type::Int)
            } else {
                None
            }
        }
        "to_float" => {
            if arg_types.len() == 1 && arg_types[0] == Type::Str {
                Some(Type::Float64)
            } else {
                None
            }
        }
        "to_string" => {
            if arg_types.len() == 1 {
                Some(Type::Str)
            } else {
                None
            }
        }
        "print" => {
            // print accepts any arguments and returns None (void)
            Some(Type::Dynamic) // Using Dynamic as void placeholder
        }
        // File I/O functions
        "read_file" => {
            if arg_types.len() == 1 && arg_types[0] == Type::Str {
                Some(Type::Str)
            } else {
                None
            }
        }
        "write_file" => {
            if arg_types.len() == 2 && arg_types[0] == Type::Str && arg_types[1] == Type::Str {
                Some(Type::Dynamic) // void
            } else {
                None
            }
        }
        "read_lines" => {
            if arg_types.len() == 1 && arg_types[0] == Type::Str {
                Some(Type::List(Box::new(Type::Str)))
            } else {
                None
            }
        }
        // Path manipulation functions
        "path_join" => {
            if arg_types.len() == 1 {
                if let Type::List(elem) = &arg_types[0] {
                    if **elem == Type::Str {
                        return Some(Type::Str);
                    }
                }
            }
            None
        }
        "path_dir" | "path_base" | "path_ext" => {
            if arg_types.len() == 1 && arg_types[0] == Type::Str {
                Some(Type::Str)
            } else {
                None
            }
        }
        // Date/time functions
        "now" => {
            if arg_types.is_empty() {
                Some(Type::Float64)
            } else {
                None
            }
        }
        "time_format" => {
            if arg_types.len() == 2 && arg_types[0] == Type::Float64 && arg_types[1] == Type::Str {
                Some(Type::Str)
            } else {
                None
            }
        }
        // Regular expression functions
        "regex_match" => {
            if arg_types.len() == 2 && arg_types[0] == Type::Str && arg_types[1] == Type::Str {
                Some(Type::Bool)
            } else {
                None
            }
        }
        "regex_find" | "regex_replace" => {
            if arg_types.len() == 3 && arg_types[0] == Type::Str && arg_types[1] == Type::Str && arg_types[2] == Type::Str {
                Some(Type::Str)
            } else if name == "regex_find" && arg_types.len() == 2 && arg_types[0] == Type::Str && arg_types[1] == Type::Str {
                Some(Type::Str)
            } else {
                None
            }
        }
        "regex_find_all" => {
            if arg_types.len() == 2 && arg_types[0] == Type::Str && arg_types[1] == Type::Str {
                Some(Type::List(Box::new(Type::Str)))
            } else {
                None
            }
        }
        // JSON functions
        "json_parse" => {
            if arg_types.len() == 1 && arg_types[0] == Type::Str {
                Some(Type::Dynamic) // JSON can return various types
            } else {
                None
            }
        }
        "json_stringify" => {
            if arg_types.len() == 1 {
                Some(Type::Str)
            } else {
                None
            }
        }
        _ => None,
    }
}
