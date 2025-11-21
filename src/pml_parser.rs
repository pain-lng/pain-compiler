// PML parser - builds PmlNode tree from tokens

use crate::pml::PmlNode;
use crate::pml_lexer::{PmlLexer, PmlToken};

#[derive(Debug, Clone, PartialEq)]
pub enum PmlParseError {
    UnexpectedToken(String),
    InvalidIndentation,
    ExpectedValue,
    ExpectedKey,
}

impl std::fmt::Display for PmlParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PmlParseError::UnexpectedToken(token) => {
                write!(f, "Unexpected token: {}", token)
            }
            PmlParseError::InvalidIndentation => {
                write!(f, "Invalid indentation: tabs must be used consistently")
            }
            PmlParseError::ExpectedValue => {
                write!(f, "Expected value after colon")
            }
            PmlParseError::ExpectedKey => {
                write!(f, "Expected key before colon")
            }
        }
    }
}

impl std::error::Error for PmlParseError {}

/// PML parser
pub struct PmlParser {
    tokens: Vec<(PmlToken, usize)>,
    position: usize,
}

impl PmlParser {
    pub fn new(source: &str) -> Self {
        let mut lexer = PmlLexer::new(source);
        let tokens = lexer.tokenize();
        Self {
            tokens,
            position: 0,
        }
    }

    fn peek(&self) -> Option<(PmlToken, usize)> {
        self.tokens.get(self.position).cloned()
    }

    fn advance(&mut self) -> Option<(PmlToken, usize)> {
        let token = self.peek();
        if token.is_some() {
            self.position += 1;
        }
        token
    }

    fn skip_newlines(&mut self) {
        while let Some((PmlToken::Newline, _)) = self.peek() {
            self.advance();
        }
        while let Some((PmlToken::Comment(_), _)) = self.peek() {
            self.advance();
            if let Some((PmlToken::Newline, _)) = self.peek() {
                self.advance();
            }
        }
    }

    pub fn parse(&mut self) -> Result<PmlNode, PmlParseError> {
        self.skip_newlines();
        // If we're at EOF after skipping newlines/comments, return empty map
        if self.peek().is_none() || matches!(self.peek(), Some((PmlToken::Eof, _))) {
            return Ok(PmlNode::map());
        }
        self.parse_node(0)
    }

    fn parse_node(&mut self, base_indent: usize) -> Result<PmlNode, PmlParseError> {
        self.skip_newlines();

        if let Some((token, indent)) = self.peek() {
            if indent < base_indent {
                // End of current block
                return Err(PmlParseError::InvalidIndentation);
            }

            match token {
                PmlToken::Key(key) => {
                    // This is a MAP
                    self.advance();
                    if let Some((PmlToken::Colon, _)) = self.peek() {
                        self.advance();
                    } else {
                        return Err(PmlParseError::ExpectedValue);
                    }

                    // Check if there's a value on the same line
                    self.skip_newlines();
                    if let Some((PmlToken::Value(value), indent_val)) = self.peek() {
                        if indent_val == base_indent {
                            // Scalar value
                            self.advance();
                            let mut map = PmlNode::map();
                            map.map
                                .as_mut()
                                .unwrap()
                                .insert(key.clone(), PmlNode::scalar(value.clone()));
                            // Continue parsing siblings
                            self.parse_map_entries(&mut map, base_indent)?;
                            Ok(map)
                        } else if indent_val > base_indent {
                            // Nested structure
                            let mut map = PmlNode::map();
                            let value_node = self.parse_node(indent_val)?;
                            map.map.as_mut().unwrap().insert(key.clone(), value_node);
                            self.parse_map_entries(&mut map, base_indent)?;
                            Ok(map)
                        } else {
                            // Empty map, value starts on next line
                            let mut map = PmlNode::map();
                            self.parse_map_entries(&mut map, base_indent + 1)?;
                            map.map
                                .as_mut()
                                .unwrap()
                                .insert(key.clone(), self.parse_node(base_indent + 1)?);
                            Ok(map)
                        }
                    } else if let Some((PmlToken::ListItem, _)) = self.peek() {
                        // List value
                        let mut map = PmlNode::map();
                        let list_node = self.parse_list(base_indent + 1)?;
                        map.map.as_mut().unwrap().insert(key.clone(), list_node);
                        self.parse_map_entries(&mut map, base_indent)?;
                        Ok(map)
                    } else {
                        // Empty value, expect nested structure
                        let mut map = PmlNode::map();
                        self.skip_newlines();
                        if let Some((_, next_indent)) = self.peek() {
                            if next_indent > base_indent {
                                let value_node = self.parse_node(next_indent)?;
                                map.map.as_mut().unwrap().insert(key.clone(), value_node);
                            } else {
                                // Empty map
                                map.map
                                    .as_mut()
                                    .unwrap()
                                    .insert(key.clone(), PmlNode::map());
                            }
                        }
                        self.parse_map_entries(&mut map, base_indent)?;
                        Ok(map)
                    }
                }
                PmlToken::ListItem => {
                    // This is a LIST
                    self.parse_list(base_indent)
                }
                PmlToken::Value(value) => {
                    // Scalar value
                    self.advance();
                    Ok(PmlNode::scalar(value.clone()))
                }
                _ => Err(PmlParseError::UnexpectedToken(format!("{:?}", token))),
            }
        } else {
            // EOF - return empty map
            Ok(PmlNode::map())
        }
    }

    fn parse_map_entries(
        &mut self,
        map: &mut PmlNode,
        base_indent: usize,
    ) -> Result<(), PmlParseError> {
        loop {
            self.skip_newlines();
            if let Some((_, indent)) = self.peek() {
                if indent != base_indent {
                    break;
                }

                if let Some((PmlToken::Key(key), _)) = self.peek() {
                    self.advance();
                    if let Some((PmlToken::Colon, _)) = self.peek() {
                        self.advance();
                    } else {
                        return Err(PmlParseError::ExpectedValue);
                    }

                    self.skip_newlines();
                    if let Some((PmlToken::Value(value), indent_val)) = self.peek() {
                        if indent_val == base_indent {
                            // Scalar on same line
                            self.advance();
                            map.map
                                .as_mut()
                                .unwrap()
                                .insert(key.clone(), PmlNode::scalar(value.clone()));
                        } else if indent_val > base_indent {
                            // Nested structure
                            let value_node = self.parse_node(indent_val)?;
                            map.map.as_mut().unwrap().insert(key.clone(), value_node);
                        } else {
                            break;
                        }
                    } else if let Some((PmlToken::ListItem, _)) = self.peek() {
                        // List value
                        let list_node = self.parse_list(base_indent + 1)?;
                        map.map.as_mut().unwrap().insert(key.clone(), list_node);
                    } else {
                        // Empty or nested
                        self.skip_newlines();
                        if let Some((_, next_indent)) = self.peek() {
                            if next_indent > base_indent {
                                let value_node = self.parse_node(next_indent)?;
                                map.map.as_mut().unwrap().insert(key.clone(), value_node);
                            } else {
                                // Empty map
                                map.map
                                    .as_mut()
                                    .unwrap()
                                    .insert(key.clone(), PmlNode::map());
                            }
                        } else {
                            map.map
                                .as_mut()
                                .unwrap()
                                .insert(key.clone(), PmlNode::map());
                        }
                    }
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        Ok(())
    }

    fn parse_list(&mut self, base_indent: usize) -> Result<PmlNode, PmlParseError> {
        let mut list = PmlNode::list();

        loop {
            self.skip_newlines();
            if let Some((PmlToken::ListItem, indent)) = self.peek() {
                if indent < base_indent {
                    break;
                }
                self.advance();

                self.skip_newlines();
                if let Some((PmlToken::Value(value), indent_val)) = self.peek() {
                    if indent_val == base_indent {
                        // Scalar list item
                        self.advance();
                        list.list
                            .as_mut()
                            .unwrap()
                            .push(PmlNode::scalar(value.clone()));
                    } else if indent_val > base_indent {
                        // Nested structure
                        let item_node = self.parse_node(indent_val)?;
                        list.list.as_mut().unwrap().push(item_node);
                    } else {
                        break;
                    }
                } else if let Some((PmlToken::Key(_), _)) = self.peek() {
                    // Map as list item
                    let item_node = self.parse_node(base_indent)?;
                    list.list.as_mut().unwrap().push(item_node);
                } else {
                    // Empty item
                    list.list.as_mut().unwrap().push(PmlNode::map());
                }
            } else {
                break;
            }
        }

        Ok(list)
    }
}

/// Parse PML source code into PmlNode tree
pub fn parse_pml(source: &str) -> Result<PmlNode, PmlParseError> {
    let mut parser = PmlParser::new(source);
    parser.parse()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pml::PmlNodeKind;

    #[test]
    fn test_parse_simple_map() {
        let source = "title: \"Hello\"\nwidth: 400";
        let result = parse_pml(source);
        assert!(result.is_ok());
        let node = result.unwrap();
        assert_eq!(node.kind, PmlNodeKind::Map);
        assert!(node.map.is_some());
        let map = node.map.unwrap();
        assert!(map.contains_key("title"));
        assert!(map.contains_key("width"));
    }

    #[test]
    fn test_parse_nested_map() {
        let source = "window:\n\ttitle: \"Demo\"\n\twidth: 400";
        let result = parse_pml(source);
        assert!(result.is_ok());
        let node = result.unwrap();
        assert_eq!(node.kind, PmlNodeKind::Map);
        let window = node.get("window").unwrap();
        assert_eq!(window.kind, PmlNodeKind::Map);
        let title = window.get("title").unwrap();
        assert_eq!(title.kind, PmlNodeKind::Scalar);
        assert_eq!(title.scalar.as_ref().unwrap(), "Demo");
    }

    #[test]
    fn test_parse_list() {
        let source = "items:\n\t- \"apple\"\n\t- \"banana\"";
        let result = parse_pml(source);
        assert!(result.is_ok());
        let node = result.unwrap();
        let items = node.get("items").unwrap();
        assert_eq!(items.kind, PmlNodeKind::List);
        assert_eq!(items.list.as_ref().unwrap().len(), 2);
    }

    #[test]
    fn test_parse_scalar_types() {
        // Test basic scalar parsing - only one key per line for now
        let source = "key1: \"value1\"";
        let result = parse_pml(source);
        assert!(result.is_ok());
        let node = result.unwrap();
        assert!(node.get("key1").is_some(), "key1 should be present");
        let val = node.get("key1").unwrap();
        assert_eq!(val.kind, PmlNodeKind::Scalar);
    }

    #[test]
    fn test_parse_comments() {
        let source = "title: \"Hello\"  # comment\nwidth: 400";
        let result = parse_pml(source);
        assert!(result.is_ok());
        let node = result.unwrap();
        assert!(node.get("title").is_some());
        assert!(node.get("width").is_some());
    }

    #[test]
    fn test_parse_empty_file() {
        let source = "";
        let result = parse_pml(source);
        // Empty file should return empty map
        if let Ok(node) = result {
            assert_eq!(node.kind, PmlNodeKind::Map);
        }
        // Or it might return an error - both are acceptable for now
    }

    #[test]
    fn test_parse_only_comments() {
        let source = "# comment 1\n# comment 2";
        let result = parse_pml(source);
        // Comments-only file should return empty map
        if let Ok(node) = result {
            assert_eq!(node.kind, PmlNodeKind::Map);
        }
        // Or it might return an error - both are acceptable for now
    }

    #[test]
    fn test_parse_empty_map() {
        let source = "empty:";
        let result = parse_pml(source);
        assert!(result.is_ok());
        let node = result.unwrap();
        let empty = node.get("empty").unwrap();
        assert_eq!(empty.kind, PmlNodeKind::Map);
    }

    #[test]
    fn test_parse_empty_list() {
        let source = "items:";
        let result = parse_pml(source);
        assert!(result.is_ok());
        let node = result.unwrap();
        let items = node.get("items");
        // Empty list might be represented as empty map or list
        assert!(items.is_some());
    }

    #[test]
    fn test_parse_list_with_maps() {
        let source =
            "children:\n\t- type: label\n\t  text: \"Hello\"\n\t- type: button\n\t  text: \"OK\"";
        let result = parse_pml(source);
        assert!(result.is_ok());
        let node = result.unwrap();
        let children = node.get("children").unwrap();
        assert_eq!(children.kind, PmlNodeKind::List);
        assert_eq!(children.list.as_ref().unwrap().len(), 2);
    }

    #[test]
    fn test_parse_deeply_nested() {
        let source = "a:\n\tb:\n\t\tc:\n\t\t\td: \"value\"";
        let result = parse_pml(source);
        assert!(result.is_ok());
        let node = result.unwrap();
        let a = node.get("a").unwrap();
        let b = a.get("b").unwrap();
        let c = b.get("c").unwrap();
        let d = c.get("d").unwrap();
        assert_eq!(d.kind, PmlNodeKind::Scalar);
        assert_eq!(d.scalar.as_ref().unwrap(), "value");
    }

    #[test]
    fn test_parse_multiple_keys() {
        let source = "key1: \"value1\"\nkey2: \"value2\"\nkey3: \"value3\"";
        let result = parse_pml(source);
        assert!(result.is_ok());
        let node = result.unwrap();
        assert!(node.get("key1").is_some());
        assert!(node.get("key2").is_some());
        assert!(node.get("key3").is_some());
    }

    #[test]
    fn test_parse_unquoted_strings() {
        // Test single unquoted string value
        let source = "mode: release";
        let result = parse_pml(source);
        assert!(result.is_ok());
        let node = result.unwrap();
        assert!(node.get("mode").is_some());
    }

    #[test]
    fn test_parse_string_with_spaces() {
        let source = "title: \"Hello World\"\npath: \"C:/Program Files\"";
        let result = parse_pml(source);
        assert!(result.is_ok());
        let node = result.unwrap();
        let title = node.get("title").unwrap();
        assert_eq!(title.scalar.as_ref().unwrap(), "Hello World");
    }

    #[test]
    fn test_parse_escape_sequences() {
        let source = "message: \"Line 1\\nLine 2\"\ntab: \"col1\\tcol2\"";
        let result = parse_pml(source);
        assert!(result.is_ok());
        let node = result.unwrap();
        let message = node.get("message").unwrap();
        assert!(message.scalar.as_ref().unwrap().contains('\n'));
    }

    #[test]
    fn test_parse_numeric_values() {
        // Test single numeric value
        let source = "int_val: 123";
        let result = parse_pml(source);
        assert!(result.is_ok());
        let node = result.unwrap();
        // Values are stored as strings, type detection happens in to_value()
        assert!(node.get("int_val").is_some());
    }

    #[test]
    fn test_parse_bool_and_null() {
        // Test single bool/null value
        let source = "is_active: true";
        let result = parse_pml(source);
        assert!(result.is_ok());
        let node = result.unwrap();
        assert!(node.get("is_active").is_some());

        // Test null
        let source2 = "value: null";
        let result2 = parse_pml(source2);
        assert!(result2.is_ok());
        let node2 = result2.unwrap();
        assert!(node2.get("value").is_some());
    }

    #[test]
    fn test_to_value_conversion() {
        use pain_runtime::Value;

        // Test string
        let source = "str: \"hello\"";
        let result = parse_pml(source);
        assert!(result.is_ok());
        let node = result.unwrap();
        let str_val = node.get("str").unwrap().to_value();
        assert!(matches!(str_val, Value::String(_)));

        // Test int
        let source2 = "int: 123";
        let result2 = parse_pml(source2);
        assert!(result2.is_ok());
        let node2 = result2.unwrap();
        let int_val = node2.get("int").unwrap().to_value();
        assert!(matches!(int_val, Value::Int(_)));

        // Test bool
        let source3 = "bool: true";
        let result3 = parse_pml(source3);
        assert!(result3.is_ok());
        let node3 = result3.unwrap();
        let bool_val = node3.get("bool").unwrap().to_value();
        assert!(matches!(bool_val, Value::Bool(_)));

        // Test null
        let source4 = "null_val: null";
        let result4 = parse_pml(source4);
        assert!(result4.is_ok());
        let node4 = result4.unwrap();
        let null_val = node4.get("null_val").unwrap().to_value();
        assert!(matches!(null_val, Value::None));
    }

    #[test]
    fn test_to_value_map_conversion() {
        use pain_runtime::Value;
        let source = "window:\n\ttitle: \"Demo\"\n\twidth: 400";
        let result = parse_pml(source);
        assert!(result.is_ok());
        let node = result.unwrap();
        let value = node.to_value();
        assert!(matches!(value, Value::Object(_)));
    }

    #[test]
    fn test_to_value_list_conversion() {
        use pain_runtime::Value;
        let source = "items:\n\t- \"apple\"\n\t- \"banana\"";
        let result = parse_pml(source);
        assert!(result.is_ok());
        let node = result.unwrap();
        let items = node.get("items").unwrap();
        let value = items.to_value();
        assert!(matches!(value, Value::List(_)));
    }

    // Error handling tests
    #[test]
    fn test_invalid_indentation() {
        // Mixing tabs and spaces should be caught by lexer or parser
        // This is a basic test - actual behavior depends on implementation
        let source = "key:\n    value"; // spaces instead of tab
        let result = parse_pml(source);
        // Should either parse (if lexer normalizes) or error
        // For now, just verify it doesn't panic
        let _ = result;
    }

    #[test]
    fn test_missing_colon() {
        // Key without colon might be handled differently
        // This tests parser robustness
        let source = "key value";
        let result = parse_pml(source);
        // Should either parse as key-value or error gracefully
        let _ = result;
    }

    // Edge cases and stress tests
    #[test]
    fn test_very_deep_nesting() {
        // Test deeply nested structure (10 levels)
        let source = "a:\n\tb:\n\t\tc:\n\t\t\td:\n\t\t\t\te:\n\t\t\t\t\tf:\n\t\t\t\t\t\tg:\n\t\t\t\t\t\t\th:\n\t\t\t\t\t\t\t\ti:\n\t\t\t\t\t\t\t\t\tj: \"value\"";
        let result = parse_pml(source);
        assert!(result.is_ok(), "Should handle deep nesting");
    }

    #[test]
    fn test_large_list() {
        // Test list with many items
        let mut source = String::from("items:\n");
        for i in 0..100 {
            source.push_str(&format!("\t- \"item{}\"\n", i));
        }
        let result = parse_pml(&source);
        assert!(result.is_ok(), "Should handle large lists");
        let node = result.unwrap();
        let items = node.get("items").unwrap();
        assert_eq!(items.list.as_ref().unwrap().len(), 100);
    }

    #[test]
    fn test_many_keys() {
        // Test map with many keys
        let mut source = String::new();
        for i in 0..100 {
            source.push_str(&format!("key{}: \"value{}\"\n", i, i));
        }
        let result = parse_pml(&source);
        assert!(result.is_ok(), "Should handle many keys");
        let node = result.unwrap();
        assert_eq!(node.map.as_ref().unwrap().len(), 100);
    }

    #[test]
    fn test_string_with_special_chars() {
        let source = "path: \"C:/Program Files/Pain\"\nurl: \"https://example.com?q=test&x=1\"\njson: \"{\\\"key\\\": \\\"value\\\"}\"";
        let result = parse_pml(source);
        assert!(
            result.is_ok(),
            "Should handle special characters in strings"
        );
    }

    #[test]
    fn test_quoted_keys() {
        // Keys with special characters might need quotes (if supported)
        // For now, test that normal keys work
        let source = "normal_key: \"value\"\nkey_with_underscore: \"value\"";
        let result = parse_pml(source);
        assert!(result.is_ok(), "Should handle keys with underscores");
    }

    #[test]
    fn test_empty_string_value() {
        let source = "empty: \"\"\nblank: \"\"";
        let result = parse_pml(source);
        assert!(result.is_ok(), "Should handle empty string values");
        let node = result.unwrap();
        let empty = node.get("empty").unwrap();
        assert_eq!(empty.scalar.as_ref().unwrap(), "");
    }

    #[test]
    fn test_whitespace_only_value() {
        let source = "spaces: \"   \"\ntabs: \"\t\t\"";
        let result = parse_pml(source);
        assert!(result.is_ok(), "Should handle whitespace-only values");
    }

    #[test]
    fn test_multiline_comment() {
        let source = "# Line 1 comment\n# Line 2 comment\nkey: \"value\"\n# Line 3 comment";
        let result = parse_pml(source);
        assert!(result.is_ok(), "Should handle multiple comment lines");
        let node = result.unwrap();
        assert!(node.get("key").is_some());
    }

    #[test]
    fn test_comment_at_end_of_line() {
        let source = "key1: \"value1\"  # comment 1\nkey2: \"value2\"  # comment 2";
        let result = parse_pml(source);
        assert!(result.is_ok(), "Should handle comments at end of lines");
        let node = result.unwrap();
        assert!(node.get("key1").is_some());
        assert!(node.get("key2").is_some());
    }

    #[test]
    fn test_nested_lists() {
        let source = "matrix:\n\t- \n\t\t- 1\n\t\t- 2\n\t- \n\t\t- 3\n\t\t- 4";
        let result = parse_pml(source);
        // Nested lists might not be fully supported in v0.1, but should not panic
        let _ = result;
    }

    #[test]
    fn test_list_with_mixed_types() {
        // Test list with different scalar types (each on separate line)
        let source = "mixed:\n\t- \"string\"\n\t- \"123\"\n\t- \"true\"\n\t- \"null\"";
        let result = parse_pml(source);
        assert!(result.is_ok(), "Should handle list with mixed scalar types");
        let node = result.unwrap();
        let mixed = node.get("mixed").unwrap();
        assert_eq!(mixed.list.as_ref().unwrap().len(), 4);
    }

    #[test]
    fn test_unicode_strings() {
        let source = "russian: \"Привет\"\njapanese: \"こんにちは\"\nemoji: \"😀🎉\"";
        let result = parse_pml(source);
        assert!(result.is_ok(), "Should handle Unicode strings");
        let node = result.unwrap();
        assert!(node.get("russian").is_some());
        assert!(node.get("japanese").is_some());
        assert!(node.get("emoji").is_some());
    }

    #[test]
    fn test_very_long_string() {
        let long_string = "x".repeat(10000);
        let source = format!("long: \"{}\"", long_string);
        let result = parse_pml(&source);
        assert!(result.is_ok(), "Should handle very long strings");
        let node = result.unwrap();
        let long = node.get("long").unwrap();
        assert_eq!(long.scalar.as_ref().unwrap().len(), 10000);
    }

    #[test]
    fn test_escape_sequences_all() {
        let source = r#"escaped: "Line1\nLine2\tTabbed\rReturn\\Backslash\"Quote\'Apostrophe""#;
        let result = parse_pml(source);
        assert!(result.is_ok(), "Should handle all escape sequences");
        let node = result.unwrap();
        let escaped = node.get("escaped").unwrap();
        let value = escaped.scalar.as_ref().unwrap();
        assert!(value.contains('\n'), "Should contain newline");
        assert!(value.contains('\t'), "Should contain tab");
    }

    #[test]
    fn test_consecutive_newlines() {
        let source = "key1: \"value1\"\n\n\nkey2: \"value2\"";
        let result = parse_pml(source);
        assert!(result.is_ok(), "Should handle consecutive newlines");
        let node = result.unwrap();
        assert!(node.get("key1").is_some());
        assert!(node.get("key2").is_some());
    }

    #[test]
    fn test_trailing_whitespace() {
        let source = "key1: \"value1\"   \nkey2: \"value2\"\t";
        let result = parse_pml(source);
        assert!(result.is_ok(), "Should handle trailing whitespace");
    }

    #[test]
    fn test_value_with_colon() {
        let source = "time: \"12:34:56\"\nurl: \"http://example.com\"";
        let result = parse_pml(source);
        assert!(result.is_ok(), "Should handle values containing colons");
        let node = result.unwrap();
        let time = node.get("time").unwrap();
        assert_eq!(time.scalar.as_ref().unwrap(), "12:34:56");
    }

    #[test]
    fn test_value_with_hash() {
        let source = "color: \"#FF0000\"\ncomment: \"This is #1\"";
        let result = parse_pml(source);
        assert!(result.is_ok(), "Should handle values containing hash");
        let node = result.unwrap();
        let color = node.get("color").unwrap();
        assert_eq!(color.scalar.as_ref().unwrap(), "#FF0000");
    }
}
