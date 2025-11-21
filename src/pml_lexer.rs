// PML lexer - tokenizes PML source code

#[derive(Debug, Clone, PartialEq)]
pub enum PmlToken {
    Key(String),     // key in "key: value"
    Value(String),   // value (scalar)
    ListItem,        // "-" for list items
    Colon,           // ":"
    Comment(String), // "# comment"
    Newline,         // end of line
    Eof,             // end of file
}

/// PML lexer
pub struct PmlLexer {
    source: Vec<char>,
    position: usize,
    current_line: usize,
}

impl PmlLexer {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.chars().collect(),
            position: 0,
            current_line: 1,
        }
    }

    fn peek(&self) -> Option<char> {
        self.source.get(self.position).copied()
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.peek();
        if ch == Some('\n') {
            self.current_line += 1;
        }
        if ch.is_some() {
            self.position += 1;
        }
        ch
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            if ch == ' ' || ch == '\r' {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn read_string(&mut self) -> String {
        let mut result = String::new();
        let quote = self.advance().unwrap(); // consume opening quote
        while let Some(ch) = self.peek() {
            if ch == quote {
                self.advance(); // consume closing quote
                break;
            } else if ch == '\\' {
                self.advance(); // consume backslash
                if let Some(escaped) = self.advance() {
                    result.push(match escaped {
                        'n' => '\n',
                        't' => '\t',
                        'r' => '\r',
                        '\\' => '\\',
                        '"' => '"',
                        '\'' => '\'',
                        _ => escaped,
                    });
                }
            } else {
                result.push(ch);
                self.advance();
            }
        }
        result
    }

    fn read_until(&mut self, stop: char) -> String {
        let mut result = String::new();
        while let Some(ch) = self.peek() {
            if ch == stop {
                break;
            }
            result.push(ch);
            self.advance();
        }
        result
    }

    pub fn tokenize(&mut self) -> Vec<(PmlToken, usize)> {
        let mut tokens = Vec::new();
        let mut indent_level = 0;

        loop {
            self.skip_whitespace();

            if let Some(ch) = self.peek() {
                match ch {
                    '\t' => {
                        indent_level += 1;
                        self.advance();
                    }
                    '\n' => {
                        tokens.push((PmlToken::Newline, indent_level));
                        indent_level = 0;
                        self.advance();
                    }
                    '#' => {
                        let comment = self.read_until('\n');
                        tokens.push((PmlToken::Comment(comment), indent_level));
                        indent_level = 0;
                    }
                    '-' => {
                        self.advance();
                        // Check if it's followed by space (list item marker)
                        if self.peek() == Some(' ') {
                            self.advance(); // consume space
                            tokens.push((PmlToken::ListItem, indent_level));
                        } else {
                            // Part of a value
                            let mut value = String::from('-');
                            while let Some(c) = self.peek() {
                                if c == '\n' || c == '#' {
                                    break;
                                }
                                value.push(c);
                                self.advance();
                            }
                            tokens.push((PmlToken::Value(value.trim().to_string()), indent_level));
                        }
                    }
                    ':' => {
                        self.advance();
                        tokens.push((PmlToken::Colon, indent_level));
                    }
                    '"' | '\'' => {
                        let value = self.read_string();
                        tokens.push((PmlToken::Value(value), indent_level));
                    }
                    _ => {
                        // Read key or unquoted value
                        let text = self.read_until(':');
                        if self.peek() == Some(':') {
                            // It's a key
                            tokens.push((PmlToken::Key(text.trim().to_string()), indent_level));
                        } else {
                            // It's a value (read until newline or comment)
                            let mut value = text;
                            while let Some(c) = self.peek() {
                                if c == '\n' || c == '#' {
                                    break;
                                }
                                value.push(c);
                                self.advance();
                            }
                            tokens.push((PmlToken::Value(value.trim().to_string()), indent_level));
                        }
                    }
                }
            } else {
                tokens.push((PmlToken::Eof, 0));
                break;
            }
        }

        tokens
    }
}
