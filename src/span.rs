// Source code location tracking

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub line: usize,   // 1-based line number
    pub column: usize, // 1-based column number
    pub offset: usize, // 0-based byte offset in source
}

impl Position {
    pub fn new(line: usize, column: usize, offset: usize) -> Self {
        Self {
            line,
            column,
            offset,
        }
    }

    pub fn start() -> Self {
        Self {
            line: 1,
            column: 1,
            offset: 0,
        }
    }
}

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn single(pos: Position) -> Self {
        Self {
            start: pos,
            end: pos,
        }
    }

    pub fn merge(self, other: Span) -> Self {
        Self {
            start: if self.start.offset < other.start.offset {
                self.start
            } else {
                other.start
            },
            end: if self.end.offset > other.end.offset {
                self.end
            } else {
                other.end
            },
        }
    }

    /// Get a snippet of source code for this span
    pub fn snippet<'a>(&self, source: &'a str) -> Option<&'a str> {
        if self.start.offset < source.len() && self.end.offset <= source.len() {
            Some(&source[self.start.offset..self.end.offset])
        } else {
            None
        }
    }

    /// Get the line number where this span starts
    pub fn line(&self) -> usize {
        self.start.line
    }

    /// Get the column number where this span starts
    pub fn column(&self) -> usize {
        self.start.column
    }
}

/// Helper to track position while parsing
pub struct PositionTracker {
    source: String,
    lines: Vec<usize>, // Byte offsets of line starts
}

impl PositionTracker {
    pub fn new(source: &str) -> Self {
        let mut lines = vec![0]; // First line starts at offset 0
        for (i, ch) in source.char_indices() {
            if ch == '\n' {
                lines.push(i + 1); // Next line starts after newline
            }
        }
        Self {
            source: source.to_string(),
            lines,
        }
    }

    /// Convert byte offset to Position
    pub fn position_at(&self, offset: usize) -> Position {
        // Find which line this offset is on
        let mut line = 1;
        let mut line_start = 0;

        for (i, &line_offset) in self.lines.iter().enumerate() {
            if line_offset > offset {
                break;
            }
            line = i + 1;
            line_start = line_offset;
        }

        // Calculate column (in characters, not bytes)
        let line_text = if line_start < self.source.len() {
            let line_end = if line < self.lines.len() {
                self.lines[line]
            } else {
                self.source.len()
            };
            &self.source[line_start..offset.min(line_end)]
        } else {
            ""
        };

        let column = line_text.chars().count() + 1; // 1-based

        Position::new(line, column, offset)
    }

    /// Get a line of source code (1-based)
    pub fn get_line(&self, line_num: usize) -> Option<&str> {
        if line_num == 0 || line_num > self.lines.len() {
            return None;
        }

        let start = self.lines[line_num - 1];
        let end = if line_num < self.lines.len() {
            self.lines[line_num]
        } else {
            self.source.len()
        };

        // Remove trailing newline if present
        let line = &self.source[start..end];
        Some(line.trim_end_matches('\n').trim_end_matches('\r'))
    }

    /// Get context lines around a position
    pub fn get_context(&self, line: usize, context_lines: usize) -> Vec<(usize, String)> {
        let mut result = Vec::new();
        let start_line = line.saturating_sub(context_lines);
        let end_line = (line + context_lines).min(self.lines.len());

        for i in start_line..=end_line {
            if let Some(line_text) = self.get_line(i) {
                result.push((i, line_text.to_string()));
            }
        }

        result
    }

    /// Get line start offset (0-based line number)
    pub fn line_start(&self, line: usize) -> usize {
        if line == 0 || line > self.lines.len() {
            return 0;
        }
        self.lines[line - 1]
    }
}
