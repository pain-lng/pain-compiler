// Lexer module - tokenizer using logos

use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\r\n\f]+")] // Skip whitespace
#[logos(skip r"#.*")] // Skip comments
pub enum Token {
    // Doc comments must come before String to match first
    // Doc comments (Python-style triple quotes)
    // Matches """...""" including multiline
    // Using a custom regex that handles newlines
    #[regex(r#""""[^"]*""""#, |lex| {
        let s = lex.slice();
        // Remove triple quotes (first 3 and last 3 chars)
        if s.len() >= 6 {
            let content = &s[3..s.len()-3];
            // Trim leading/trailing whitespace but preserve internal formatting
            content.trim().to_string()
        } else {
            String::new()
        }
    })]
    DocComment(String),
    // Keywords
    #[token("fn")]
    Fn,
    #[token("let")]
    Let,
    #[token("var")]
    Var,
    #[token("return")]
    Return,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("for")]
    For,
    #[token("in")]
    In,
    #[token("while")]
    While,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("class")]
    Class,
    #[token("import")]
    Import,
    #[token("async")]
    Async,
    #[token("await")]
    Await,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("None")]
    None,
    #[token("isinstance")]
    IsInstance,
    #[token("dynamic")]
    Dynamic,

    // Type keywords
    #[token("int")]
    Int,
    #[token("str")]
    Str,
    #[token("float32")]
    Float32,
    #[token("float64")]
    Float64,
    #[token("bool")]
    Bool,
    #[token("list")]
    List,
    #[token("array")]
    Array,
    #[token("map")]
    Map,
    #[token("Tensor")]
    Tensor,

    // Operators
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("==")]
    EqEq,
    #[token("!=")]
    Ne,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("<=")]
    Le,
    #[token(">=")]
    Ge,
    #[token("=")]
    Assign,
    #[token("+=")]
    PlusEq,
    #[token("-=")]
    MinusEq,
    #[token("*=")]
    StarEq,
    #[token("/=")]
    SlashEq,
    #[token("->")]
    Arrow,
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("!")]
    Not,

    // Punctuation
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token(".")]
    Dot,
    #[token("@")]
    At,

    // Literals
    #[regex(r"[0-9]+", |lex| lex.slice().parse().ok())]
    Integer(i64),
    
    #[regex(r"[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?[fF]?", |lex| {
        let s = lex.slice();
        s.trim_end_matches(['f', 'F']).parse().ok()
    })]
    Float(f64),
    
    #[regex(r#""([^"\\]|\\.)*""#, |lex| {
        let s = lex.slice();
        // Remove quotes and unescape
        s[1..s.len()-1].to_string()
    })]
    String(String),
    
    #[regex(r#"f"([^"\\]|\\.)*""#, |lex| {
        let s = lex.slice();
        // Remove f" prefix and closing quote
        s[2..s.len()-1].to_string()
    })]
    FString(String),
    
    // Identifiers
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Ident(String),
}

// Simple lexer wrapper for future indentation support
// For MVP, we use the basic Token lexer directly
// Indentation handling will be added in a future iteration
pub type Lexer<'source> = logos::Lexer<'source, Token>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keywords() {
        let mut lexer = Token::lexer("fn let var return if else");
        assert_eq!(lexer.next(), Some(Ok(Token::Fn)));
        assert_eq!(lexer.next(), Some(Ok(Token::Let)));
        assert_eq!(lexer.next(), Some(Ok(Token::Var)));
        assert_eq!(lexer.next(), Some(Ok(Token::Return)));
        assert_eq!(lexer.next(), Some(Ok(Token::If)));
        assert_eq!(lexer.next(), Some(Ok(Token::Else)));
    }

    #[test]
    fn test_operators() {
        let mut lexer = Token::lexer("+ - * / == != < >");
        assert_eq!(lexer.next(), Some(Ok(Token::Plus)));
        assert_eq!(lexer.next(), Some(Ok(Token::Minus)));
        assert_eq!(lexer.next(), Some(Ok(Token::Star)));
        assert_eq!(lexer.next(), Some(Ok(Token::Slash)));
        assert_eq!(lexer.next(), Some(Ok(Token::EqEq)));
        assert_eq!(lexer.next(), Some(Ok(Token::Ne)));
        assert_eq!(lexer.next(), Some(Ok(Token::Lt)));
        assert_eq!(lexer.next(), Some(Ok(Token::Gt)));
    }

    #[test]
    fn test_literals() {
        let mut lexer = Token::lexer("123 45.67 \"hello\" f\"world\"");
        assert_eq!(lexer.next(), Some(Ok(Token::Integer(123))));
        assert_eq!(lexer.next(), Some(Ok(Token::Float(45.67))));
        assert_eq!(lexer.next(), Some(Ok(Token::String("hello".to_string()))));
        assert_eq!(lexer.next(), Some(Ok(Token::FString("world".to_string()))));
    }

    #[test]
    fn test_identifiers() {
        let mut lexer = Token::lexer("foo bar_123 xyz");
        assert_eq!(lexer.next(), Some(Ok(Token::Ident("foo".to_string()))));
        assert_eq!(lexer.next(), Some(Ok(Token::Ident("bar_123".to_string()))));
        assert_eq!(lexer.next(), Some(Ok(Token::Ident("xyz".to_string()))));
    }

    #[test]
    fn test_function_declaration() {
        let mut lexer = Token::lexer("fn greet(name: str) -> str:");
        assert_eq!(lexer.next(), Some(Ok(Token::Fn)));
        assert_eq!(lexer.next(), Some(Ok(Token::Ident("greet".to_string()))));
        assert_eq!(lexer.next(), Some(Ok(Token::LParen)));
        assert_eq!(lexer.next(), Some(Ok(Token::Ident("name".to_string()))));
        assert_eq!(lexer.next(), Some(Ok(Token::Colon)));
        assert_eq!(lexer.next(), Some(Ok(Token::Str)));
        assert_eq!(lexer.next(), Some(Ok(Token::RParen)));
        assert_eq!(lexer.next(), Some(Ok(Token::Arrow)));
        assert_eq!(lexer.next(), Some(Ok(Token::Str)));
        assert_eq!(lexer.next(), Some(Ok(Token::Colon)));
    }

    #[test]
    fn test_comments() {
        let mut lexer = Token::lexer("fn # comment\nmain");
        assert_eq!(lexer.next(), Some(Ok(Token::Fn)));
        assert_eq!(lexer.next(), Some(Ok(Token::Ident("main".to_string()))));
    }

    #[test]
    fn test_doc_comment() {
        let mut lexer = Token::lexer(r#""""This is a doc comment."""fn"#);
        assert_eq!(lexer.next(), Some(Ok(Token::DocComment("This is a doc comment.".to_string()))));
        assert_eq!(lexer.next(), Some(Ok(Token::Fn)));
    }
}
