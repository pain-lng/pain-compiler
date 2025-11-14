// Parser module - simple recursive descent parser for MVP
// TODO: Migrate to lalrpop when token integration is resolved

use crate::ast::*;
use crate::lexer::Token;
use logos::Logos;

pub fn parse(source: &str) -> Result<Program, String> {
    let mut lexer = Token::lexer(source);
    let mut tokens = Vec::new();
    
    while let Some(token) = lexer.next() {
        match token {
            Ok(t) => tokens.push(t),
            Err(_) => return Err("Lexer error".to_string()),
        }
    }
    
    let mut parser = Parser::new(&tokens);
    parser.parse_program()
}

struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Self { tokens, pos: 0 }
    }
    
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }
    
    fn next(&mut self) -> Option<&Token> {
        let token = self.tokens.get(self.pos);
        if token.is_some() {
            self.pos += 1;
        }
        token
    }
    
    fn expect(&mut self, expected: Token) -> Result<(), String> {
        match self.next() {
            Some(token) if std::mem::discriminant(token) == std::mem::discriminant(&expected) => Ok(()),
            Some(token) => Err(format!("Expected {:?}, got {:?}", expected, token)),
            None => Err(format!("Expected {:?}, got EOF", expected)),
        }
    }
    
    fn parse_program(&mut self) -> Result<Program, String> {
        let mut items = Vec::new();
        while self.peek().is_some() {
            items.push(self.parse_item()?);
        }
        Ok(Program { items })
    }
    
    fn parse_item(&mut self) -> Result<Item, String> {
        if matches!(self.peek(), Some(Token::Fn) | Some(Token::At)) {
            Ok(Item::Function(self.parse_function()?))
        } else {
            Err("Expected function or attribute".to_string())
        }
    }
    
    fn parse_function(&mut self) -> Result<Function, String> {
        let mut attrs = Vec::new();
        while matches!(self.peek(), Some(Token::At)) {
            attrs.push(self.parse_attribute()?);
        }
        
        self.expect(Token::Fn)?;
        
        let name = match self.next() {
            Some(Token::Ident(name)) => name.clone(),
            _ => return Err("Expected function name".to_string()),
        };
        
        self.expect(Token::LParen)?;
        let params = self.parse_parameter_list()?;
        self.expect(Token::RParen)?;
        
        let return_type = if matches!(self.peek(), Some(Token::Arrow)) {
            self.next();
            Some(self.parse_type()?)
        } else {
            None
        };
        
        self.expect(Token::Colon)?;
        
        let mut body = Vec::new();
        while !matches!(self.peek(), Some(Token::Fn) | Some(Token::At) | None) {
            body.push(self.parse_statement()?);
        }
        
        Ok(Function {
            attrs,
            name,
            params,
            return_type,
            body,
        })
    }
    
    fn parse_attribute(&mut self) -> Result<Attribute, String> {
        self.expect(Token::At)?;
        let name = match self.next() {
            Some(Token::Ident(name)) => name.clone(),
            _ => return Err("Expected attribute name".to_string()),
        };
        
        let args = if matches!(self.peek(), Some(Token::LParen)) {
            self.next();
            let args = self.parse_comma_list(|p| p.parse_expr())?;
            self.expect(Token::RParen)?;
            args
        } else {
            Vec::new()
        };
        
        Ok(Attribute { name, args })
    }
    
    fn parse_parameter_list(&mut self) -> Result<Vec<Parameter>, String> {
        self.parse_comma_list(|p| p.parse_parameter())
    }
    
    fn parse_parameter(&mut self) -> Result<Parameter, String> {
        let name = match self.next() {
            Some(Token::Ident(name)) => name.clone(),
            _ => return Err("Expected parameter name".to_string()),
        };
        self.expect(Token::Colon)?;
        let ty = self.parse_type()?;
        Ok(Parameter { name, ty })
    }
    
    fn parse_type(&mut self) -> Result<Type, String> {
        match self.next() {
            Some(Token::Int) => Ok(Type::Int),
            Some(Token::Str) => Ok(Type::Str),
            Some(Token::Float32) => Ok(Type::Float32),
            Some(Token::Float64) => Ok(Type::Float64),
            Some(Token::Bool) => Ok(Type::Bool),
            Some(Token::Dynamic) => Ok(Type::Dynamic),
            Some(Token::List) => {
                self.expect(Token::LBracket)?;
                let ty = self.parse_type()?;
                self.expect(Token::RBracket)?;
                Ok(Type::List(Box::new(ty)))
            }
            Some(Token::Array) => {
                self.expect(Token::LBracket)?;
                let ty = self.parse_type()?;
                self.expect(Token::RBracket)?;
                Ok(Type::Array(Box::new(ty)))
            }
            Some(Token::Map) => {
                self.expect(Token::LBracket)?;
                let k = self.parse_type()?;
                self.expect(Token::Comma)?;
                let v = self.parse_type()?;
                self.expect(Token::RBracket)?;
                Ok(Type::Map(Box::new(k), Box::new(v)))
            }
            Some(Token::Ident(name)) => Ok(Type::Named(name.clone())),
            _ => Err("Expected type".to_string()),
        }
    }
    
    fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.peek() {
            Some(Token::Let) => {
                self.next();
                let name = match self.next() {
                    Some(Token::Ident(name)) => name.clone(),
                    _ => return Err("Expected variable name".to_string()),
                };
                let ty = if matches!(self.peek(), Some(Token::Colon)) {
                    self.next();
                    Some(self.parse_type()?)
                } else {
                    None
                };
                self.expect(Token::Assign)?;
                let init = self.parse_expr()?;
                Ok(Statement::Let {
                    mutable: false,
                    name,
                    ty,
                    init,
                })
            }
            Some(Token::Var) => {
                self.next();
                let name = match self.next() {
                    Some(Token::Ident(name)) => name.clone(),
                    _ => return Err("Expected variable name".to_string()),
                };
                let ty = if matches!(self.peek(), Some(Token::Colon)) {
                    self.next();
                    Some(self.parse_type()?)
                } else {
                    None
                };
                self.expect(Token::Assign)?;
                let init = self.parse_expr()?;
                Ok(Statement::Let {
                    mutable: true,
                    name,
                    ty,
                    init,
                })
            }
            Some(Token::Return) => {
                self.next();
                let expr = if !matches!(self.peek(), Some(Token::Semicolon) | Some(Token::Colon) | None) {
                    Some(self.parse_expr()?)
                } else {
                    None
                };
                Ok(Statement::Return(expr))
            }
            Some(Token::If) => {
                self.next();
                let cond = self.parse_expr()?;
                self.expect(Token::Colon)?;
                let mut then = Vec::new();
                while !matches!(self.peek(), Some(Token::Else) | Some(Token::Fn) | Some(Token::At) | None) {
                    then.push(self.parse_statement()?);
                }
                let else_ = if matches!(self.peek(), Some(Token::Else)) {
                    self.next();
                    self.expect(Token::Colon)?;
                    let mut else_body = Vec::new();
                    while !matches!(self.peek(), Some(Token::Fn) | Some(Token::At) | None) {
                        else_body.push(self.parse_statement()?);
                    }
                    Some(else_body)
                } else {
                    None
                };
                Ok(Statement::If { cond, then, else_ })
            }
            Some(Token::For) => {
                self.next();
                let var = match self.next() {
                    Some(Token::Ident(name)) => name.clone(),
                    _ => return Err("Expected variable name".to_string()),
                };
                self.expect(Token::In)?;
                let iter = self.parse_expr()?;
                self.expect(Token::Colon)?;
                let mut body = Vec::new();
                while !matches!(self.peek(), Some(Token::Fn) | Some(Token::At) | None) {
                    body.push(self.parse_statement()?);
                }
                Ok(Statement::For { var, iter, body })
            }
            Some(Token::While) => {
                self.next();
                let cond = self.parse_expr()?;
                self.expect(Token::Colon)?;
                let mut body = Vec::new();
                while !matches!(self.peek(), Some(Token::Fn) | Some(Token::At) | None) {
                    body.push(self.parse_statement()?);
                }
                Ok(Statement::While { cond, body })
            }
            Some(Token::Break) => {
                self.next();
                Ok(Statement::Break)
            }
            Some(Token::Continue) => {
                self.next();
                Ok(Statement::Continue)
            }
            _ => {
                let expr = self.parse_expr()?;
                if matches!(self.peek(), Some(Token::Semicolon)) {
                    self.next();
                }
                Ok(Statement::Expr(expr))
            }
        }
    }
    
    fn parse_expr(&mut self) -> Result<Expr, String> {
        self.parse_expr_or()
    }
    
    fn parse_expr_or(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_expr_and()?;
        while matches!(self.peek(), Some(Token::Or)) {
            self.next();
            let right = self.parse_expr_and()?;
            left = Expr::Or(Box::new(left), Box::new(right));
        }
        Ok(left)
    }
    
    fn parse_expr_and(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_expr_eq()?;
        while matches!(self.peek(), Some(Token::And)) {
            self.next();
            let right = self.parse_expr_eq()?;
            left = Expr::And(Box::new(left), Box::new(right));
        }
        Ok(left)
    }
    
    fn parse_expr_eq(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_expr_cmp()?;
        while matches!(self.peek(), Some(Token::EqEq) | Some(Token::Ne)) {
            let op = self.peek().unwrap().clone();
            self.next();
            let right = self.parse_expr_cmp()?;
            left = match op {
                Token::EqEq => Expr::Eq(Box::new(left), Box::new(right)),
                Token::Ne => Expr::Ne(Box::new(left), Box::new(right)),
                _ => unreachable!(),
            };
        }
        Ok(left)
    }
    
    fn parse_expr_cmp(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_expr_add()?;
        while matches!(self.peek(), Some(Token::Lt) | Some(Token::Gt) | Some(Token::Le) | Some(Token::Ge)) {
            let op = self.peek().unwrap().clone();
            self.next();
            let right = self.parse_expr_add()?;
            left = match op {
                Token::Lt => Expr::Lt(Box::new(left), Box::new(right)),
                Token::Gt => Expr::Gt(Box::new(left), Box::new(right)),
                Token::Le => Expr::Le(Box::new(left), Box::new(right)),
                Token::Ge => Expr::Ge(Box::new(left), Box::new(right)),
                _ => unreachable!(),
            };
        }
        Ok(left)
    }
    
    fn parse_expr_add(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_expr_mul()?;
        while matches!(self.peek(), Some(Token::Plus) | Some(Token::Minus)) {
            let op = self.peek().unwrap().clone();
            self.next();
            let right = self.parse_expr_mul()?;
            left = match op {
                Token::Plus => Expr::Add(Box::new(left), Box::new(right)),
                Token::Minus => Expr::Sub(Box::new(left), Box::new(right)),
                _ => unreachable!(),
            };
        }
        Ok(left)
    }
    
    fn parse_expr_mul(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_expr_unary()?;
        while matches!(self.peek(), Some(Token::Star) | Some(Token::Slash) | Some(Token::Percent)) {
            let op = self.peek().unwrap().clone();
            self.next();
            let right = self.parse_expr_unary()?;
            left = match op {
                Token::Star => Expr::Mul(Box::new(left), Box::new(right)),
                Token::Slash => Expr::Div(Box::new(left), Box::new(right)),
                Token::Percent => Expr::Mod(Box::new(left), Box::new(right)),
                _ => unreachable!(),
            };
        }
        Ok(left)
    }
    
    fn parse_expr_unary(&mut self) -> Result<Expr, String> {
        match self.peek() {
            Some(Token::Not) => {
                self.next();
                Ok(Expr::Not(Box::new(self.parse_expr_unary()?)))
            }
            Some(Token::Minus) => {
                self.next();
                Ok(Expr::Neg(Box::new(self.parse_expr_unary()?)))
            }
            Some(Token::IsInstance) => {
                self.next();
                self.expect(Token::LParen)?;
                let expr = self.parse_expr()?;
                self.expect(Token::Comma)?;
                let ty = self.parse_type()?;
                self.expect(Token::RParen)?;
                Ok(Expr::IsInstance(Box::new(expr), ty))
            }
            _ => self.parse_expr_primary(),
        }
    }
    
    fn parse_expr_primary(&mut self) -> Result<Expr, String> {
        match self.next() {
            Some(Token::Integer(n)) => Ok(Expr::Integer(*n)),
            Some(Token::Float(f)) => Ok(Expr::Float(*f)),
            Some(Token::String(s)) => Ok(Expr::String(s.clone())),
            Some(Token::FString(s)) => Ok(Expr::FString(s.clone())),
            Some(Token::True) => Ok(Expr::Bool(true)),
            Some(Token::False) => Ok(Expr::Bool(false)),
            Some(Token::None) => Ok(Expr::None),
            Some(Token::Ident(name)) => {
                let mut expr = Expr::Ident(name.clone());
                // Handle postfix operators
                while matches!(self.peek(), Some(Token::LParen) | Some(Token::LBracket) | Some(Token::Dot)) {
                    match self.peek() {
                        Some(Token::LParen) => {
                            self.next();
                            let args = self.parse_comma_list(|p| p.parse_expr())?;
                            self.expect(Token::RParen)?;
                            expr = Expr::Call {
                                callee: Box::new(expr),
                                args,
                            };
                        }
                        Some(Token::LBracket) => {
                            self.next();
                            let index = self.parse_expr()?;
                            self.expect(Token::RBracket)?;
                            expr = Expr::Index(Box::new(expr), Box::new(index));
                        }
                        Some(Token::Dot) => {
                            self.next();
                            let field = match self.next() {
                                Some(Token::Ident(name)) => name.clone(),
                                _ => return Err("Expected field name".to_string()),
                            };
                            expr = Expr::Member(Box::new(expr), field);
                        }
                        _ => break,
                    }
                }
                Ok(expr)
            }
            Some(Token::LParen) => {
                let expr = self.parse_expr()?;
                self.expect(Token::RParen)?;
                Ok(expr)
            }
            _ => Err("Expected expression".to_string()),
        }
    }
    
    fn parse_comma_list<T, F>(&mut self, mut parse_item: F) -> Result<Vec<T>, String>
    where
        F: FnMut(&mut Self) -> Result<T, String>,
    {
        let mut items = Vec::new();
        if !matches!(self.peek(), Some(Token::RParen) | Some(Token::RBracket) | None) {
            items.push(parse_item(self)?);
            while matches!(self.peek(), Some(Token::Comma)) {
                self.next();
                items.push(parse_item(self)?);
            }
        }
        Ok(items)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_function() {
        let source = "fn greet(name: str) -> str: return \"Hello\"";
        let result = parse(source);
        assert!(result.is_ok());
        let program = result.unwrap();
        assert_eq!(program.items.len(), 1);
    }

    #[test]
    fn test_parse_expression() {
        let source = "fn add(a: int, b: int) -> int: return a + b";
        let result = parse(source);
        assert!(result.is_ok());
    }
}
