// Parser module - simple recursive descent parser for MVP
// TODO: Migrate to lalrpop when token integration is resolved

use crate::ast::*;
use crate::lexer::{IndentLexer, Token, TokenWithSpan};
use crate::span::{Position, Span};

pub fn parse(source: &str) -> Result<Program, String> {
    // Use IndentLexer for proper indentation handling
    let mut lexer = IndentLexer::new(source);
    let mut tokens = Vec::new();

    for token in &mut lexer {
        match token {
            Ok(t) => tokens.push(t),
            Err(_) => return Err("Lexer error".to_string()),
        }
    }

    let mut parser = Parser::new(&tokens, source);
    parser.parse_program()
}

struct Parser<'a> {
    tokens: &'a [TokenWithSpan],
    _source: &'a str, // Reserved for future use (error messages with source snippets)
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [TokenWithSpan], source: &'a str) -> Self {
        Self {
            tokens,
            _source: source,
            pos: 0,
        }
    }

    fn peek(&self) -> Option<&TokenWithSpan> {
        self.tokens.get(self.pos)
    }

    fn peek_token(&self) -> Option<&Token> {
        self.peek().map(|t| &t.token)
    }

    fn next(&mut self) -> Option<&TokenWithSpan> {
        let token = self.tokens.get(self.pos);
        if token.is_some() {
            self.pos += 1;
        }
        token
    }

    fn current_span(&self) -> Span {
        if let Some(token) = self.peek() {
            token.span
        } else if let Some(last_token) = self.tokens.last() {
            last_token.span
        } else {
            Span::single(Position::start())
        }
    }

    fn expect(&mut self, expected: Token) -> Result<Span, String> {
        match self.next() {
            Some(token_span)
                if std::mem::discriminant(&token_span.token)
                    == std::mem::discriminant(&expected) =>
            {
                Ok(token_span.span)
            }
            Some(token_span) => {
                let span = token_span.span;
                Err(format!(
                    "Expected {:?}, got {:?} at {}:{}",
                    expected,
                    token_span.token,
                    span.line(),
                    span.column()
                ))
            }
            None => {
                let span = self.current_span();
                Err(format!(
                    "Expected {:?}, got EOF at {}:{}",
                    expected,
                    span.line(),
                    span.column()
                ))
            }
        }
    }

    fn parse_program(&mut self) -> Result<Program, String> {
        let start_span = if let Some(first) = self.tokens.first() {
            first.span
        } else {
            Span::single(Position::start())
        };

        let mut items = Vec::new();
        while self.peek().is_some() {
            items.push(self.parse_item()?);
        }

        let end_span = if let Some(last) = self.tokens.last() {
            last.span
        } else {
            start_span
        };

        let span = Span::new(start_span.start, end_span.end);
        Ok(Program { items, span })
    }

    fn parse_item(&mut self) -> Result<Item, String> {
        if matches!(self.peek_token(), Some(Token::Class)) {
            Ok(Item::Class(self.parse_class()?))
        } else if matches!(
            self.peek_token(),
            Some(Token::Fn) | Some(Token::At) | Some(Token::DocComment(_))
        ) {
            Ok(Item::Function(self.parse_function()?))
        } else {
            let span = self.current_span();
            Err(format!(
                "Expected function, class, or attribute at {}:{}",
                span.line(),
                span.column()
            ))
        }
    }

    fn parse_function(&mut self) -> Result<Function, String> {
        let func_start_span = self.current_span();

        // Parse doc comment if present (before attributes)
        let doc = if matches!(self.peek_token(), Some(Token::DocComment(_))) {
            if let Some(token_span) = self.next() {
                if let Token::DocComment(doc_str) = &token_span.token {
                    Some(doc_str.clone())
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };

        let mut attrs = Vec::new();
        while matches!(self.peek_token(), Some(Token::At)) {
            attrs.push(self.parse_attribute()?);
        }

        self.expect(Token::Fn)?;

        let name_span = self.current_span();
        let name = match self.next() {
            Some(token_span) if matches!(token_span.token, Token::Ident(_)) => {
                if let Token::Ident(name) = &token_span.token {
                    name.clone()
                } else {
                    return Err(format!(
                        "Expected function name at {}:{}",
                        name_span.line(),
                        name_span.column()
                    ));
                }
            }
            _ => {
                return Err(format!(
                    "Expected function name at {}:{}",
                    name_span.line(),
                    name_span.column()
                ))
            }
        };

        self.expect(Token::LParen)?;
        let params = self.parse_parameter_list()?;
        self.expect(Token::RParen)?;

        let return_type = if matches!(self.peek_token(), Some(Token::Arrow)) {
            self.next();
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(Token::Colon)?;

        // Expect Indent token after colon for function body (optional for top-level)
        let mut body = Vec::new();
        if matches!(self.peek_token(), Some(Token::Indent)) {
            self.next(); // consume Indent
                         // Parse function body until Dedent
            while !matches!(self.peek_token(), Some(Token::Dedent) | None) {
                body.push(self.parse_statement()?);
            }
            // Consume Dedent if present
            if matches!(self.peek_token(), Some(Token::Dedent)) {
                self.next();
            }
        } else {
            // No indentation - parse single statement or empty body
            if !matches!(
                self.peek_token(),
                Some(Token::Fn) | Some(Token::At) | Some(Token::DocComment(_)) | None
            ) {
                body.push(self.parse_statement()?);
            }
        }

        let func_end_span = self.current_span();

        let span = Span::new(func_start_span.start, func_end_span.end);
        Ok(Function {
            doc,
            attrs,
            name,
            params,
            return_type,
            body,
            span,
        })
    }

    fn parse_class(&mut self) -> Result<Class, String> {
        let class_start_span = self.current_span();

        // Parse doc comment if present
        let doc = if matches!(self.peek_token(), Some(Token::DocComment(_))) {
            if let Some(token_span) = self.next() {
                if let Token::DocComment(doc_str) = &token_span.token {
                    Some(doc_str.clone())
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };

        let mut attrs = Vec::new();
        while matches!(self.peek_token(), Some(Token::At)) {
            attrs.push(self.parse_attribute()?);
        }

        self.expect(Token::Class)?;

        let name_span = self.current_span();
        let name = match self.next() {
            Some(token_span) if matches!(token_span.token, Token::Ident(_)) => {
                if let Token::Ident(name) = &token_span.token {
                    name.clone()
                } else {
                    return Err(format!(
                        "Expected class name at {}:{}",
                        name_span.line(),
                        name_span.column()
                    ));
                }
            }
            _ => {
                return Err(format!(
                    "Expected class name at {}:{}",
                    name_span.line(),
                    name_span.column()
                ))
            }
        };

        self.expect(Token::Colon)?;

        // Expect Indent token after colon
        if !matches!(self.peek_token(), Some(Token::Indent)) {
            let span = self.current_span();
            return Err(format!(
                "Expected indented block after ':' at {}:{}",
                span.line(),
                span.column()
            ));
        }
        self.next(); // consume Indent

        let mut fields = Vec::new();
        let mut methods = Vec::new();

        // Parse class body until Dedent
        while !matches!(self.peek_token(), Some(Token::Dedent) | None) {
            // Check if it's a field (var/let with type annotation) or method (fn)
            if matches!(self.peek_token(), Some(Token::Var) | Some(Token::Let)) {
                fields.push(self.parse_class_field()?);
            } else if matches!(
                self.peek_token(),
                Some(Token::Fn) | Some(Token::At) | Some(Token::DocComment(_))
            ) {
                methods.push(self.parse_function()?);
            } else {
                let span = self.current_span();
                return Err(format!(
                    "Expected field or method in class body at {}:{}",
                    span.line(),
                    span.column()
                ));
            }
        }

        // Consume Dedent if present
        if matches!(self.peek_token(), Some(Token::Dedent)) {
            self.next();
        }

        let class_end_span = if let Some(last_method) = methods.last() {
            last_method.span
        } else if let Some(last_field) = fields.last() {
            last_field.span
        } else {
            self.current_span()
        };

        let span = Span::new(class_start_span.start, class_end_span.end);
        Ok(Class {
            doc,
            attrs,
            name,
            fields,
            methods,
            span,
        })
    }

    fn parse_class_field(&mut self) -> Result<ClassField, String> {
        let field_start_span = self.current_span();
        let mutable = matches!(self.peek_token(), Some(Token::Var));
        if matches!(self.peek_token(), Some(Token::Var) | Some(Token::Let)) {
            self.next();
        } else {
            let span = self.current_span();
            return Err(format!(
                "Expected 'var' or 'let' for class field at {}:{}",
                span.line(),
                span.column()
            ));
        }

        let name_span = self.current_span();
        let name = match self.next() {
            Some(token_span) if matches!(token_span.token, Token::Ident(_)) => {
                if let Token::Ident(name) = &token_span.token {
                    name.clone()
                } else {
                    return Err(format!(
                        "Expected field name at {}:{}",
                        name_span.line(),
                        name_span.column()
                    ));
                }
            }
            _ => {
                return Err(format!(
                    "Expected field name at {}:{}",
                    name_span.line(),
                    name_span.column()
                ))
            }
        };

        self.expect(Token::Colon)?;
        let ty = self.parse_type()?;

        // Optional initializer (for now, we'll skip it)
        if matches!(self.peek_token(), Some(Token::Assign)) {
            self.next();
            self.parse_expr()?; // Skip initializer for now
        }

        let field_end_span = self.current_span();
        let span = Span::new(field_start_span.start, field_end_span.end);
        Ok(ClassField {
            name,
            ty,
            mutable,
            span,
        })
    }

    fn parse_attribute(&mut self) -> Result<Attribute, String> {
        self.expect(Token::At)?;
        let name_span = self.current_span();
        let name = match self.next() {
            Some(token_span) if matches!(token_span.token, Token::Ident(_)) => {
                if let Token::Ident(name) = &token_span.token {
                    name.clone()
                } else {
                    return Err(format!(
                        "Expected attribute name at {}:{}",
                        name_span.line(),
                        name_span.column()
                    ));
                }
            }
            _ => {
                return Err(format!(
                    "Expected attribute name at {}:{}",
                    name_span.line(),
                    name_span.column()
                ))
            }
        };

        let args = if matches!(self.peek_token(), Some(Token::LParen)) {
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
        let name_span = self.current_span();
        let name = match self.next() {
            Some(token_span) if matches!(token_span.token, Token::Ident(_)) => {
                if let Token::Ident(name) = &token_span.token {
                    name.clone()
                } else {
                    return Err(format!(
                        "Expected parameter name at {}:{}",
                        name_span.line(),
                        name_span.column()
                    ));
                }
            }
            _ => {
                return Err(format!(
                    "Expected parameter name at {}:{}",
                    name_span.line(),
                    name_span.column()
                ))
            }
        };
        self.expect(Token::Colon)?;
        let ty = self.parse_type()?;
        Ok(Parameter { name, ty })
    }

    fn parse_type(&mut self) -> Result<Type, String> {
        match self.next() {
            Some(token_span) => match &token_span.token {
                Token::Int => Ok(Type::Int),
                Token::Str => Ok(Type::Str),
                Token::Float32 => Ok(Type::Float32),
                Token::Float64 => Ok(Type::Float64),
                Token::Bool => Ok(Type::Bool),
                Token::Dynamic => Ok(Type::Dynamic),
                Token::List => {
                    self.expect(Token::LBracket)?;
                    let ty = self.parse_type()?;
                    self.expect(Token::RBracket)?;
                    Ok(Type::List(Box::new(ty)))
                }
                Token::Array => {
                    self.expect(Token::LBracket)?;
                    let ty = self.parse_type()?;
                    self.expect(Token::RBracket)?;
                    Ok(Type::Array(Box::new(ty)))
                }
                Token::Map => {
                    self.expect(Token::LBracket)?;
                    let k = self.parse_type()?;
                    self.expect(Token::Comma)?;
                    let v = self.parse_type()?;
                    self.expect(Token::RBracket)?;
                    Ok(Type::Map(Box::new(k), Box::new(v)))
                }
                Token::Ident(name) => Ok(Type::Named(name.clone())),
                _ => {
                    let span = token_span.span;
                    Err(format!(
                        "Expected type at {}:{}",
                        span.line(),
                        span.column()
                    ))
                }
            },
            None => {
                let span = self.current_span();
                Err(format!(
                    "Expected type at {}:{}",
                    span.line(),
                    span.column()
                ))
            }
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.peek_token() {
            Some(Token::Let) => {
                self.next();
                let name_span = self.current_span();
                let name = match self.next() {
                    Some(token_span) if matches!(token_span.token, Token::Ident(_)) => {
                        if let Token::Ident(name) = &token_span.token {
                            name.clone()
                        } else {
                            return Err(format!(
                                "Expected variable name at {}:{}",
                                name_span.line(),
                                name_span.column()
                            ));
                        }
                    }
                    _ => {
                        return Err(format!(
                            "Expected variable name at {}:{}",
                            name_span.line(),
                            name_span.column()
                        ))
                    }
                };
                let ty = if matches!(self.peek_token(), Some(Token::Colon)) {
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
                let name_span = self.current_span();
                let name = match self.next() {
                    Some(token_span) if matches!(token_span.token, Token::Ident(_)) => {
                        if let Token::Ident(name) = &token_span.token {
                            name.clone()
                        } else {
                            return Err(format!(
                                "Expected variable name at {}:{}",
                                name_span.line(),
                                name_span.column()
                            ));
                        }
                    }
                    _ => {
                        return Err(format!(
                            "Expected variable name at {}:{}",
                            name_span.line(),
                            name_span.column()
                        ))
                    }
                };
                let ty = if matches!(self.peek_token(), Some(Token::Colon)) {
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
                let expr = if !matches!(
                    self.peek_token(),
                    Some(Token::Semicolon) | Some(Token::Colon) | None
                ) {
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

                // Expect Indent token after colon
                if !matches!(self.peek_token(), Some(Token::Indent)) {
                    let span = self.current_span();
                    return Err(format!(
                        "Expected indented block after ':' at {}:{}",
                        span.line(),
                        span.column()
                    ));
                }
                self.next(); // consume Indent

                let mut then = Vec::new();
                // Parse if body until Dedent
                while !matches!(
                    self.peek_token(),
                    Some(Token::Dedent) | Some(Token::Else) | None
                ) {
                    then.push(self.parse_statement()?);
                }

                // Consume Dedent if present
                if matches!(self.peek_token(), Some(Token::Dedent)) {
                    self.next();
                }

                let else_ = if matches!(self.peek_token(), Some(Token::Else)) {
                    self.next();
                    self.expect(Token::Colon)?;

                    // Expect Indent token after else colon
                    if !matches!(self.peek_token(), Some(Token::Indent)) {
                        let span = self.current_span();
                        return Err(format!(
                            "Expected indented block after 'else:' at {}:{}",
                            span.line(),
                            span.column()
                        ));
                    }
                    self.next(); // consume Indent

                    let mut else_body = Vec::new();
                    // Parse else body until Dedent
                    while !matches!(self.peek_token(), Some(Token::Dedent) | None) {
                        else_body.push(self.parse_statement()?);
                    }

                    // Consume Dedent if present
                    if matches!(self.peek_token(), Some(Token::Dedent)) {
                        self.next();
                    }

                    Some(else_body)
                } else {
                    None
                };
                Ok(Statement::If { cond, then, else_ })
            }
            Some(Token::For) => {
                self.next();
                let var_span = self.current_span();
                let var = match self.next() {
                    Some(token_span) if matches!(token_span.token, Token::Ident(_)) => {
                        if let Token::Ident(name) = &token_span.token {
                            name.clone()
                        } else {
                            return Err(format!(
                                "Expected variable name at {}:{}",
                                var_span.line(),
                                var_span.column()
                            ));
                        }
                    }
                    _ => {
                        return Err(format!(
                            "Expected variable name at {}:{}",
                            var_span.line(),
                            var_span.column()
                        ))
                    }
                };
                self.expect(Token::In)?;
                let iter = self.parse_expr()?;
                self.expect(Token::Colon)?;

                // Expect Indent token after colon
                if !matches!(self.peek_token(), Some(Token::Indent)) {
                    let span = self.current_span();
                    return Err(format!(
                        "Expected indented block after ':' at {}:{}",
                        span.line(),
                        span.column()
                    ));
                }
                self.next(); // consume Indent

                let mut body = Vec::new();
                // Parse for body until Dedent
                while !matches!(self.peek_token(), Some(Token::Dedent) | None) {
                    body.push(self.parse_statement()?);
                }

                // Consume Dedent if present
                if matches!(self.peek_token(), Some(Token::Dedent)) {
                    self.next();
                }

                Ok(Statement::For { var, iter, body })
            }
            Some(Token::While) => {
                self.next();
                let cond = self.parse_expr()?;
                self.expect(Token::Colon)?;

                // Expect Indent token after colon
                if !matches!(self.peek_token(), Some(Token::Indent)) {
                    let span = self.current_span();
                    return Err(format!(
                        "Expected indented block after ':' at {}:{}",
                        span.line(),
                        span.column()
                    ));
                }
                self.next(); // consume Indent

                let mut body = Vec::new();
                // Parse while body until Dedent
                while !matches!(self.peek_token(), Some(Token::Dedent) | None) {
                    body.push(self.parse_statement()?);
                }

                // Consume Dedent if present
                if matches!(self.peek_token(), Some(Token::Dedent)) {
                    self.next();
                }

                Ok(Statement::While { cond, body })
            }
            Some(Token::Break) => {
                self.next();
                if matches!(self.peek_token(), Some(Token::Semicolon)) {
                    self.next();
                }
                Ok(Statement::Break)
            }
            Some(Token::Continue) => {
                self.next();
                if matches!(self.peek_token(), Some(Token::Semicolon)) {
                    self.next();
                }
                Ok(Statement::Continue)
            }
            _ => {
                let expr = self.parse_expr()?;
                if matches!(self.peek_token(), Some(Token::Semicolon)) {
                    self.next();
                }
                Ok(Statement::Expr(expr))
            }
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, String> {
        self.parse_expr_assign()
    }

    fn parse_expr_assign(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_expr_or()?;

        // Handle assignment operators (lowest precedence)
        while matches!(
            self.peek_token(),
            Some(Token::Assign)
                | Some(Token::PlusEq)
                | Some(Token::MinusEq)
                | Some(Token::StarEq)
                | Some(Token::SlashEq)
        ) {
            let op = if let Some(token_span) = self.peek() {
                token_span.token.clone()
            } else {
                break;
            };
            self.next();
            let right = self.parse_expr_or()?;

            left = match op {
                Token::Assign => Expr::Assign(Box::new(left), Box::new(right)),
                Token::PlusEq => Expr::AddAssign(Box::new(left), Box::new(right)),
                Token::MinusEq => Expr::SubAssign(Box::new(left), Box::new(right)),
                Token::StarEq => Expr::MulAssign(Box::new(left), Box::new(right)),
                Token::SlashEq => Expr::DivAssign(Box::new(left), Box::new(right)),
                _ => unreachable!(),
            };
        }

        Ok(left)
    }

    fn parse_expr_or(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_expr_and()?;
        while matches!(self.peek_token(), Some(Token::Or)) {
            self.next();
            let right = self.parse_expr_and()?;
            left = Expr::Or(Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_expr_and(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_expr_eq()?;
        while matches!(self.peek_token(), Some(Token::And)) {
            self.next();
            let right = self.parse_expr_eq()?;
            left = Expr::And(Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_expr_eq(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_expr_cmp()?;
        while matches!(self.peek_token(), Some(Token::EqEq) | Some(Token::Ne)) {
            let op = if let Some(token_span) = self.peek() {
                token_span.token.clone()
            } else {
                break;
            };
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
        while matches!(
            self.peek_token(),
            Some(Token::Lt) | Some(Token::Gt) | Some(Token::Le) | Some(Token::Ge)
        ) {
            let op = if let Some(token_span) = self.peek() {
                token_span.token.clone()
            } else {
                break;
            };
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
        while matches!(self.peek_token(), Some(Token::Plus) | Some(Token::Minus)) {
            let op = if let Some(token_span) = self.peek() {
                token_span.token.clone()
            } else {
                break;
            };
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
        while matches!(
            self.peek_token(),
            Some(Token::Star) | Some(Token::Slash) | Some(Token::Percent)
        ) {
            let op = if let Some(token_span) = self.peek() {
                token_span.token.clone()
            } else {
                break;
            };
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
        match self.peek_token() {
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
            Some(token_span) => match &token_span.token {
                Token::New => {
                    // Parse: new ClassName(args...)
                    let class_name_span = self.current_span();
                    let class_name = match self.next() {
                        Some(token_span) if matches!(token_span.token, Token::Ident(_)) => {
                            if let Token::Ident(name) = &token_span.token {
                                name.clone()
                            } else {
                                return Err(format!(
                                    "Expected class name after 'new' at {}:{}",
                                    class_name_span.line(),
                                    class_name_span.column()
                                ));
                            }
                        }
                        _ => {
                            return Err(format!(
                                "Expected class name after 'new' at {}:{}",
                                class_name_span.line(),
                                class_name_span.column()
                            ))
                        }
                    };
                    self.expect(Token::LParen)?;
                    let args = self.parse_comma_list(|p| p.parse_expr())?;
                    self.expect(Token::RParen)?;
                    Ok(Expr::New { class_name, args })
                }
                Token::Integer(n) => Ok(Expr::Integer(*n)),
                Token::Float(f) => Ok(Expr::Float(*f)),
                Token::String(s) => Ok(Expr::String(s.clone())),
                Token::FString(s) => Ok(Expr::FString(s.clone())),
                Token::True => Ok(Expr::Bool(true)),
                Token::False => Ok(Expr::Bool(false)),
                Token::None => Ok(Expr::None),
                Token::Ident(name) => {
                    let mut expr = Expr::Ident(name.clone());
                    // Handle postfix operators
                    while matches!(
                        self.peek_token(),
                        Some(Token::LParen) | Some(Token::LBracket) | Some(Token::Dot)
                    ) {
                        match self.peek_token() {
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
                                let field_span = self.current_span();
                                let field = match self.next() {
                                    Some(token_span)
                                        if matches!(token_span.token, Token::Ident(_)) =>
                                    {
                                        if let Token::Ident(name) = &token_span.token {
                                            name.clone()
                                        } else {
                                            return Err(format!(
                                                "Expected field name at {}:{}",
                                                field_span.line(),
                                                field_span.column()
                                            ));
                                        }
                                    }
                                    _ => {
                                        return Err(format!(
                                            "Expected field name at {}:{}",
                                            field_span.line(),
                                            field_span.column()
                                        ))
                                    }
                                };
                                expr = Expr::Member(Box::new(expr), field);
                            }
                            _ => break,
                        }
                    }
                    Ok(expr)
                }
                Token::LBracket => {
                    // List literal: [expr1, expr2, ...]
                    let elements = self.parse_comma_list(|p| p.parse_expr())?;
                    self.expect(Token::RBracket)?;
                    Ok(Expr::List(elements))
                }
                Token::LParen => {
                    let expr = self.parse_expr()?;
                    self.expect(Token::RParen)?;
                    Ok(expr)
                }
                _ => {
                    let span = token_span.span;
                    Err(format!(
                        "Expected expression at {}:{}",
                        span.line(),
                        span.column()
                    ))
                }
            },
            None => {
                let span = self.current_span();
                Err(format!(
                    "Expected expression at {}:{}",
                    span.line(),
                    span.column()
                ))
            }
        }
    }

    fn parse_comma_list<T, F>(&mut self, mut parse_item: F) -> Result<Vec<T>, String>
    where
        F: FnMut(&mut Self) -> Result<T, String>,
    {
        let mut items = Vec::new();
        if !matches!(
            self.peek_token(),
            Some(Token::RParen) | Some(Token::RBracket) | None
        ) {
            items.push(parse_item(self)?);
            while matches!(self.peek_token(), Some(Token::Comma)) {
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

    #[test]
    fn test_parse_complex_expression() {
        let source = "fn test(a: int, b: int, c: int) -> int: return (a + b) * c";
        let result = parse(source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_nested_blocks() {
        let source = "fn test():
    if True:
        if False:
            return 1
        else:
            return 2
    return 3";
        let result = parse(source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_for_loop() {
        let source = "fn test():
    for item in [1, 2, 3]:
        return item";
        let result = parse(source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_class() {
        let source = "class Point:
    let x: int
    let y: int";
        let result = parse(source);
        assert!(result.is_ok());
        let program = result.unwrap();
        assert!(program
            .items
            .iter()
            .any(|item| matches!(item, Item::Class(_))));
    }

    #[test]
    fn test_parse_error_recovery() {
        let source = "fn test(
    return 42";
        let result = parse(source);
        assert!(
            result.is_err(),
            "Expected parse error for incomplete function"
        );
    }

    #[test]
    fn test_parse_multiple_functions() {
        let source = "fn func1(): return 1

fn func2(): return 2";
        let result = parse(source);
        assert!(result.is_ok());
        let program = result.unwrap();
        assert_eq!(program.items.len(), 2);
    }

    #[test]
    fn test_parse_operator_precedence() {
        let source = "fn test(a: int, b: int, c: int) -> int: return a + b * c";
        let result = parse(source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_list_literal() {
        let source = "fn test() -> list[int]: return [1, 2, 3]";
        let result = parse(source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_empty_list() {
        let source = "fn test() -> list[int]: return []";
        let result = parse(source);
        assert!(result.is_ok());
    }
}
