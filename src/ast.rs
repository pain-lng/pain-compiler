// AST module - Abstract Syntax Tree node types

use crate::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub items: Vec<Item>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Function(Function),
    Class(Class),
    // Future: Import, etc.
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub doc: Option<String>, // Doc comment (Python-style)
    pub attrs: Vec<Attribute>,
    pub name: String,
    pub params: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub body: Vec<Statement>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    pub doc: Option<String>, // Doc comment
    pub attrs: Vec<Attribute>,
    pub name: String,
    pub fields: Vec<ClassField>,
    pub methods: Vec<Function>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassField {
    pub name: String,
    pub ty: Type,
    pub mutable: bool,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    pub name: String,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Str,
    Float32,
    Float64,
    Bool,
    Dynamic,
    List(Box<Type>),
    Array(Box<Type>),
    Map(Box<Type>, Box<Type>),
    Tensor(Box<Type>, Vec<Expr>), // Tensor[f32, (N, M)]
    Named(String),                // User-defined types
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expr(Expr),
    Let {
        mutable: bool,
        name: String,
        ty: Option<Type>,
        init: Expr,
    },
    Return(Option<Expr>),
    If {
        cond: Expr,
        then: Vec<Statement>,
        else_: Option<Vec<Statement>>,
    },
    For {
        var: String,
        iter: Expr,
        body: Vec<Statement>,
    },
    While {
        cond: Expr,
        body: Vec<Statement>,
    },
    Break,
    Continue,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    // Literals
    Integer(i64),
    Float(f64),
    String(String),
    FString(String),
    Bool(bool),
    None,
    List(Vec<Expr>), // List literal: [1, 2, 3]

    // Variables
    Ident(String),

    // Binary operations
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    Ne(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>),
    Ge(Box<Expr>, Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),

    // Unary operations
    Not(Box<Expr>),
    Neg(Box<Expr>),

    // Assignment
    Assign(Box<Expr>, Box<Expr>),
    AddAssign(Box<Expr>, Box<Expr>),
    SubAssign(Box<Expr>, Box<Expr>),
    MulAssign(Box<Expr>, Box<Expr>),
    DivAssign(Box<Expr>, Box<Expr>),

    // Function call
    Call { callee: Box<Expr>, args: Vec<Expr> },

    // Indexing
    Index(Box<Expr>, Box<Expr>),

    // Member access
    Member(Box<Expr>, String),

    // Type checking
    IsInstance(Box<Expr>, Type),

    // Object creation
    New { class_name: String, args: Vec<Expr> },
}
