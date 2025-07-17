use crate::lexer::{Token, TokenType, Span};
use std::collections::HashMap;
use thiserror::Error;
use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Type {
    // Primitive types
    Num,      // Generic number (ntr, int, flt)
    Ntr,      // Natural number (unsigned)
    Int,      // Integer
    Flt,      // Float
    Str,      // Generic string (fst, dst)
    Fst,      // Fixed string
    Dst,      // Dynamic string
    Lst,      // Generic list (arr, vec, zip)
    Arr(Box<Type>, usize), // Fixed array [Type; size]
    Vec(Box<Type>),        // Dynamic vector [Type]
    Zip(Vec<Type>),        // Zip tuple (Type, Type, ...)
    Bit,      // Boolean
    Nil,      // Void/null
    Custom(String), // User-defined type
    Flex(Vec<Type>), // Flexible type (multiple possibilities)
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum BinaryOp {
    Add, Sub, Mul, Div, Mod,
    Eq, Ne, Lt, Le, Gt, Ge,
    And, Or,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum UnaryOp {
    Not, Neg,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expr {
    Literal(Literal),
    Identifier(String),
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        operand: Box<Expr>,
    },
    Call {
        callee: String,
        args: Vec<Expr>,
    },
    MemberAccess {
        object: Box<Expr>,
        member: String,
    },
    IndexAccess {
        object: Box<Expr>,
        index: Box<Expr>,
    },
    ArrayLiteral(Vec<Expr>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Stmt {
    VarDecl {
        name: String,
        type_annotation: Option<Type>,
        init: Expr,
        is_flex: bool,
    },
    ConstDecl {
        name: String,
        type_annotation: Option<Type>,
        init: Expr,
    },
    Assignment {
        target: String,
        value: Expr,
        is_plus_assign: bool,
    },
    If {
        condition: Expr,
        then_branch: Vec<Stmt>,
        else_ifs: Vec<(Expr, Vec<Stmt>)>,
        else_branch: Option<Vec<Stmt>>,
    },
    While {
        condition: Expr,
        body: Vec<Stmt>,
    },
    For {
        var_name: String,
        var_type: Option<Type>,
        iterable: Expr,
        body: Vec<Stmt>,
    },
    ForEach {
        var_name: String,
        var_type: Option<Type>,
        iterable: Expr,
        body: Vec<Stmt>,
    },
    Match {
        expr: Expr,
        arms: Vec<MatchArm>,
    },
    Return(Option<Expr>),
    Break,
    Continue,
    Expression(Expr),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MatchArm {
    pub pattern: Expr,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Parameter {
    pub name: String,
    pub type_annotation: Option<Type>,
    pub default_value: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FunctionDef {
    pub visibility: Option<Visibility>,
    pub name: String,
    pub return_type: Option<Type>,
    pub parameters: Vec<Parameter>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MapDef {
    pub return_type: Option<Type>,
    pub name: String,
    pub match_expr: Expr,
    pub arms: Vec<MatchArm>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FieldDef {
    pub name: String,
    pub components: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ComponentDef {
    pub name: String,
    pub required_components: Vec<String>,
    pub values: Vec<ValueDecl>,
    pub functions: Vec<FunctionDef>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ValueDecl {
    pub visibility: Option<Visibility>,
    pub name: String,
    pub type_annotation: Option<Type>,
    pub init: Option<Expr>,
    pub is_const: bool,
    pub is_flex: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Enum