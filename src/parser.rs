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
pub struct EnumDef {
    pub name: String,
    pub variants: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Item {
    Function(FunctionDef),
    Map(MapDef),
    Field(FieldDef),
    Component(ComponentDef),
    Enum(EnumDef),
    Statement(Stmt),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Program {
    pub items: Vec<Item>,
}

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Unexpected token at line {line}, column {column}: expected {expected}, found {found}")]
    UnexpectedToken { line: usize, column: usize, expected: String, found: String },
    #[error("Unexpected end of input")]
    UnexpectedEof,
    #[error("Invalid type annotation: {0}")]
    InvalidType(String),
    #[error("Invalid expression")]
    InvalidExpression,
    #[error("Invalid statement")]
    InvalidStatement,
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Program, ParseError> {
        let mut items = Vec::new();
        
        while !self.is_at_end() {
            if self.match_token(&TokenType::EOF) {
                break;
            }
            items.push(self.parse_item()?);
        }
        
        Ok(Program { items })
    }

    fn parse_item(&mut self) -> Result<Item, ParseError> {
        // Check for visibility modifier
        let visibility = if self.match_token(&TokenType::Pub) {
            Some(Visibility::Public)
        } else if self.match_token(&TokenType::Prv) {
            Some(Visibility::Private)
        } else {
            None
        };

        match &self.peek().token_type {
            TokenType::Def => Ok(Item::Function(self.parse_function(visibility)?)),
            TokenType::Map => Ok(Item::Map(self.parse_map()?)),
            TokenType::Field => Ok(Item::Field(self.parse_field()?)),
            TokenType::Comp => Ok(Item::Component(self.parse_component()?)),
            TokenType::Enum => Ok(Item::Enum(self.parse_enum()?)),
            _ => Ok(Item::Statement(self.parse_statement()?)),
        }
    }

    fn parse_function(&mut self, visibility: Option<Visibility>) -> Result<FunctionDef, ParseError> {
        self.consume(&TokenType::Def, "Expected 'def'")?;
        
        // Optional return type
        let return_type = if self.check(&TokenType::Lt) {
            Some(self.parse_type_annotation()?)
        } else {
            None
        };
        
        let name = self.consume_identifier("Expected function name")?;
        
        self.consume(&TokenType::LParen, "Expected '(' after function name")?;
        
        let mut parameters = Vec::new();
        if !self.check(&TokenType::RParen) {
            loop {
                let param_name = self.consume_identifier("Expected parameter name")?;
                let param_type = if self.check(&TokenType::Lt) {
                    Some(self.parse_type_annotation()?)
                } else {
                    None
                };
                
                let default_value = if self.match_token(&TokenType::Assign) {
                    Some(self.parse_expression()?)
                } else {
                    None
                };
                
                parameters.push(Parameter {
                    name: param_name,
                    type_annotation: param_type,
                    default_value,
                });
                
                if !self.match_token(&TokenType::Comma) {
                    break;
                }
            }
        }
        
        self.consume(&TokenType::RParen, "Expected ')' after parameters")?;
        self.consume(&TokenType::Colon, "Expected ':' after function signature")?;
        
        let body = self.parse_statement_block()?;
        
        Ok(FunctionDef {
            visibility,
            name,
            return_type,
            parameters,
            body,
        })
    }

    fn parse_map(&mut self) -> Result<MapDef, ParseError> {
        self.consume(&TokenType::Map, "Expected 'map'")?;
        
        let return_type = if self.check(&TokenType::Lt) {
            Some(self.parse_type_annotation()?)
        } else {
            None
        };
        
        let name = self.consume_identifier("Expected map name")?;
        self.consume(&TokenType::Colon, "Expected ':' after map name")?;
        self.consume(&TokenType::Match, "Expected 'match' in map definition")?;
        
        let match_expr = self.parse_expression()?;
        self.consume(&TokenType::Colon, "Expected ':' after match expression")?;
        
        let mut arms = Vec::new();
        while !self.is_at_end() && !self.check(&TokenType::Def) && !self.check(&TokenType::Map) && 
              !self.check(&TokenType::Field) && !self.check(&TokenType::Comp) && !self.check(&TokenType::Enum) {
            arms.push(self.parse_match_arm()?);
        }
        
        Ok(MapDef {
            return_type,
            name,
            match_expr,
            arms,
        })
    }

    fn parse_field(&mut self) -> Result<FieldDef, ParseError> {
        self.consume(&TokenType::Field, "Expected 'field'")?;
        let name = self.consume_identifier("Expected field name")?;
        self.consume(&TokenType::Colon, "Expected ':' after field name")?;
        self.consume(&TokenType::Use, "Expected 'use' in field definition")?;
        self.consume(&TokenType::Colon, "Expected ':' after 'use'")?;
        
        let mut components = Vec::new();
        loop {
            components.push(self.consume_identifier("Expected component name")?);
            if !self.match_token(&TokenType::Comma) {
                break;
            }
        }
        
        Ok(FieldDef { name, components })
    }

    fn parse_component(&mut self) -> Result<ComponentDef, ParseError> {
        self.consume(&TokenType::Comp, "Expected 'comp'")?;
        let name = self.consume_identifier("Expected component name")?;
        self.consume(&TokenType::Colon, "Expected ':' after component name")?;
        
        let mut required_components = Vec::new();
        let mut values = Vec::new();
        let mut functions = Vec::new();
        
        // Parse req block
        if self.match_token(&TokenType::Req) {
            self.consume(&TokenType::Colon, "Expected ':' after 'req'")?;
            loop {
                required_components.push(self.consume_identifier("Expected component name")?);
                if !self.match_token(&TokenType::Comma) {
                    break;
                }
            }
        }
        
        // Parse val block
        if self.match_token(&TokenType::Val) {
            self.consume(&TokenType::Colon, "Expected ':' after 'val'")?;
            
            let val_visibility = if self.match_token(&TokenType::Pub) {
                Some(Visibility::Public)
            } else if self.match_token(&TokenType::Prv) {
                Some(Visibility::Private)
            } else {
                None
            };
            
            while !self.check(&TokenType::Impl) && !self.is_at_end() {
                let visibility = if self.match_token(&TokenType::Pub) {
                    Some(Visibility::Public)
                } else if self.match_token(&TokenType::Prv) {
                    Some(Visibility::Private)
                } else {
                    val_visibility.clone()
                };
                
                let is_const = self.match_token(&TokenType::Const);
                let is_flex = if !is_const {
                    self.match_token(&TokenType::Flex) || self.match_token(&TokenType::Var)
                } else {
                    false
                };
                
                let name = self.consume_identifier("Expected value name")?;
                let type_annotation = if self.check(&TokenType::Lt) {
                    Some(self.parse_type_annotation()?)
                } else {
                    None
                };
                
                let init = if self.match_token(&TokenType::Assign) {
                    Some(self.parse_expression()?)
                } else {
                    None
                };
                
                self.consume(&TokenType::Semicolon, "Expected ';' after value declaration")?;
                
                values.push(ValueDecl {
                    visibility,
                    name,
                    type_annotation,
                    init,
                    is_const,
                    is_flex,
                });
            }
        }
        
        // Parse impl block
        if self.match_token(&TokenType::Impl) {
            self.consume(&TokenType::Colon, "Expected ':' after 'impl'")?;
            
            let impl_visibility = if self.match_token(&TokenType::Pub) {
                Some(Visibility::Public)
            } else if self.match_token(&TokenType::Prv) {
                Some(Visibility::Private)
            } else {
                Some(Visibility::Public) // Default to public for impl
            };
            
            while !self.is_at_end() && !self.check(&TokenType::Def) && !self.check(&TokenType::Map) && 
                  !self.check(&TokenType::Field) && !self.check(&TokenType::Comp) && !self.check(&TokenType::Enum) {
                let visibility = if self.match_token(&TokenType::Pub) {
                    Some(Visibility::Public)
                } else if self.match_token(&TokenType::Prv) {
                    Some(Visibility::Private)
                } else {
                    impl_visibility.clone()
                };
                
                functions.push(self.parse_function(visibility)?);
            }
        }
        
        Ok(ComponentDef {
            name,
            required_components,
            values,
            functions,
        })
    }

    fn parse_enum(&mut self) -> Result<EnumDef, ParseError> {
        self.consume(&TokenType::Enum, "Expected 'enum'")?;
        let name = self.consume_identifier("Expected enum name")?;
        self.consume(&TokenType::Colon, "Expected ':' after enum name")?;
        
        let mut variants = Vec::new();
        loop {
            variants.push(self.consume_identifier("Expected variant name")?);
            if !self.match_token(&TokenType::Comma) {
                break;
            }
        }
        
        Ok(EnumDef { name, variants })
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        match &self.peek().token_type {
            TokenType::Var | TokenType::Flex => self.parse_var_decl(),
            TokenType::Const => self.parse_const_decl(),
            TokenType::If => self.parse_if_stmt(),
            TokenType::While => self.parse_while_stmt(),
            TokenType::For => self.parse_for_stmt(),
            TokenType::Foreach => self.parse_foreach_stmt(),
            TokenType::Match => self.parse_match_stmt(),
            TokenType::Return => self.parse_return_stmt(),
            TokenType::Break => {
                self.advance();
                self.consume(&TokenType::Semicolon, "Expected ';' after 'break'")?;
                Ok(Stmt::Break)
            }
            TokenType::Continue => {
                self.advance();
                self.consume(&TokenType::Semicolon, "Expected ';' after 'continue'")?;
                Ok(Stmt::Continue)
            }
            TokenType::Identifier(_) => {
                // Could be assignment or expression
                let checkpoint = self.current;
                let identifier = self.consume_identifier("Expected identifier")?;
                
                if self.match_token(&TokenType::Assign) || self.match_token(&TokenType::PlusAssign) {
                    let is_plus_assign = self.previous().token_type == TokenType::PlusAssign;
                    let value = self.parse_expression()?;
                    self.consume(&TokenType::Semicolon, "Expected ';' after assignment")?;
                    Ok(Stmt::Assignment {
                        target: identifier,
                        value,
                        is_plus_assign,
                    })
                } else {
                    // Reset and parse as expression
                    self.current = checkpoint;
                    let expr = self.parse_expression()?;
                    self.consume(&TokenType::Semicolon, "Expected ';' after expression")?;
                    Ok(Stmt::Expression(expr))
                }
            }
            _ => {
                let expr = self.parse_expression()?;
                self.consume(&TokenType::Semicolon, "Expected ';' after expression")?;
                Ok(Stmt::Expression(expr))
            }
        }
    }

    fn parse_var_decl(&mut self) -> Result<Stmt, ParseError> {
        let is_flex = self.match_token(&TokenType::Flex);
        if !is_flex {
            self.consume(&TokenType::Var, "Expected 'var'")?;
        }
        
        let name = self.consume_identifier("Expected variable name")?;
        let type_annotation = if self.check(&TokenType::Lt) {
            Some(self.parse_type_annotation()?)
        } else {
            None
        };
        
        self.consume(&TokenType::Assign, "Expected '=' in variable declaration")?;
        let init = self.parse_expression()?;
        self.consume(&TokenType::Semicolon, "Expected ';' after variable declaration")?;
        
        Ok(Stmt::VarDecl {
            name,
            type_annotation,
            init,
            is_flex,
        })
    }

    fn parse_const_decl(&mut self) -> Result<Stmt, ParseError> {
        self.consume(&TokenType::Const, "Expected 'const'")?;
        let name = self.consume_identifier("Expected constant name")?;
        let type_annotation = if self.check(&TokenType::Lt) {
            Some(self.parse_type_annotation()?)
        } else {
            None
        };
        
        self.consume(&TokenType::Assign, "Expected '=' in constant declaration")?;
        let init = self.parse_expression()?;
        self.consume(&TokenType::Semicolon, "Expected ';' after constant declaration")?;
        
        Ok(Stmt::ConstDecl {
            name,
            type_annotation,
            init,
        })
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.consume(&TokenType::If, "Expected 'if'")?;
        let condition = self.parse_expression()?;
        self.consume(&TokenType::Colon, "Expected ':' after if condition")?;
        let then_branch = self.parse_statement_block()?;
        
        let mut else_ifs = Vec::new();
        while self.match_token(&TokenType::Else) && self.check(&TokenType::If) {
            self.consume(&TokenType::If, "Expected 'if' after 'else'")?;
            let else_if_condition = self.parse_expression()?;
            self.consume(&TokenType::Colon, "Expected ':' after else if condition")?;
            let else_if_body = self.parse_statement_block()?;
            else_ifs.push((else_if_condition, else_if_body));
        }
        
        let else_branch = if self.match_token(&TokenType::Else) {
            self.consume(&TokenType::Colon, "Expected ':' after 'else'")?;
            Some(self.parse_statement_block()?)
        } else {
            None
        };
        
        Ok(Stmt::If {
            condition,
            then_branch,
            else_ifs,
            else_branch,
        })
    }

    fn parse_while_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.consume(&TokenType::While, "Expected 'while'")?;
        let condition = self.parse_expression()?;
        self.consume(&TokenType::Colon, "Expected ':' after while condition")?;
        let body = self.parse_statement_block()?;
        
        Ok(Stmt::While { condition, body })
    }

    fn parse_for_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.consume(&TokenType::For, "Expected 'for'")?;
        
        let is_flex = self.match_token(&TokenType::Flex);
        if !is_flex {
            self.consume(&TokenType::Var, "Expected 'var' or 'flex'")?;
        }
        
        let var_name = self.consume_identifier("Expected variable name")?;
        let var_type = if self.check(&TokenType::Lt) {
            Some(self.parse_type_annotation()?)
        } else {
            None
        };
        
        self.consume(&TokenType::In, "Expected 'in' in for loop")?;
        let iterable = self.parse_expression()?;
        self.consume(&TokenType::Colon, "Expected ':' after for expression")?;
        let body = self.parse_statement_block()?;
        
        Ok(Stmt::For {
            var_name,
            var_type,
            iterable,
            body,
        })
    }

    fn parse_foreach_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.consume(&TokenType::Foreach, "Expected 'foreach'")?;
        
        let is_flex = self.match_token(&TokenType::Flex);
        if !is_flex {
            self.consume(&TokenType::Var, "Expected 'var' or 'flex'")?;
        }
        
        let var_name = self.consume_identifier("Expected variable name")?;
        let var_type = if self.check(&TokenType::Lt) {
            Some(self.parse_type_annotation()?)
        } else {
            None
        };
        
        self.consume(&TokenType::In, "Expected 'in' in foreach loop")?;
        let iterable = self.parse_expression()?;
        self.consume(&TokenType::Colon, "Expected ':' after foreach expression")?;
        let body = self.parse_statement_block()?;
        
        Ok(Stmt::ForEach {
            var_name,
            var_type,
            iterable,
            body,
        })
    }

    fn parse_match_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.consume(&TokenType::Match, "Expected 'match'")?;
        let expr = self.parse_expression()?;
        self.consume(&TokenType::Colon, "Expected ':' after match expression")?;
        
        let mut arms = Vec::new();
        while !self.is_at_end() && !self.check(&TokenType::Def) && !self.check(&TokenType::Map) && 
              !self.check(&TokenType::Field) && !self.check(&TokenType::Comp) && !self.check(&TokenType::Enum) {
            arms.push(self.parse_match_arm()?);
        }
        
        Ok(Stmt::Match { expr, arms })
    }

    fn parse_match_arm(&mut self) -> Result<MatchArm, ParseError> {
        let pattern = self.parse_expression()?;
        self.consume(&TokenType::Arrow, "Expected '=>' in match arm")?;
        
        let body = if self.match_token(&TokenType::LBrace) {
            let stmts = self.parse_statement_block()?;
            self.consume(&TokenType::RBrace, "Expected '}' after match arm block")?;
            stmts
        } else {
            vec![Stmt::Expression(self.parse_expression()?)]
        };
        
        self.match_token(&TokenType::Comma); // Optional comma
        
        Ok(MatchArm { pattern, body })
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.consume(&TokenType::Return, "Expected 'return'")?;
        
        let value = if self.check(&TokenType::Semicolon) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        
        self.consume(&TokenType::Semicolon, "Expected ';' after return statement")?;
        Ok(Stmt::Return(value))
    }

    fn parse_statement_block(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = Vec::new();
        
        while !self.is_at_end() && !self.check(&TokenType::Else) && !self.check(&TokenType::RBrace) {
            // Check for end of block conditions
            if self.check(&TokenType::Def) || self.check(&TokenType::Map) || 
               self.check(&TokenType::Field) || self.check(&TokenType::Comp) || 
               self.check(&TokenType::Enum) {
                break;
            }
            
            statements.push(self.parse_statement()?);
        }
        
        Ok(statements)
    }

    fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_or()
    }

    fn parse_or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_and()?;
        
        while self.match_token(&TokenType::Or) {
            let right = self.parse_and()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: BinaryOp::Or,
                right: Box::new(right),
            };
        }
        
        Ok(expr)
    }

    fn parse_and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_equality()?;
        
        while self.match_token(&TokenType::And) {
            let right = self.parse_equality()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: BinaryOp::And,
                right: Box::new(right),
            };
        }
        
        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_comparison()?;
        
        while let Some(op) = self.match_equality_op() {
            let right = self.parse_comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }
        
        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_term()?;
        
        while let Some(op) = self.match_comparison_op() {
            let right = self.parse_term()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }
        
        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_factor()?;
        
        while let Some(op) = self.match_term_op() {
            let right = self.parse_factor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }
        
        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_unary()?;
        
        while let Some(op) = self.match_factor_op() {
            let right = self.parse_unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }
        
        Ok(expr)
    }

    // Add these methods to the impl Parser block

fn parse_unary(&mut self) -> Result<Expr, ParseError> {
    if self.match_token(&TokenType::Not) {
        let operand = self.parse_unary()?;
        Ok(Expr::Unary {
            op: UnaryOp::Not,
            operand: Box::new(operand),
        })
    } else if self.match_token(&TokenType::Minus) {
        let operand = self.parse_unary()?;
        Ok(Expr::Unary {
            op: UnaryOp::Neg,
            operand: Box::new(operand),
        })
    } else {
        self.parse_primary()
    }
}

fn parse_primary(&mut self) -> Result<Expr, ParseError> {
    match &self.peek().token_type {
        TokenType::Number(n) => {
            self.advance();
            Ok(Expr::Literal(Literal::Number(*n)))
        }
        TokenType::String(s) => {
            self.advance();
            Ok(Expr::Literal(Literal::String(s.clone())))
        }
        TokenType::Boolean(b) => {
            self.advance();
            Ok(Expr::Literal(Literal::Boolean(*b)))
        }
        TokenType::Nil => {
            self.advance();
            Ok(Expr::Literal(Literal::Nil))
        }
        TokenType::Identifier(_) => {
            let name = self.consume_identifier("Expected identifier")?;
            
            // Check if this is a function call
            if self.match_token(&TokenType::LParen) {
                let mut args = Vec::new();
                if !self.check(&TokenType::RParen) {
                    loop {
                        args.push(self.parse_expression()?);
                        if !self.match_token(&TokenType::Comma) {
                            break;
                        }
                    }
                }
                self.consume(&TokenType::RParen, "Expected ')' after arguments")?;
                Ok(Expr::Call { callee: name, args })
            } else {
                Ok(Expr::Identifier(name))
            }
        }
        TokenType::LParen => {
            self.advance();
            let expr = self.parse_expression()?;
            self.consume(&TokenType::RParen, "Expected ')' after expression")?;
            Ok(expr)
        }
        TokenType::LBracket => {
            self.advance();
            let mut elements = Vec::new();
            if !self.check(&TokenType::RBracket) {
                loop {
                    elements.push(self.parse_expression()?);
                    if !self.match_token(&TokenType::Comma) {
                        break;
                    }
                }
            }
            self.consume(&TokenType::RBracket, "Expected ']' after array elements")?;
            Ok(Expr::ArrayLiteral(elements))
        }
        _ => Err(ParseError::InvalidExpression),
    }
}

fn parse_type_annotation(&mut self) -> Result<Type, ParseError> {
    self.consume(&TokenType::Lt, "Expected '<' for type annotation")?;
    
    let ty = match &self.peek().token_type {
        TokenType::Arr => {
            self.advance();
            self.consume(&TokenType::LBracket, "Expected '[' after 'arr'")?;
            let element_type = Box::new(self.parse_type_annotation()?);
            self.consume(&TokenType::Semicolon, "Expected ';' in array type")?;
            let size = if let TokenType::Number(n) = &self.peek().token_type {
                let size = *n as usize;
                self.advance();
                size
            } else {
                return Err(ParseError::InvalidType("Expected array size".to_string()));
            };
            self.consume(&TokenType::RBracket, "Expected ']' after array size")?;
            Type::Arr(element_type, size)
        }
        TokenType::Vec => {
            self.advance();
            self.consume(&TokenType::LBracket, "Expected '[' after 'vec'")?;
            let element_type = Box::new(self.parse_type_annotation()?);
            self.consume(&TokenType::RBracket, "Expected ']' after vector type")?;
            Type::Vec(element_type)
        }
        TokenType::Zip => {
            self.advance();
            self.consume(&TokenType::LParen, "Expected '(' after 'zip'")?;
            let mut types = Vec::new();
            if !self.check(&TokenType::RParen) {
                loop {
                    types.push(self.parse_type_annotation()?);
                    if !self.match_token(&TokenType::Comma) {
                        break;
                    }
                }
            }
            self.consume(&TokenType::RParen, "Expected ')' after zip types")?;
            Type::Zip(types)
        }
        TokenType::Num => { self.advance(); Type::Num }
        TokenType::Ntr => { self.advance(); Type::Ntr }
        TokenType::Int => { self.advance(); Type::Int }
        TokenType::Flt => { self.advance(); Type::Flt }
        TokenType::Str => { self.advance(); Type::Str }
        TokenType::Fst => { self.advance(); Type::Fst }
        TokenType::Dst => { self.advance(); Type::Dst }
        TokenType::Lst => { self.advance(); Type::Lst }
        TokenType::Bit => { self.advance(); Type::Bit }
        TokenType::Nil => { self.advance(); Type::Nil }
        TokenType::Identifier(name) => {
            self.advance();
            Type::Custom(name.clone())
        }
        _ => return Err(ParseError::InvalidType("Expected valid type".to_string())),
    };
    
    self.consume(&TokenType::Gt, "Expected '>' after type annotation")?;
    Ok(ty)
}

    // Utility methods needed:

    fn match_token(&mut self, token_type: &TokenType) -> bool {
        if self.check(token_type) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            &self.peek().token_type == token_type
        }
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::EOF
    }

    fn consume(&mut self, token_type: &TokenType, message: &str) -> Result<(), ParseError> {
        if self.check(token_type) {
            self.advance();
            Ok(())
        } else {
            let token = self.peek();
            Err(ParseError::UnexpectedToken {
                line: token.span.line,
                column: token.span.column,
                expected: format!("{:?}", token_type),
                found: format!("{:?}", token.token_type),
            })
        }
    }

    fn consume_identifier(&mut self, message: &str) -> Result<String, ParseError> {
        if let TokenType::Identifier(name) = &self.peek().token_type {
            let name = name.clone();
            self.advance();
            Ok(name)
        } else {
            let token = self.peek();
            Err(ParseError::UnexpectedToken {
                line: token.span.line,
                column: token.span.column,
                expected: "identifier".to_string(),
                found: format!("{:?}", token.token_type),
            })
        }
    }

// Operator matching helpers:

    fn match_equality_op(&mut self) -> Option<BinaryOp> {
        if self.match_token(&TokenType::Eq) {
            Some(BinaryOp::Eq)
        } else if self.match_token(&TokenType::Ne) {
            Some(BinaryOp::Ne)
        } else {
            None
        }
    }

    fn match_comparison_op(&mut self) -> Option<BinaryOp> {
        if self.match_token(&TokenType::Lt) {
            Some(BinaryOp::Lt)
        } else if self.match_token(&TokenType::Le) {
            Some(BinaryOp::Le)
        } else if self.match_token(&TokenType::Gt) {
            Some(BinaryOp::Gt)
        } else if self.match_token(&TokenType::Ge) {
            Some(BinaryOp::Ge)
        } else {
            None
        }
    }

    fn match_term_op(&mut self) -> Option<BinaryOp> {
        if self.match_token(&TokenType::Plus) {
            Some(BinaryOp::Add)
        } else if self.match_token(&TokenType::Minus) {
            Some(BinaryOp::Sub)
        } else {
            None
        }
    }

    fn match_factor_op(&mut self) -> Option<BinaryOp> {
        if self.match_token(&TokenType::Multiply) {
            Some(BinaryOp::Mul)
        } else if self.match_token(&TokenType::Divide) {
            Some(BinaryOp::Div)
        } else if self.match_token(&TokenType::Modulo) {
            Some(BinaryOp::Mod)
        } else {
            None
        }
    }
}