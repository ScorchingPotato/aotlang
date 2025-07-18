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
    MemberCall {
        object: Box<Expr>,
        method: String,
        args: Vec<Expr>,
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
    MemberAssignment {
        object: Expr,
        member: String,
        value: Expr,
        is_plus_assign: bool,
    },
    IndexAssignment {
        object: Expr,
        index: Expr,
        value: Expr,
        is_plus_assign: bool,
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
    Field(FieldDef),
    Component(ComponentDef),
    Enum(EnumDef),
    Statement(Stmt),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Program {
    pub items: Vec<Item>,
}

use std::fmt::Write;

impl Program {
    pub fn print(&self) -> String {
        let mut output = String::new();
        writeln!(output, "Program {{").unwrap();
        
        for (i, item) in self.items.iter().enumerate() {
            writeln!(output, "  Item[{}]:", i).unwrap();
            print_item(&mut output, item, 2);
        }
        
        writeln!(output, "}}").unwrap();
        output
    }
}

fn print_item(output: &mut String, item: &Item, indent: usize) {
    let spaces = "  ".repeat(indent);
    
    match item {
        Item::Function(func) => {
            writeln!(output, "{}Function {{", spaces).unwrap();
            writeln!(output, "{}  name: \"{}\"", spaces, func.name).unwrap();
            writeln!(output, "{}  visibility: {:?}", spaces, func.visibility).unwrap();
            writeln!(output, "{}  return_type: {:?}", spaces, func.return_type).unwrap();
            writeln!(output, "{}  parameters: [", spaces).unwrap();
            for param in &func.parameters {
                writeln!(output, "{}    Parameter {{ name: \"{}\", type: {:?}, default: {:?} }}", 
                    spaces, param.name, param.type_annotation, param.default_value).unwrap();
            }
            writeln!(output, "{}  ]", spaces).unwrap();
            writeln!(output, "{}  body: [", spaces).unwrap();
            for stmt in &func.body {
                print_stmt(output, stmt, indent + 3);
            }
            writeln!(output, "{}  ]", spaces).unwrap();
            writeln!(output, "{}}}", spaces).unwrap();
        }
        Item::Field(field) => {
            writeln!(output, "{}Field {{", spaces).unwrap();
            writeln!(output, "{}  name: \"{}\"", spaces, field.name).unwrap();
            writeln!(output, "{}  components: {:?}", spaces, field.components).unwrap();
            writeln!(output, "{}}}", spaces).unwrap();
        }
        Item::Component(comp) => {
            writeln!(output, "{}Component {{", spaces).unwrap();
            writeln!(output, "{}  name: \"{}\"", spaces, comp.name).unwrap();
            writeln!(output, "{}  required_components: {:?}", spaces, comp.required_components).unwrap();
            writeln!(output, "{}  values: [", spaces).unwrap();
            for value in &comp.values {
                writeln!(output, "{}    ValueDecl {{ name: \"{}\", visibility: {:?} , type: {:?}, is_const: {}, is_flex: {} }}", 
                    spaces, value.name, value.visibility, value.type_annotation, value.is_const, value.is_flex).unwrap();
            }
            writeln!(output, "{}  ]", spaces).unwrap();
            writeln!(output, "{}  functions: [", spaces).unwrap();
            for func in &comp.functions {
                writeln!(output, "{}    Function {{ name: \"{}\", visibility: {:?} , return_type: {:?}", spaces, func.name, func.visibility, func.return_type).unwrap();
                writeln!(output, "{}      parameters: [", spaces).unwrap();
                for param in &func.parameters {
                    writeln!(output, "{}        Parameter {{ name: \"{}\", type: {:?}, default: {:?} }}", 
                        spaces, param.name, param.type_annotation, param.default_value).unwrap();
                }
                writeln!(output, "{}      ]", spaces).unwrap();
                writeln!(output, "{}      body: [", spaces).unwrap();
                for stmt in &func.body {
                    print_stmt(output, stmt, indent + 3);
                }
                writeln!(output, "{}      ]", spaces).unwrap();
                writeln!(output, "{}    }}", spaces).unwrap();
            }
            writeln!(output, "{}  ]", spaces).unwrap();
            writeln!(output, "{}}}", spaces).unwrap();
        }
        Item::Enum(enum_def) => {
            writeln!(output, "{}Enum {{", spaces).unwrap();
            writeln!(output, "{}  name: \"{}\"", spaces, enum_def.name).unwrap();
            writeln!(output, "{}  variants: {:?}", spaces, enum_def.variants).unwrap();
            writeln!(output, "{}}}", spaces).unwrap();
        }
        Item::Statement(stmt) => {
            writeln!(output, "{}Statement:", spaces).unwrap();
            print_stmt(output, stmt, indent + 1);
        }
    }
}

fn print_stmt(output: &mut String, stmt: &Stmt, indent: usize) {
    let spaces = "  ".repeat(indent);
    
    match stmt {
        Stmt::VarDecl { name, type_annotation, init, is_flex } => {
            writeln!(output, "{}VarDecl {{ name: \"{}\", type: {:?}, is_flex: {}, init: {} }}", 
                spaces, name, type_annotation, is_flex, expr_to_string(init)).unwrap();
        }
        Stmt::ConstDecl { name, type_annotation, init } => {
            writeln!(output, "{}ConstDecl {{ name: \"{}\", type: {:?}, init: {} }}", 
                spaces, name, type_annotation, expr_to_string(init)).unwrap();
        }
        Stmt::Assignment { target, value, is_plus_assign } => {
            let op = if *is_plus_assign { "+=" } else { "=" };
            writeln!(output, "{}Assignment {{ target: \"{}\", op: \"{}\", value: {} }}", 
                spaces, target, op, expr_to_string(value)).unwrap();
        }
        Stmt::MemberAssignment { object, member, value, is_plus_assign } => {
            let op = if *is_plus_assign { "+=" } else { "=" };
            writeln!(output, "{}MemberAssignment {{ object: {}, member: \"{}\", op: \"{}\", value: {} }}", 
                spaces, expr_to_string(object), member, op, expr_to_string(value)).unwrap();
        }
        Stmt::IndexAssignment { object, index, value, is_plus_assign } => {
            let op = if *is_plus_assign { "+=" } else { "=" };
            writeln!(output, "{}IndexAssignment {{ object: {}, index: {}, op: \"{}\", value: {} }}", 
                spaces, expr_to_string(object), expr_to_string(index), op, expr_to_string(value)).unwrap();
        }
        Stmt::If { condition, then_branch, else_ifs, else_branch } => {
            writeln!(output, "{}If {{", spaces).unwrap();
            writeln!(output, "{}  condition: {}", spaces, expr_to_string(condition)).unwrap();
            writeln!(output, "{}  then_branch: [", spaces).unwrap();
            for stmt in then_branch {
                print_stmt(output, stmt, indent + 2);
            }
            writeln!(output, "{}  ]", spaces).unwrap();
            if !else_ifs.is_empty() {
                writeln!(output, "{}  else_ifs: [", spaces).unwrap();
                for (cond, body) in else_ifs {
                    writeln!(output, "{}    ElseIf {{ condition: {}, body: [...] }}", spaces, expr_to_string(cond)).unwrap();
                }
                writeln!(output, "{}  ]", spaces).unwrap();
            }
            if let Some(else_body) = else_branch {
                writeln!(output, "{}  else_branch: [", spaces).unwrap();
                for stmt in else_body {
                    print_stmt(output, stmt, indent + 2);
                }
                writeln!(output, "{}  ]", spaces).unwrap();
            }
            writeln!(output, "{}}}", spaces).unwrap();
        }
        Stmt::While { condition, body } => {
            writeln!(output, "{}While {{", spaces).unwrap();
            writeln!(output, "{}  condition: {}", spaces, expr_to_string(condition)).unwrap();
            writeln!(output, "{}  body: [", spaces).unwrap();
            for stmt in body {
                print_stmt(output, stmt, indent + 2);
            }
            writeln!(output, "{}  ]", spaces).unwrap();
            writeln!(output, "{}}}", spaces).unwrap();
        }
        Stmt::For { var_name, var_type, iterable, body } => {
            writeln!(output, "{}For {{", spaces).unwrap();
            writeln!(output, "{}  var_name: \"{}\", var_type: {:?}", spaces, var_name, var_type).unwrap();
            writeln!(output, "{}  iterable: {}", spaces, expr_to_string(iterable)).unwrap();
            writeln!(output, "{}  body: [", spaces).unwrap();
            for stmt in body {
                print_stmt(output, stmt, indent + 2);
            }
            writeln!(output, "{}  ]", spaces).unwrap();
            writeln!(output, "{}}}", spaces).unwrap();
        }
        Stmt::ForEach { var_name, var_type, iterable, body } => {
            writeln!(output, "{}ForEach {{", spaces).unwrap();
            writeln!(output, "{}  var_name: \"{}\", var_type: {:?}", spaces, var_name, var_type).unwrap();
            writeln!(output, "{}  iterable: {}", spaces, expr_to_string(iterable)).unwrap();
            writeln!(output, "{}  body: [", spaces).unwrap();
            for stmt in body {
                print_stmt(output, stmt, indent + 2);
            }
            writeln!(output, "{}  ]", spaces).unwrap();
            writeln!(output, "{}}}", spaces).unwrap();
        }
        Stmt::Match { expr, arms } => {
            writeln!(output, "{}Match {{", spaces).unwrap();
            writeln!(output, "{}  expr: {}", spaces, expr_to_string(expr)).unwrap();
            writeln!(output, "{}  arms: [", spaces).unwrap();
            for arm in arms {
                writeln!(output, "{}    MatchArm {{ pattern: {}, body: [...] }}", 
                    spaces, expr_to_string(&arm.pattern)).unwrap();
            }
            writeln!(output, "{}  ]", spaces).unwrap();
            writeln!(output, "{}}}", spaces).unwrap();
        }
        Stmt::Return(expr) => {
            writeln!(output, "{}Return({})", spaces, 
                expr.as_ref().map_or("None".to_string(), |e| expr_to_string(e))).unwrap();
        }
        Stmt::Break => {
            writeln!(output, "{}Break", spaces).unwrap();
        }
        Stmt::Continue => {
            writeln!(output, "{}Continue", spaces).unwrap();
        }
        Stmt::Expression(expr) => {
            writeln!(output, "{}Expression({})", spaces, expr_to_string(expr)).unwrap();
        }
    }
}

fn expr_to_string(expr: &Expr) -> String {
    match expr {
        Expr::Literal(lit) => match lit {
            Literal::Number(n) => n.to_string(),
            Literal::String(s) => format!("\"{}\"", s),
            Literal::Boolean(b) => b.to_string(),
            Literal::Nil => "nil".to_string(),
        },
        Expr::Identifier(name) => name.clone(),
        Expr::Binary { left, op, right } => {
            format!("({} {:?} {})", expr_to_string(left), op, expr_to_string(right))
        }
        Expr::Unary { op, operand } => {
            format!("({:?} {})", op, expr_to_string(operand))
        }
        Expr::Call { callee, args } => {
            let args_str = args.iter().map(expr_to_string).collect::<Vec<_>>().join(", ");
            format!("{}({})", callee, args_str)
        }
        Expr::MemberAccess { object, member } => {
            format!("{}.{}", expr_to_string(object), member)
        }
        Expr::MemberCall { object, method, args } => {
            let args_str = args.iter().map(expr_to_string).collect::<Vec<_>>().join(", ");
            format!("{}.{}({})", expr_to_string(object), method, args_str)
        }
        Expr::IndexAccess { object, index } => {
            format!("{}[{}]", expr_to_string(object), expr_to_string(index))
        }
        Expr::ArrayLiteral(elements) => {
            let elements_str = elements.iter().map(expr_to_string).collect::<Vec<_>>().join(", ");
            format!("[{}]", elements_str)
        }
    }
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
            
            // Skip over any newlines or whitespace tokens if they exist
            // The main fix: make sure we don't skip over important tokens
            if self.is_at_end() {
                break;
            }
            
            match self.parse_item() {
                Ok(item) => items.push(item),
                Err(e) => {
                    // For debugging: you might want to print the error
                    eprintln!("Parse error: {:?}", e);
                    eprintln!("Current token: {:?}", self.peek());
                    return Err(e);
                }
            }
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

        // The main issue was here - we need to check the current token, not peek
        match &self.peek().token_type {
            TokenType::Def => {
                Ok(Item::Function(self.parse_function(visibility)?))
            },
            TokenType::Field => Ok(Item::Field(self.parse_field()?)),
            TokenType::Comp => Ok(Item::Component(self.parse_component()?)),
            TokenType::Enum => Ok(Item::Enum(self.parse_enum()?)),
            _ => {
                // If no visibility modifier was consumed, try parsing as statement
                if visibility.is_some() {
                    return Err(ParseError::UnexpectedToken {
                        line: self.peek().span.line,
                        column: self.peek().span.column,
                        expected: "def, field, comp, or enum after visibility modifier".to_string(),
                        found: format!("{:?}", self.peek().token_type),
                    });
                }
                Ok(Item::Statement(self.parse_statement()?))
            },
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
            
            while !self.check(&TokenType::Impl) && !self.is_at_end() && 
                !self.check(&TokenType::Field) && !self.check(&TokenType::Comp) && !self.check(&TokenType::Enum) {
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
            
            // Fixed: Now we look for 'def' tokens to parse functions within impl
            while !self.is_at_end() && 
                !self.check(&TokenType::Field) && !self.check(&TokenType::Comp) && !self.check(&TokenType::Enum) {
                
                // Check if we've reached the end of the impl block (next top-level item)
                if self.check(&TokenType::Pub) || self.check(&TokenType::Prv) {
                    // Look ahead to see if this is a top-level declaration
                    let saved_pos = self.current;
                    self.advance(); // consume pub/prv
                    if self.check(&TokenType::Field) || self.check(&TokenType::Comp) || self.check(&TokenType::Enum) {
                        // This is a top-level declaration, restore position and break
                        self.current = saved_pos;
                        break;
                    }
                    // Not a top-level declaration, restore position and continue
                    self.current = saved_pos;
                }
                
                // Parse function visibility
                let visibility = if self.match_token(&TokenType::Pub) {
                    Some(Visibility::Public)
                } else if self.match_token(&TokenType::Prv) {
                    Some(Visibility::Private)
                } else {
                    impl_visibility.clone()
                };
                
                // We expect a function definition here
                if self.check(&TokenType::Def) {
                    functions.push(self.parse_function(visibility)?);
                } else {
                    // If we don't see a def, we're probably at the end of the impl block
                    break;
                }
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
            // Could be assignment, member assignment, index assignment, or expression
            let expr = self.parse_expression()?;
            
            // Check if this is an assignment
            if self.match_token(&TokenType::Assign) || self.match_token(&TokenType::PlusAssign) {
                let is_plus_assign = self.previous().token_type == TokenType::PlusAssign;
                let value = self.parse_expression()?;
                self.consume(&TokenType::Semicolon, "Expected ';' after assignment")?;
                
                // Create appropriate assignment based on expression type
                match expr {
                    Expr::Identifier(name) => {
                        Ok(Stmt::Assignment {
                            target: name,
                            value,
                            is_plus_assign,
                        })
                    }
                    Expr::MemberAccess { object, member } => {
                        Ok(Stmt::MemberAssignment {
                            object: *object,
                            member,
                            value,
                            is_plus_assign,
                        })
                    }
                    Expr::IndexAccess { object, index } => {
                        Ok(Stmt::IndexAssignment {
                            object: *object,
                            index: *index,
                            value,
                            is_plus_assign,
                        })
                    }
                    _ => {
                        Err(ParseError::InvalidStatement)
                    }
                }
            } else {
                // Not an assignment, treat as expression statement
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
        while !self.is_at_end() && !self.check(&TokenType::Def) &&
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
            if self.check(&TokenType::Def) || 
               self.check(&TokenType::Field) || self.check(&TokenType::Comp) || 
               self.check(&TokenType::Enum) {
                break;
            }
            
            // Also break on EOF to prevent infinite loops
            if self.check(&TokenType::EOF) {
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
    let mut expr = match &self.peek().token_type {
        TokenType::Number(n) => {
            let value = *n;
            self.advance();
            Expr::Literal(Literal::Number(value))
        }
        TokenType::String(s) => {
            let value = s.clone();
            self.advance();
            Expr::Literal(Literal::String(value))
        }
        TokenType::Boolean(b) => {
            let value = *b;
            self.advance();
            Expr::Literal(Literal::Boolean(value))
        }
        TokenType::Nil => {
            self.advance();
            Expr::Literal(Literal::Nil)
        }
        TokenType::Identifier(_) => {
            let name = self.consume_identifier("Expected identifier")?;
            Expr::Identifier(name)
        }
        TokenType::LParen => {
            self.advance();
            let expr = self.parse_expression()?;
            self.consume(&TokenType::RParen, "Expected ')' after expression")?;
            expr
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
            Expr::ArrayLiteral(elements)
        }
        _ => return Err(ParseError::InvalidExpression),
    };

    // Handle chained member access, index access, and function calls
    loop {
        if self.match_token(&TokenType::Dot) {
            let member = self.consume_identifier("Expected member name after '.'")?;
            
            // Check if this is a member function call
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
                expr = Expr::MemberCall {
                    object: Box::new(expr),
                    method: member,
                    args,
                };
            } else {
                expr = Expr::MemberAccess {
                    object: Box::new(expr),
                    member,
                };
            }
        } else if self.match_token(&TokenType::LBracket) {
            let index = self.parse_expression()?;
            self.consume(&TokenType::RBracket, "Expected ']' after index")?;
            expr = Expr::IndexAccess {
                object: Box::new(expr),
                index: Box::new(index),
            };
        } else if self.match_token(&TokenType::LParen) {
            // Function call
            if let Expr::Identifier(name) = expr {
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
                expr = Expr::Call { callee: name, args };
            } else {
                return Err(ParseError::InvalidExpression);
            }
        } else {
            break;
        }
    }

    Ok(expr)
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
            let custom_name = name.clone();
            self.advance();
            Type::Custom(custom_name)
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