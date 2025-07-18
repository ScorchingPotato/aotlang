use pest::Parser;
use pest_derive::Parser;
use thiserror::Error;
use serde::{Serialize, Deserialize};

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct AotParser;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TokenType {
    // Keywords
    Def,
    Field,
    Comp,
    Enum,
    Var,
    Flex,
    Const,
    If,
    Else,
    While,
    For,
    Foreach,
    Match,
    Return,
    Break,
    Continue,
    In,
    Use,
    Req,
    Val,
    Impl,
    Pub,
    Prv,
    
    // Types
    Num,
    Ntr,
    Int,
    Flt,
    Str,
    Fst,
    Dst,
    Lst,
    Arr,
    Vec,
    Zip,
    Bit,
    Nil,
    
    // Operators
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    Assign,
    PlusAssign,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Not,
    And,
    Or,
    Arrow,
    
    // Punctuation
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Comma,
    Semicolon,
    Colon,
    Dot,
    
    // Literals
    Number(f64),
    String(String),
    Boolean(bool),
    Identifier(String),
    
    // Special
    EOF,
    Newline,
    Whitespace,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Token {
    pub token_type: TokenType,
    pub span: Span,
    pub raw: String,
}

#[derive(Debug, Error)]
pub enum LexError {
    #[error("Unexpected character at line {line}, column {column}: {ch}")]
    UnexpectedChar { line: usize, column: usize, ch: char },
    #[error("Unterminated string literal at line {line}, column {column}")]
    UnterminatedString { line: usize, column: usize },
    #[error("Invalid number format at line {line}, column {column}: {text}")]
    InvalidNumber { line: usize, column: usize, text: String },
    #[error("Parse error: {0}")]
    ParseError(#[from] pest::error::Error<Rule>),
}

pub struct Lexer {
    input: String,
    position: usize,
    line: usize,
    column: usize,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        Self {
            input,
            position: 0,
            line: 1,
            column: 1,
        }
    }
    
    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();
        
        while !self.is_at_end() {
            self.skip_whitespace();
            
            if self.is_at_end() {
                break;
            }
            
            let start_pos = self.position;
            let start_line = self.line;
            let start_column = self.column;
            
            let token_type = self.next_token()?;
            let end_pos = self.position;
            
            if !matches!(token_type, TokenType::Whitespace | TokenType::Newline) {
                tokens.push(Token {
                    token_type,
                    span: Span {
                        start: start_pos,
                        end: end_pos,
                        line: start_line,
                        column: start_column,
                    },
                    raw: self.input[start_pos..end_pos].to_string(),
                });
            }
        }
        
        tokens.push(Token {
            token_type: TokenType::EOF,
            span: Span {
                start: self.position,
                end: self.position,
                line: self.line,
                column: self.column,
            },
            raw: String::new(),
        });
        
        Ok(tokens)
    }
    
    fn next_token(&mut self) -> Result<TokenType, LexError> {
        let ch = self.advance();
        
        match ch {
            // Single-character tokens
            '(' => Ok(TokenType::LParen),
            ')' => Ok(TokenType::RParen),
            '[' => Ok(TokenType::LBracket),
            ']' => Ok(TokenType::RBracket),
            '{' => Ok(TokenType::LBrace),
            '}' => Ok(TokenType::RBrace),
            ',' => Ok(TokenType::Comma),
            ';' => Ok(TokenType::Semicolon),
            ':' => Ok(TokenType::Colon),
            '.' => Ok(TokenType::Dot),
            '%' => Ok(TokenType::Modulo),
            
            // Multi-character operators
            '+' => {
                if self.match_char('=') {
                    Ok(TokenType::PlusAssign)
                } else {
                    Ok(TokenType::Plus)
                }
            }
            '-' => Ok(TokenType::Minus),
            '*' => Ok(TokenType::Multiply),
            '/' => {
                if self.match_char('/') {
                    // Line comment
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                    Ok(TokenType::Whitespace)
                } else if self.match_char('*') {
                    // Block comment
                    while !self.is_at_end() {
                        if self.peek() == '*' && self.peek_next() == '/' {
                            self.advance(); // consume '*'
                            self.advance(); // consume '/'
                            break;
                        }
                        self.advance();
                    }
                    Ok(TokenType::Whitespace)
                } else {
                    Ok(TokenType::Divide)
                }
            }
            '=' => {
                if self.match_char('=') {
                    Ok(TokenType::Eq)
                } else if self.match_char('>') {
                    Ok(TokenType::Arrow)
                } else {
                    Ok(TokenType::Assign)
                }
            }
            '!' => {
                if self.match_char('=') {
                    Ok(TokenType::Ne)
                } else {
                    Ok(TokenType::Not)
                }
            }
            '<' => {
                if self.match_char('=') {
                    Ok(TokenType::Le)
                } else {
                    Ok(TokenType::Lt)
                }
            }
            '>' => {
                if self.match_char('=') {
                    Ok(TokenType::Ge)
                } else {
                    Ok(TokenType::Gt)
                }
            }
            '&' => {
                if self.match_char('&') {
                    Ok(TokenType::And)
                } else {
                    Err(LexError::UnexpectedChar {
                        line: self.line,
                        column: self.column - 1,
                        ch,
                    })
                }
            }
            '|' => {
                if self.match_char('|') {
                    Ok(TokenType::Or)
                } else {
                    Err(LexError::UnexpectedChar {
                        line: self.line,
                        column: self.column - 1,
                        ch,
                    })
                }
            }
            
            // String literals
            '"' => self.string_literal(),
            
            // Numbers
            '0'..='9' => self.number_literal(),
            
            // Identifiers and keywords
            'a'..='z' | 'A'..='Z' | '_' => self.identifier(),
            
            // Whitespace
            ' ' | '\r' | '\t' => Ok(TokenType::Whitespace),
            '\n' => {
                self.line += 1;
                self.column = 1;
                Ok(TokenType::Newline)
            }
            
            _ => Err(LexError::UnexpectedChar {
                line: self.line,
                column: self.column - 1,
                ch,
            }),
        }
    }
    
    fn string_literal(&mut self) -> Result<TokenType, LexError> {
        let mut value = String::new();
        
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
                self.column = 1;
            }
            
            if self.peek() == '\\' {
                self.advance(); // consume backslash
                match self.peek() {
                    'n' => value.push('\n'),
                    't' => value.push('\t'),
                    'r' => value.push('\r'),
                    '\\' => value.push('\\'),
                    '"' => value.push('"'),
                    _ => {
                        value.push('\\');
                        value.push(self.peek());
                    }
                }
            } else {
                value.push(self.peek());
            }
            self.advance();
        }
        
        if self.is_at_end() {
            return Err(LexError::UnterminatedString {
                line: self.line,
                column: self.column,
            });
        }
        
        // Consume closing quote
        self.advance();
        
        Ok(TokenType::String(value))
    }
    
    fn number_literal(&mut self) -> Result<TokenType, LexError> {
        let start_pos = self.position - 1;
        
        while self.peek().is_ascii_digit() {
            self.advance();
        }
        
        // Look for decimal point
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            self.advance(); // consume '.'
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }
        
        let text = &self.input[start_pos..self.position];
        match text.parse::<f64>() {
            Ok(value) => Ok(TokenType::Number(value)),
            Err(_) => Err(LexError::InvalidNumber {
                line: self.line,
                column: self.column - text.len(),
                text: text.to_string(),
            }),
        }
    }
    
    fn identifier(&mut self) -> Result<TokenType, LexError> {
        let start_pos = self.position - 1;
        
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.advance();
        }
        
        let text = &self.input[start_pos..self.position];
        
        let token_type = match text {
            // Keywords
            "def" => TokenType::Def,
            "field" => TokenType::Field,
            "comp" => TokenType::Comp,
            "enum" => TokenType::Enum,
            "var" => TokenType::Var,
            "flex" => TokenType::Flex,
            "const" => TokenType::Const,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "while" => TokenType::While,
            "for" => TokenType::For,
            "foreach" => TokenType::Foreach,
            "match" => TokenType::Match,
            "return" => TokenType::Return,
            "break" => TokenType::Break,
            "continue" => TokenType::Continue,
            "in" => TokenType::In,
            "use" => TokenType::Use,
            "req" => TokenType::Req,
            "val" => TokenType::Val,
            "impl" => TokenType::Impl,
            "pub" => TokenType::Pub,
            "prv" => TokenType::Prv,
            
            // Types
            "num" => TokenType::Num,
            "ntr" => TokenType::Ntr,
            "int" => TokenType::Int,
            "flt" => TokenType::Flt,
            "str" => TokenType::Str,
            "fst" => TokenType::Fst,
            "dst" => TokenType::Dst,
            "lst" => TokenType::Lst,
            "arr" => TokenType::Arr,
            "vec" => TokenType::Vec,
            "zip" => TokenType::Zip,
            "bit" => TokenType::Bit,
            "nil" => TokenType::Nil,
            
            // Boolean literals
            "true" => TokenType::Boolean(true),
            "false" => TokenType::Boolean(false),
            
            // Identifier
            _ => TokenType::Identifier(text.to_string()),
        };
        
        Ok(token_type)
    }
    
    fn skip_whitespace(&mut self) {
        while matches!(self.peek(), ' ' | '\r' | '\t') {
            self.advance();
        }
    }
    
    fn advance(&mut self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        
        let ch = self.input.chars().nth(self.position).unwrap_or('\0');
        self.position += 1;
        self.column += 1;
        ch
    }
    
    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.peek() != expected {
            false
        } else {
            self.advance();
            true
        }
    }
    
    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.input.chars().nth(self.position).unwrap_or('\0')
        }
    }
    
    fn peek_next(&self) -> char {
        if self.position + 1 >= self.input.len() {
            '\0'
        } else {
            self.input.chars().nth(self.position + 1).unwrap_or('\0')
        }
    }
    
    fn is_at_end(&self) -> bool {
        self.position >= self.input.len()
    }
}