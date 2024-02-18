use gamma_macros::{SmartExpression, SmartStatement};
use std::any::Any;
use std::fmt::{Debug, Formatter};
use crate::parsing::lexer::{Token, TokenType};

pub const INITIAL_PRECEDENCE: i8 = -1;
pub const PREFIX_PRECEDENCE: i8 = 10;

pub fn get_operator_precedence(token_type: TokenType) -> i8 {
    match token_type {
        TokenType::Or => 0,
        TokenType::And => 1,
        TokenType::Lower | TokenType::Greater | TokenType::LowerEqual |
        TokenType::GreaterEqual | TokenType::DoubleEquals | TokenType::TildeEqual
            => 2,
        TokenType::Bar => 3,
        TokenType::Tilde => 4,
        TokenType::Ampersand => 5,
        TokenType::ShiftLeft | TokenType::ShiftRight => 6,
        TokenType::DoubleDot => 7,
        TokenType::Plus | TokenType::Minus => 8,
        TokenType::Star | TokenType::Slash | TokenType::DoubleSlash | TokenType::Percent => 9,
        // 10 is reserved for prefix
        TokenType::Caret => 11,
        _ => -1,
    }
}

pub trait Expression {
    fn as_any(&self) -> &dyn Any;
    fn to_string(&self) -> String;
}

#[derive(SmartExpression)]
pub struct IntExpression {
    pub value: i64,
}

impl IntExpression {
    pub fn new(value: i64) -> Self {
        return Self { value }
    }
}

impl Debug for IntExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, SmartExpression)]
pub struct IdentifierExpression {
    pub identifier: String,
}

impl IdentifierExpression {
    pub fn new(identifier: String) -> Self {
        return Self { identifier }
    }
}

#[derive(SmartExpression)]
pub struct PrefixExpression {
    pub operator: TokenType,
    pub value: Box<dyn Expression>,
}

impl PrefixExpression {
    pub fn new(operator: TokenType, value: Box<dyn Expression>,) -> Self {
        return Self {
            operator,
            value,
        }
    }
}

impl Debug for PrefixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,
               "{:?}{}",
               self.operator,
               self.value.to_string()
        )
    }
}

#[derive(SmartExpression)]
pub struct InfixExpression {
    pub left_value: Box<dyn Expression>,
    pub operator: TokenType,
    pub right_value: Box<dyn Expression>,
}

impl InfixExpression {
    pub fn new(left_value: Box<dyn Expression>, operator: TokenType, right_value: Box<dyn Expression>,) -> Self {
        return Self {
            left_value,
            operator,
            right_value,
        }
    }
}

impl Debug for InfixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,
               "({} {:?} {})",
               self.left_value.to_string(),
               self.operator,
               self.right_value.to_string()
        )
    }
}

#[derive(SmartExpression)]
pub struct FunctionExpression {
    pub parameters: Vec<String>,
    pub block: Vec<Box<dyn Statement>>,
}

impl FunctionExpression {
    pub fn new(parameters: Vec<String>, block: Vec<Box<dyn Statement>>) -> Self {
        return Self {
            parameters,
            block,
        }
    }
}

impl Debug for FunctionExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,
               "({:?}) {:?} end",
               self.parameters,
               self.block.iter().map(|stmt| { stmt.to_string() }).collect::<Vec<_>>(),
        )
    }
}

pub trait Statement {
    fn as_any(&self) -> &dyn Any;
    fn to_string(&self) -> String;
}

#[derive(SmartStatement)]
pub struct AssignmentStatement {
    pub variable: Token,
    pub value: Box<dyn Expression>,
}

impl AssignmentStatement {
    pub fn new(variable: Token, value: Box<dyn Expression>) -> Self {
        return Self {
            variable,
            value,
        };
    }
}

impl Debug for AssignmentStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "local {:?} = {}", self.variable, self.value.to_string())
    }
}

#[derive(SmartStatement)]
pub struct ReturnStatement {
    pub value: Box<dyn Expression>,
}

impl Debug for ReturnStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "return {}", self.value.to_string())
    }
}

impl ReturnStatement {
    pub fn new(value: Box<dyn Expression>) -> Self {
        return Self {
            value,
        };
    }
}

#[derive(SmartStatement)]
pub struct IfStatement {
    pub condition: Box<dyn Expression>,
    pub block: Vec<Box<dyn Statement>>,
}

impl Debug for IfStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
           "if {} then {:?} end",
           self.condition.to_string(),
           self.block.iter().map(|stmt| { stmt.to_string() }).collect::<Vec<_>>()
        )
    }
}

impl IfStatement {
    pub fn new(condition: Box<dyn Expression>, block: Vec<Box<dyn Statement>>,) -> Self {
        return Self {
            condition,
            block,
        };
    }
}

#[derive(SmartStatement)]
pub struct FunctionStatement {
    pub name: String,
    pub function: FunctionExpression,
}

impl Debug for FunctionStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
           "function {} {:?}",
           self.name,
           self.function
        )
    }
}

impl FunctionStatement {
    pub fn new(name: String, function: FunctionExpression) -> Self {
        return Self {
            name,
            function,
        };
    }
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}