use gamma_macros::{SmartExpression, SmartStatement};
use std::any::Any;
use std::fmt::{Debug, Formatter};
use crate::parsing::lexer::Token;

pub trait Expression {
    fn as_any(&self) -> &dyn Any;
    fn to_string(&self) -> String;
}

#[derive(Debug, SmartExpression)]
pub struct IntExpression {
    pub value: i64,
}

impl IntExpression {
    pub fn new(value: i64) -> Self {
        return Self { value }
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
        write!(f, "variable: {:?}, value: {}", self.variable, self.value.to_string())
    }
}

#[derive(SmartStatement)]
pub struct ReturnStatement {
    pub value: Box<dyn Expression>,
}

impl Debug for ReturnStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "value: {}", self.value.to_string())
    }
}

impl ReturnStatement {
    pub fn new(value: Box<dyn Expression>) -> Self {
        return Self {
            value,
        };
    }
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}