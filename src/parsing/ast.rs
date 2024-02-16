use gamma_macros::{AnyExpression, AnyStatement};
use std::any::Any;
use crate::parsing::lexer::Token;

pub trait Expression {
    fn as_any(&self) -> &dyn Any;
}

#[derive(Debug, AnyExpression)]
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
}

#[derive(AnyStatement)]
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

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}