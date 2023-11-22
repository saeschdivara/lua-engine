use crate::parser::expression::Expression;

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Invalid,
    LocalStatement { identifier: Expression, value: Expression },
}