use std::rc::Rc;
use crate::parsing::ast::FunctionExpression;

#[derive(Clone)]
pub enum NumberType {
    Int(i64),
    Float(f64)
}

#[derive(Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(NumberType),
    String(String),
    Function(Rc<FunctionExpression>),
    UserData,
    Thread,
    Table,
}