use std::rc::Rc;
use crate::parsing::ast::FunctionExpression;

pub enum NumberType {
    Int(i64),
    Float(f64)
}

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