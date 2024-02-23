use std::collections::HashMap;
use std::rc::Rc;
use crate::evaluation::runtime::Runtime;

use crate::parsing::ast::FunctionExpression;

pub struct EvalError {
    pub message: String,
}

impl EvalError {
    pub fn new(message: String) -> Self {
        Self {
            message,
        }
    }
}

pub type EvalResult = Result<Value, EvalError>;
pub type EvalListResult = Result<Vec<Value>, EvalError>;
pub type EvalStatementResult = Result<(), EvalError>;
pub type NativeFunc = fn(args: &Vec<Value>, callstack: &mut Runtime) -> EvalResult;
pub type NativeMutableFunc = fn(args: &mut Vec<Value>, callstack: &mut Runtime) -> EvalResult;

#[derive(Clone, Debug)]
pub enum NumberType {
    Int(i64),
    Float(f64)
}

#[derive(Clone, Debug)]
pub enum FunctionType {
    Require,
    Native(NativeFunc),
    NativeMutable(NativeMutableFunc),
    Expression(Rc<FunctionExpression>),
}

#[derive(Clone, Debug)]
pub struct TableData {
    pub values: Vec<Value>,
    pub properties: HashMap<String, Value>,
    pub meta_table: Option<Value>,
}

#[derive(Clone, Debug)]
pub enum ObjectValue {
    Table(TableData),
}

impl ObjectValue {
    pub fn table(val: TableData) -> Self {
        return Self::Table(val);
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(NumberType),
    String(String),
    Function(FunctionType),
    UserData,
    Thread,
    Table(usize),
}

impl Value {
    pub fn is_nil(&self) -> bool {
        match self {
            Value::Nil => true,
            _ => false
        }
    }

    pub fn is_boolean(&self) -> bool {
        match self {
            Value::Boolean(_) => true,
            _ => false
        }
    }

    pub fn is_number(&self) -> bool {
        match self {
            Value::Number(_) => true,
            _ => false
        }
    }

    pub fn is_equals(&self, val: &Value) -> bool {
        if self.is_nil() && val.is_nil() { return true }
        else if self.is_boolean() && val.is_boolean() {
            if let Value::Boolean(l) = self && let Value::Boolean(r) = val{
                return l == r
            }
        }
        else if self.is_number() && val.is_number() {
            if let Value::Number(NumberType::Int(l)) = self && let Value::Number(NumberType::Int(r)) = val{
                return l == r
            }
        }

        return false
    }

    pub fn plus(&self, val: &Value) -> Value {
        if self.is_nil() && val.is_nil() { Value::Nil }
        else if self.is_boolean() || val.is_boolean() {
            Value::Nil
        }
        else if self.is_number() && val.is_number() {
            if let Value::Number(NumberType::Int(l)) = self && let Value::Number(NumberType::Int(r)) = val{
                Value::Number(NumberType::Int(l + r))
            } else {
                Value::Nil
            }
        } else {
            Value::Nil
        }
    }

    pub fn multiply(&self, val: &Value) -> Value {
        if self.is_nil() && val.is_nil() { Value::Nil }
        else if self.is_boolean() || val.is_boolean() {
            Value::Nil
        }
        else if self.is_number() && val.is_number() {
            if let Value::Number(NumberType::Int(l)) = self && let Value::Number(NumberType::Int(r)) = val{
                Value::Number(NumberType::Int(l * r))
            } else {
                Value::Nil
            }
        } else {
            Value::Nil
        }
    }
}