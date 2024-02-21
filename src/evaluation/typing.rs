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
}