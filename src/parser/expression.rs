use crate::ast::{Number, Token};
use crate::parser::Statement;

#[derive(Debug, PartialEq, Clone)]
pub enum Table {
    Array(Vec<Expression>)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Variable { name: String, token: Token },
    String { value: String, token: Token },
    Number { value: Number, token: Token },
    Table { value: Table, token: Token },
    Function { name: Option<String>, parameters: Vec<Expression>, body: Vec<Statement>, token: Token },
}

pub enum OperatorPrecedence {
    Lowest = 0,
    Or = 1,
    And = 2,
    Compare = 3,
    // <     >     <=    >=    ~=    ==
    BitOr = 4,
    // |
    BitNot = 5,
    // ~
    BitAnd = 6,
    // ~
    BitShift = 7,
    // <<    >>
    Concat = 8,
    // ..
    Sum = 9,
    // +     -
    Multiplication = 10,
    //  *     /     //    %
    Unary = 11,
    //  unary operators (not   #     -     ~)
    Exponentiation = 12, // ^
}
