use gamma_macros::{SmartExpression, SmartStatement};
use std::any::Any;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;
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
        TokenType::LeftParen => 12,
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

#[derive(SmartExpression)]
pub struct IdentifierExpression {
    pub identifier: String,
}

impl IdentifierExpression {
    pub fn new(identifier: String) -> Self {
        return Self { identifier }
    }
}

impl Debug for IdentifierExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.identifier)
    }
}

#[derive(SmartExpression)]
pub struct StringExpression {
    pub value: String,
}

impl StringExpression {
    pub fn new(value: String) -> Self {
        return Self { value }
    }
}

impl Debug for StringExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{}\"", self.value)
    }
}

#[derive(SmartExpression)]
pub struct TableExpression {
    pub values: Vec<Box<dyn Expression>>
}

impl TableExpression {
    pub fn new(values: Vec<Box<dyn Expression>>) -> Self {
        return Self { values }
    }
}

impl Debug for TableExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {:?} {}",
            "{",
            self.values.iter().map(|expr| { expr.to_string() }).collect::<Vec<_>>(),
            "}"
        )
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

#[derive(SmartExpression)]
pub struct CallExpression {
    pub function: Box<dyn Expression>,
    pub arguments: Vec<Box<dyn Expression>>,
}

impl CallExpression {
    pub fn new(function: Box<dyn Expression>, arguments: Vec<Box<dyn Expression>>) -> Self {
        return Self {
            function,
            arguments,
        }
    }
}

impl Debug for CallExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,
               "{}({:?})",
               self.function.to_string(),
               self.arguments.iter().map(|stmt| { stmt.to_string() }).collect::<Vec<_>>(),
        )
    }
}

pub trait Statement {
    fn as_any(&self) -> &dyn Any;
    fn to_string(&self) -> String;
}

pub enum AssignmentType {
    Local,
    Reassignment
}

#[derive(SmartStatement)]
pub struct AssignmentStatement {
    pub assignment_type: AssignmentType,
    pub variable: Token,
    pub value: Box<dyn Expression>,
}

impl AssignmentStatement {
    pub fn local(variable: Token, value: Box<dyn Expression>) -> Self {
        return Self {
            assignment_type: AssignmentType::Local,
            variable,
            value,
        };
    }

    pub fn reassignment(variable: Token, value: Box<dyn Expression>) -> Self {
        return Self {
            assignment_type: AssignmentType::Reassignment,
            variable,
            value,
        };
    }
}

impl Debug for AssignmentStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.assignment_type {
            AssignmentType::Local => write!(f, "local {} = {}", self.variable.literal, self.value.to_string()),
            AssignmentType::Reassignment => write!(f, "{} = {}", self.variable.literal, self.value.to_string()),
        }
    }
}

pub enum LoopType {
    While,
    Repeat,
}

#[derive(SmartStatement)]
pub struct LoopStatement {
    pub loop_type: LoopType,
    pub condition: Box<dyn Expression>,
    pub block: Vec<Box<dyn Statement>>,
}

impl LoopStatement {
    pub fn while_loop(
        condition: Box<dyn Expression>,
        block: Vec<Box<dyn Statement>>) -> Self {
        return Self {
            condition,
            block,
            loop_type: LoopType::While,
        };
    }

    pub fn repeat_loop(
        condition: Box<dyn Expression>,
        block: Vec<Box<dyn Statement>>) -> Self {
        return Self {
            condition,
            block,
            loop_type: LoopType::Repeat,
        };
    }
}

impl Debug for LoopStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.loop_type {
            LoopType::While => {
                write!(
                    f,
                    "while {} do {:?} end",
                    self.condition.to_string(),
                    self.block.iter().map(|stmt| { stmt.to_string() }).collect::<Vec<_>>()
                )
            }
            LoopType::Repeat => {
                write!(
                    f,
                    "repeat {:?} until {}",
                    self.block.iter().map(|stmt| { stmt.to_string() }).collect::<Vec<_>>(),
                    self.condition.to_string(),
                )
            }
        }
    }
}

// Lua docs:
// The for loop has some subtleties that you should learn in order to make good use of it.
// First, all three expressions are evaluated once, before the loop starts.
// Second, the control variable is a local variable automatically declared by
// the for statement and is visible only inside the loop.
// A typical mistake is to assume that the variable still exists after the loop ends
#[derive(SmartStatement)]
pub struct ForStatement {
    pub initial_variable: Box<dyn Statement>,
    pub end_value: Box<dyn Expression>,
    pub increment_value: Box<dyn Expression>,
    pub block: Vec<Box<dyn Statement>>,
}

impl Debug for ForStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "for {}, {}, {} do {:?} end",
            self.initial_variable.to_string(),
            self.end_value.to_string(),
            self.increment_value.to_string(),
            self.block.iter().map(|stmt| { stmt.to_string() }).collect::<Vec<_>>(),
        )
    }
}

impl ForStatement {
    pub fn new(
        initial_variable:
        Box<dyn Statement>,
        end_value: Box<dyn Expression>,
        increment_value: Box<dyn Expression>,
        block: Vec<Box<dyn Statement>>
    ) -> Self {
        return Self {
            initial_variable,
            end_value,
            increment_value,
            block,
        };
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
    pub elseif_blocks: Vec<Box<dyn Statement>>,
    pub else_block: Vec<Box<dyn Statement>>,
}

impl Debug for IfStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
           "if {} then {:?} {:?} else {:?} end",
           self.condition.to_string(),
           self.block.iter().map(|stmt| { stmt.to_string() }).collect::<Vec<_>>(),
           self.elseif_blocks.iter().map(|stmt| { stmt.to_string() }).collect::<Vec<_>>(),
           self.else_block.iter().map(|stmt| { stmt.to_string() }).collect::<Vec<_>>(),
        )
    }
}

impl IfStatement {
    pub fn new(
        condition: Box<dyn Expression>,
        block: Vec<Box<dyn Statement>>,
        elseif_blocks: Vec<Box<dyn Statement>>,
        else_block: Vec<Box<dyn Statement>>,
    ) -> Self {
        return Self {
            condition,
            block,
            elseif_blocks,
            else_block,
        };
    }
}

#[derive(SmartStatement)]
pub struct ElseIfStatement {
    pub condition: Box<dyn Expression>,
    pub block: Vec<Box<dyn Statement>>,
}

impl Debug for ElseIfStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
           "elseif {} then {:?} end",
           self.condition.to_string(),
           self.block.iter().map(|stmt| { stmt.to_string() }).collect::<Vec<_>>()
        )
    }
}

impl ElseIfStatement {
    pub fn new(condition: Box<dyn Expression>, block: Vec<Box<dyn Statement>>) -> Self {
        return Self {
            condition,
            block,
        };
    }
}

#[derive(SmartStatement)]
pub struct FunctionStatement {
    pub name: String,
    pub function: Rc<FunctionExpression>,
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
            function: Rc::new(function),
        };
    }
}

#[derive(SmartStatement)]
pub struct FunctionCallStatement {
    pub call: Box<dyn Expression>,
}

impl Debug for FunctionCallStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.call.to_string())
    }
}

impl FunctionCallStatement {
    pub fn new(call: Box<dyn Expression>) -> Self {
        return Self {
            call,
        };
    }
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}