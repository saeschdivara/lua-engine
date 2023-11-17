use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

use crate::ast::{Keyword, Token};
use crate::parser::expression::{Expression, ExpressionParsingResult, OperatorPrecedence};

mod expression;

#[derive(Debug, Clone)]
pub struct Statement {
    pub expressions: Vec<Expression>,
}

pub struct LuaCode {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct ParsingError {
    message: String,
}

impl ParsingError {
    fn new(message: &str) -> Self {
        return Self {
            message: message.to_string(),
        };
    }
}

impl Error for ParsingError {}

impl Display for ParsingError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Could not parse it: {}", self.message)
    }
}

pub struct Parser {
}

impl Parser {
    pub fn new() -> Self {
        Self {}
    }

    pub fn parse_lua_ast(&self, mut token_stream: Vec<Token>) -> Result<LuaCode, Box<dyn Error>> {
        let mut statements = vec![];

        loop {
            match self.parse_next_statement(token_stream) {
                Ok((tokens, stmt)) => {
                    println!("Stmt: {:?}", stmt.clone());
                    statements.push(stmt);
                    token_stream = tokens;
                }
                Err(err) => {
                    println!("{}", err);
                    break
                }
            }
        }

        Ok(LuaCode { statements })
    }

    fn parse_next_statement(&self, mut tokens: Vec<Token>) -> Result<(Vec<Token>, Statement), Box<dyn Error>> {
        let mut current_token = tokens.remove(0);
        let nl = Token::NewLine {};

        while current_token == nl {
            current_token = tokens.remove(0);
        }

        match current_token {
            Token::Keyword { literal } => {
                match literal {
                    Keyword::Local => self.parse_local_statement(tokens),
                    _ => Err(Box::new(ParsingError::new("Invalid keyword for statement beginning")))
                }
            }
            _ => Err(Box::new(ParsingError::new("Statement has to begin with keyword")))
        }
    }

    fn parse_local_statement(&self, mut tokens: Vec<Token>) -> Result<(Vec<Token>, Statement), Box<dyn Error>> {
        let mut stmt = Statement { expressions: vec![] };
        let current_token = tokens.remove(0);
        match current_token.clone() {
            Token::Identifier { literal } => {
                stmt.expressions.push(Expression::Variable { name: literal, token: current_token })
            }
            _ => return Err(Box::new(ParsingError::new("Local expects identifier")))
        };

        let current_token = tokens.remove(0);
        match current_token {
            Token::Equal { .. } => {}
            _ => return Err(Box::new(ParsingError::new("Expect = after identifier")))
        };

        let (mut tokens, value) = self.parse_expression(tokens, OperatorPrecedence::Lowest)?;
        stmt.expressions.push(value);

        let current_token = tokens.remove(0);
        match current_token {
            Token::SemiColon { .. } => {}
            _ => return Err(Box::new(ParsingError::new("Statement has to end with ;")))
        };

        Ok((tokens, stmt))
    }

    fn get_prefix_parser(&self, token: &Token) -> Option<fn(&Parser, Token, Vec<Token>) -> ExpressionParsingResult> {
        match token {
            Token::String { .. } => Some(Parser::parse_string),
            Token::Number { .. } => Some(Parser::parse_number),
            Token::LeftCurlyBracket { .. } => Some(Parser::parse_table),
            _ => None
        }
    }
}