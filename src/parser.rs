use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use crate::ast::{Keyword, Token};

#[derive(Debug, Clone)]
pub enum Expression {
    Variable { name: String }
}

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

pub fn parse_lua_ast(mut token_stream: Vec<Token>) -> Result<LuaCode, Box<dyn Error>> {
    let mut statements = vec![];

    while let Ok((tokens, stmt)) = parse_next_statement(token_stream) {
        println!("Stmt: {:?}", stmt.clone());
        statements.push(stmt);
        token_stream = tokens;
    }

    Ok(LuaCode { statements })
}

fn parse_next_statement(mut tokens: Vec<Token>) -> Result<(Vec<Token>, Statement), Box<dyn Error>> {
    let current_token = tokens.remove(0);
    match current_token {
        Token::Keyword { literal } => {
            match literal {
                Keyword::Local => parse_local_statement(tokens),
                _ => Err(Box::new(ParsingError::new("Invalid keyword for statement beginning")))
            }
        }
        _ => Err(Box::new(ParsingError::new("Statement has to begin with keyword")))
    }
}

fn parse_local_statement(mut tokens: Vec<Token>) -> Result<(Vec<Token>, Statement), Box<dyn Error>> {
    let mut stmt = Statement { expressions: vec![] };
    let current_token = tokens.remove(0);
    match current_token {
        Token::Identifier { literal } => {
            stmt.expressions.push(Expression::Variable { name: literal })
        },
        _ => return Err(Box::new(ParsingError::new("Local expects identifier")))
    };

    let current_token = tokens.remove(0);
    match current_token {
        Token::Equal { .. } => {}
        _ => return Err(Box::new(ParsingError::new("Expect = after identifier")))
    };

    let (mut tokens, value) = parse_expression(tokens)?;
    stmt.expressions.push(value);

    let current_token = tokens.remove(0);
    match current_token {
        Token::SemiColon { .. } => {}
        _ => return Err(Box::new(ParsingError::new("Statement has to end with ;")))
    };

    Ok((tokens, stmt))
}

fn parse_expression(mut tokens: Vec<Token>) -> Result<(Vec<Token>, Expression), Box<dyn Error>> {
    Err(Box::new(ParsingError::new("Could not parse expression")))
}