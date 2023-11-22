use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::IResult;

use crate::ast::{Keyword, Token, TokenStream};
use crate::parser::expression::Expression;

mod expression;
mod traits;

#[derive(Debug, PartialEq, Clone)]
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

    pub fn parse_lua_ast(&self, mut token_stream: TokenStream) -> Result<LuaCode, Box<dyn Error>> {
        let ((remaining_tokens, statements)) = self.parse_ast(token_stream)?;
        println!("Tokens: {:?}", remaining_tokens);

        Ok(LuaCode { statements })
    }

    fn parse_ast(&self, token_stream: TokenStream) -> IResult<TokenStream, Vec<Statement>> {
        let (remaining_tokens, stmt) = alt((
            Parser::parse_local,
            Parser::parse_function,
        ))(token_stream)?;

        println!("Statement: {:?}", stmt);

        Ok((remaining_tokens, vec![]))
    }

    fn parse_local(token_stream: TokenStream) -> IResult<TokenStream, Statement> {
        let (_, remaining_tokens) = tag(Token::Keyword { literal: Keyword::Local })(token_stream)?;
        Ok((remaining_tokens, Statement { expressions: vec![]}))
    }

    fn parse_function(token_stream: TokenStream) -> IResult<TokenStream, Statement> {
        // let (remaining_tokens, _) = tag(Token::Keyword { literal: Keyword::Local })(token_stream)?;
        Ok((token_stream, Statement { expressions: vec![]}))
    }

}