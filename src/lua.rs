use std::error::Error;

use crate::ast::{Token, TokenStream};
use crate::lexer::lex_lua;
use crate::parser::Parser;

pub fn parse_file(path: &str) -> Result<(), Box<dyn Error>> {
    let file_content = std::fs::read_to_string(path)?;
    let (_, mut token_stream) = lex_lua(file_content.leak())?;
    let tokens = token_stream.tokens
        .into_iter()
        .filter(|tok| match *tok {
            Token::WhiteSpace { .. } => false,
            _ => true
        })
        .collect();

    let parser = Parser::new();

    let code = parser.parse_lua_ast(TokenStream { tokens })?;

    return Ok(());
}