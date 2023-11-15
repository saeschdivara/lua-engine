use std::error::Error;

use crate::ast::Token;
use crate::lexer::lex_lua;
use crate::parser::Parser;

pub fn parse_file(path: &str) -> Result<(), Box<dyn Error>> {
    let file_content = std::fs::read_to_string(path)?;
    let (_, tokens) = lex_lua(file_content.leak())?;

    let parser = Parser::new();

    let code = parser.parse_lua_ast(
        tokens
            .into_iter()
            .filter(|tok| match *tok {
                Token::WhiteSpace { .. } => false,
                _ => true
            })
            .collect()
    )?;

    return Ok(());
}