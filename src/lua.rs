use std::error::Error;

use crate::lexer::lex_lua;

pub fn parse_file(path: &str) -> Result<(), Box<dyn Error>> {
    let file_content = std::fs::read_to_string(path)?;
    parse_lua(file_content.as_str());

    return Ok(());
}


fn parse_lua(content: &str) {
    lex_lua(content).unwrap();
}

