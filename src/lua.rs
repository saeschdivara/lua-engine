use std::error::Error;
use nom::branch::alt;
use nom::character::complete::{alphanumeric1, space1};
use nom::IResult;
use crate::ast::{Keyword, Token};

pub fn parse_file(path: &str) -> Result<(), Box<dyn Error>> {
    let file_content = std::fs::read_to_string(path)?;;
    let (unparsed_code, code) = parse_lua(file_content.leak())?;

    return Ok(());
}

struct LuaCode {
    pub code: String,
}

fn parse_lua(content: &str) -> IResult<&str, LuaCode> {
    let mut rest_code = content;
    while let Ok((unparsed_code, matched_content)) = alt((
        parse_space,
        parse_identifier
    ))(rest_code) {
        println!("Token: {:?}", matched_content);
        rest_code = unparsed_code;
    }


    return Ok((rest_code, LuaCode { code: "".to_string() }));
}

fn parse_space(input: &str) -> IResult<&str, Token> {
    let (unparsed_code, matched_content) = space1(input)?;
    return Ok((unparsed_code, Token::WhiteSpace {count: matched_content.len() as u8 }));
}

fn parse_identifier(input: &str) -> IResult<&str, Token> {
    let (unparsed_code, matched_content) = alphanumeric1(input)?;

    return Ok((
        unparsed_code,
        match matched_content {
            "local" => { Token::Keyword { literal: Keyword::Local } }
            _ => {
                Token::Identifier { literal: matched_content.to_string() }
            }
        },
    ));
}