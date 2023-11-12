use nom::{InputTakeAtPosition, IResult};
use nom::branch::alt;
use nom::bytes::complete::{is_a, is_not, tag};
use nom::character::complete::*;
use nom::error::ErrorKind;
use nom::sequence::Tuple;

use crate::ast::{Keyword, Number, Token};

pub struct LuaCode {
    pub code: String,
}

pub fn lex_lua(content: &str) -> IResult<&str, LuaCode> {
    let mut rest_code = content;
    while let Ok((unparsed_code, matched_content)) = alt((
        parse_space,
        parse_line_breaks,
        parse_comment,
        parse_number,
        parse_identifier,
        parse_empty_string_token,
        parse_string_token,
        parse_operators,
        parse_other_tokens,
    ))(rest_code) {
        println!("Token: {:?}", matched_content);
        rest_code = unparsed_code;
    }


    Ok((rest_code, LuaCode { code: "".to_string() }))
}

fn parse_space(input: &str) -> IResult<&str, Token> {
    let (unparsed_code, matched_content) = space1(input)?;
    Ok((unparsed_code, Token::WhiteSpace { count: matched_content.len() as u8 }))
}

fn parse_line_breaks(input: &str) -> IResult<&str, Token> {
    let (unparsed_code, _) = newline(input)?;
    Ok((unparsed_code, Token::NewLine {}))
}

fn parse_empty_string_token(input: &str) -> IResult<&str, Token> {
    let (unparsed_code, (_, _)) = (
        tag("\""),
        tag("\""),
    ).parse(input)?;

    Ok((unparsed_code, Token::String { quote: '"', literal: "".to_string() }))
}

fn parse_string_token(input: &str) -> IResult<&str, Token> {
    let (unparsed_code, (_, matched_content, _)) = (
        tag("\""),
        is_not("\""),
        tag("\""),
    ).parse(input)?;

    Ok((unparsed_code, Token::String { quote: '"', literal: matched_content.to_string() }))
}

fn parse_comment(input: &str) -> IResult<&str, Token> {
    let (unparsed_code, (_, matched_content, _)) = (
        tag("--"),
        is_not("\r\n"),
        is_a("\r\n"),
    ).parse(input)?;

    Ok((unparsed_code, Token::Comment { literal: matched_content.to_string() }))
}

fn parse_number(input: &str) -> IResult<&str, Token> {
    let (unparsed_code, matched_content) = digit1(input)?;

    Ok((unparsed_code, Token::Number {
        literal: matched_content.to_string().clone(),
        value: Number::Int(matched_content.parse().unwrap())
    }))
}

fn parse_operators(input: &str) -> IResult<&str, Token> {
    let (unparsed_code, matched_content) = alt((
        tag("~="),
        tag("=="),
        tag("<="),
        tag(">="),
        tag("="),
        tag("~"),
        tag("<"),
        tag(">"),
        tag("-"),
        tag("+"),
    ))(input)?;

    match matched_content {
        "==" => Ok((unparsed_code, Token::DoubleEqual {})),
        "~=" => Ok((unparsed_code, Token::TildeEqual {})),
        "<=" => Ok((unparsed_code, Token::LowerEqual {})),
        ">=" => Ok((unparsed_code, Token::GreaterEqual {})),
        "=" => Ok((unparsed_code, Token::Equal {})),
        "~" => Ok((unparsed_code, Token::Tilde {})),
        "<" => Ok((unparsed_code, Token::Lower {})),
        ">" => Ok((unparsed_code, Token::Greater {})),
        "-" => Ok((unparsed_code, Token::Minus {})),
        "+" => Ok((unparsed_code, Token::Plus {})),
        _ => Ok((unparsed_code, Token::Invalid))
    }
}

fn parse_other_tokens(input: &str) -> IResult<&str, Token> {
    let (unparsed_code, matched_content) = alt((
        tag(";"),
        tag(":"),
        tag("..."),
        tag(".."),
        tag("."),
        tag(","),
        tag("{"),
        tag("}"),
        tag("["),
        tag("]"),
        tag("("),
        tag(")"),
    ))(input)?;

    match matched_content {
        ";" => Ok((unparsed_code, Token::SemiColon {})),
        ":" => Ok((unparsed_code, Token::Colon {})),
        "..." => Ok((unparsed_code, Token::TripleDot {})),
        ".." => Ok((unparsed_code, Token::DoubleDot {})),
        "." => Ok((unparsed_code, Token::Dot {})),
        "," => Ok((unparsed_code, Token::Comma {})),
        "{" => Ok((unparsed_code, Token::LeftCurlyBracket {})),
        "}" => Ok((unparsed_code, Token::RightCurlyBracket {})),
        "[" => Ok((unparsed_code, Token::LeftBracket {})),
        "]" => Ok((unparsed_code, Token::RightBracket {})),
        "(" => Ok((unparsed_code, Token::LeftParen {})),
        ")" => Ok((unparsed_code, Token::RightParen {})),
        _ => Ok((unparsed_code, Token::Invalid))
    }
}

fn parse_identifier(input: &str) -> IResult<&str, Token> {
    let (unparsed_code, matched_content) = input.split_at_position1_complete(
        |item| !item.is_alphanumeric() && item != '_',
        ErrorKind::AlphaNumeric,
    )?;

    Ok((
        unparsed_code,
        match matched_content {
            "local" => Token::Keyword { literal: Keyword::Local },
            "then" => Token::Keyword { literal: Keyword::Then },
            "end" => Token::Keyword { literal: Keyword::End },
            "for" => Token::Keyword { literal: Keyword::For },
            "do" => Token::Keyword { literal: Keyword::Do },
            "return" => Token::Keyword { literal: Keyword::Return },
            "function" => Token::Keyword { literal: Keyword::Function },
            "if" => Token::Keyword { literal: Keyword::If },
            _ => Token::Identifier { literal: matched_content.to_string() }
        },
    ))
}