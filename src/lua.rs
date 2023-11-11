use std::error::Error;
use nom::IResult;

pub fn parse_file(path: &str) -> Result<(), Box<dyn Error>> {
    let file_content = std::fs::read_to_string(path)?;
    let (unparsed_code, code) = parse_lua(file_content)?;

    return Ok(());
}

struct LuaCode {
    pub code: String
}

fn parse_lua(content: String) -> IResult<String, LuaCode> {
    return Ok((content, LuaCode { code: "".to_string() }));
}