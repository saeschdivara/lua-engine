use std::error::Error;
use crate::ast::Token;
use crate::parser::ParsingError;

#[derive(Debug, Clone)]
pub enum Expression {
    Variable { name: String, token: Token }
}

pub type ExpressionParsingResult = Result<(Vec<Token>, Expression), Box<dyn Error>>;

pub enum OperatorPrecedence {
    Lowest = 0,
    Or = 1,
    And = 2,
    Compare = 3, // <     >     <=    >=    ~=    ==
    BitOr = 4, // |
    BitNot = 5, // ~
    BitAnd = 6, // ~
    BitShift = 7, // <<    >>
    Concat = 8, // ..
    Sum = 9, // +     -
    Multiplication = 10, //  *     /     //    %
    Unary = 11, //  unary operators (not   #     -     ~)
    Exponentiation = 12 // ^
}

pub fn parse_expression(mut tokens: Vec<Token>, precedence: OperatorPrecedence) -> ExpressionParsingResult {

    //

    Err(Box::new(ParsingError::new("Could not parse expression")))
}

fn parse_variable(current_token: Token, mut tokens: Vec<Token>) -> ExpressionParsingResult {
    if let Token::Identifier { literal } = current_token.clone() {
        Ok((
            tokens,
            Expression::Variable { name: literal, token: current_token }
        ))
    } else {
        Err(Box::new(ParsingError::new("Token is not an identifier")))
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let tokens = vec![
            Token::String { literal: "String Content".to_string(), quote: '"' },
            Token::SemiColon {},
        ];

        match parse_expression(tokens, OperatorPrecedence::Lowest) {
            Ok((tokens, expr)) => {
                assert_eq!(tokens.len(), 1);
            }
            Err(msg) => {
                assert_eq!(true, false, "Parsing expression failed: {:?}", msg);
                return;
            }
        }
    }
}