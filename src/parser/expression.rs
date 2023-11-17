use std::error::Error;
use crate::ast::{Number, Token};
use crate::parser::{Parser, ParsingError};

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Variable { name: String, token: Token },
    String { value: String, token: Token },
    Number { value: Number, token: Token },
}

pub type ExpressionParsingResult = Result<(Vec<Token>, Expression), Box<dyn Error>>;

pub enum OperatorPrecedence {
    Lowest = 0,
    Or = 1,
    And = 2,
    Compare = 3,
    // <     >     <=    >=    ~=    ==
    BitOr = 4,
    // |
    BitNot = 5,
    // ~
    BitAnd = 6,
    // ~
    BitShift = 7,
    // <<    >>
    Concat = 8,
    // ..
    Sum = 9,
    // +     -
    Multiplication = 10,
    //  *     /     //    %
    Unary = 11,
    //  unary operators (not   #     -     ~)
    Exponentiation = 12, // ^
}

impl Parser {
    pub(crate) fn parse_expression(&self, mut tokens: Vec<Token>, precedence: OperatorPrecedence) -> ExpressionParsingResult {
        let current_token = tokens.remove(0);
        if let Some(prefix_function) = self.get_prefix_parser(&current_token) {
            prefix_function(self, current_token, tokens)
        } else {
            Err(Box::new(ParsingError::new(
                format!("No parsing function found for token {:?}", current_token).as_str()
            )))
        }
    }

    pub(crate) fn parse_string(&self, current_token: Token, mut tokens: Vec<Token>) -> ExpressionParsingResult {
        if let Token::String { literal, .. } = current_token.clone() {
            Ok((
                tokens,
                Expression::String { value: literal, token: current_token }
            ))
        } else {
            Err(Box::new(ParsingError::new("Token is not a string")))
        }
    }

    pub(crate) fn parse_number(&self, current_token: Token, mut tokens: Vec<Token>) -> ExpressionParsingResult {
        if let Token::Number { value, .. } = current_token.clone() {
            Ok((
                tokens,
                Expression::Number { value, token: current_token }
            ))
        } else {
            Err(Box::new(ParsingError::new("Token is not a number")))
        }
    }

    fn parse_variable(&self, current_token: Token, mut tokens: Vec<Token>) -> ExpressionParsingResult {
        if let Token::Identifier { literal } = current_token.clone() {
            Ok((
                tokens,
                Expression::Variable { name: literal, token: current_token }
            ))
        } else {
            Err(Box::new(ParsingError::new("Token is not an identifier")))
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_string_expr() {
        let string_token = Token::String { literal: "String Content".to_string(), quote: '"' };
        let tokens = vec![
            string_token.clone(),
            Token::SemiColon {},
        ];

        let parser = Parser::new();

        match parser.parse_expression(tokens, OperatorPrecedence::Lowest) {
            Ok((tokens, expr)) => {
                assert_eq!(tokens.len(), 1);

                let expected_expr = Expression::String { value: "String Content".to_string(), token: string_token };
                assert_eq!(expected_expr, expr);
            }
            Err(msg) => {
                assert_eq!(true, false, "Parsing expression failed: {:?}", msg);
                return;
            }
        }
    }

    #[test]
    fn parse_number_expr() {
        let token = Token::Number { value: Number::Int(5), literal: "5".to_string() };
        let tokens = vec![
            token.clone(),
            Token::SemiColon {},
        ];

        let parser = Parser::new();

        match parser.parse_expression(tokens, OperatorPrecedence::Lowest) {
            Ok((tokens, expr)) => {
                assert_eq!(tokens.len(), 1);

                let expected_expr = Expression::Number { value: Number::Int(5), token };
                assert_eq!(expected_expr, expr);
            }
            Err(msg) => {
                assert_eq!(true, false, "Parsing expression failed: {:?}", msg);
                return;
            }
        }
    }
}