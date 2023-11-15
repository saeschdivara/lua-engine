use std::error::Error;
use crate::ast::Token;
use crate::parser::{Parser, ParsingError};

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Variable { name: String, token: Token },
    String { value: String, token: Token }
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
            Err(Box::new(ParsingError::new("No parsing function found for token")))
        }
    }

    pub(crate) fn parse_string(&self, current_token: Token, mut tokens: Vec<Token>) -> ExpressionParsingResult {
        if let Token::String { literal, .. } = current_token.clone() {
            Ok((
                tokens,
                Expression::String { value: literal, token: current_token }
            ))
        } else {
            Err(Box::new(ParsingError::new("Token is not an identifier")))
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
    fn it_works() {
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
}