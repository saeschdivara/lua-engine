use std::error::Error;
use crate::ast::{Keyword, Number, Token};
use crate::parser::{Parser, ParsingError, Statement};

#[derive(Debug, PartialEq, Clone)]
pub enum Table {
    Array(Vec<Expression>)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Variable { name: String, token: Token },
    String { value: String, token: Token },
    Number { value: Number, token: Token },
    Table { value: Table, token: Token },
    Function { name: Option<String>, parameters: Vec<Expression>, body: Vec<Statement>, token: Token },
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

    pub(crate) fn parse_table(&self, current_token: Token, mut tokens: Vec<Token>) -> ExpressionParsingResult {
        if let Token::LeftCurlyBracket {} = current_token.clone() {
            let mut values = vec![];

            loop {
                let current_token = tokens.remove(0);

                match current_token {
                    Token::RightCurlyBracket { .. } => {
                        break
                    }
                    Token::Comma {} | Token::NewLine {} => {
                        continue
                    }
                    _ => {
                        tokens.insert(0, current_token.clone());
                        match self.parse_expression(tokens.clone(), OperatorPrecedence::Lowest) {
                            Ok((new_tokens, expr)) => {
                                tokens = new_tokens;
                                values.push(expr);
                            }
                            Err(e) => {
                                return Err(e);
                            }
                        }
                    }
                }
            }

            Ok((
                tokens,
                Expression::Table { value: Table::Array(values), token: current_token, }
            ))
        } else {
            Err(Box::new(ParsingError::new("Token is not a {")))
        }
    }

    pub(crate) fn parse_variable(&self, current_token: Token, mut tokens: Vec<Token>) -> ExpressionParsingResult {
        if let Token::Identifier { literal } = current_token.clone() {
            Ok((
                tokens,
                Expression::Variable { name: literal, token: current_token }
            ))
        } else {
            Err(Box::new(ParsingError::new("Token is not an identifier")))
        }
    }

    pub(crate) fn parse_function(&self, current_token: Token, mut tokens: Vec<Token>) -> ExpressionParsingResult {
        if let Token::Keyword { literal: Keyword::Function {} } = current_token.clone() {
            let mut current_token = tokens.remove(0);
            let function_name = match current_token {
                Token::Identifier { literal } => {
                    current_token = tokens.remove(0);
                    Some(literal)
                }
                Token::LeftParen { .. } => {
                    None
                }
                _ => return Err(Box::new(ParsingError::new("Function expected either identifier or (")))
            };

            match current_token {
                Token::LeftParen { .. } => {}
                _ => return Err(Box::new(ParsingError::new("Function expected (")))
            }

            let mut parameters = vec![];
            let mut tokens_alias = tokens;

            loop {
                current_token = tokens_alias.remove(0);

                match current_token.clone() {
                    Token::Comma { .. } => continue,
                    Token::RightParen { .. } => break,
                    Token::Identifier { .. } => {
                        let (tokens, expr) = self.parse_variable(current_token, tokens_alias).unwrap();
                        parameters.push(expr);
                        tokens_alias = tokens;
                    }
                    _ => return Err(Box::new(ParsingError::new("Wrong token in function parameters")))
                }
            }

            let mut body = vec![];

            loop {
                let peek_token = tokens_alias.get(0);
                if peek_token.is_some() {
                    let peek_token = peek_token.unwrap();

                    match peek_token {
                        Token::Keyword { literal: Keyword::End } => {
                            break
                        },
                        _ => {
                            match self.parse_next_statement(tokens_alias) {
                                Ok((tokens, stmt)) => {
                                    body.push(stmt);
                                    tokens_alias = tokens;
                                }
                                Err(err) => return Err(err)
                            }
                        }
                    }

                } else {
                    return Err(Box::new(ParsingError::new("Missing token")))
                }

                break
            }

            Ok((tokens_alias, Expression::Function {
                name: function_name,
                parameters,
                body,
                token: Token::Invalid
            }))
        } else {
            Err(Box::new(ParsingError::new("Token is not an function")))
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

    #[test]
    fn parse_table_list_expr() {
        let token = Token::Number { value: Number::Int(5), literal: "5".to_string() };
        let tokens = vec![
            Token::LeftCurlyBracket {},
            token.clone(),
            Token::Comma {},
            token.clone(),
            Token::RightCurlyBracket {},
            Token::SemiColon {},
        ];

        let parser = Parser::new();

        match parser.parse_expression(tokens, OperatorPrecedence::Lowest) {
            Ok((tokens, expr)) => {
                assert_eq!(tokens.len(), 1);

                let expected_expr = Expression::Table { value:
                    Table::Array(vec![
                        Expression::Number { value: Number::Int(5), token: token.clone() },
                        Expression::Number { value: Number::Int(5), token: token.clone() },
                    ]),
                    token: Token::LeftCurlyBracket {}
                };
                assert_eq!(expected_expr, expr);
            }
            Err(msg) => {
                assert_eq!(true, false, "Parsing expression failed: {:?}", msg);
                return;
            }
        }
    }

    #[test]
    fn parse_local_function_expr() {
        let function_name_tok = Token::Identifier { literal: "_I".to_string() };
        let param_name_tok = Token::Identifier { literal: "msg".to_string() };
        let var_name_tok = Token::Identifier { literal: "x".to_string() };
        let value_token = Token::Number { value: Number::Int(5), literal: "5".to_string() };
        let tokens = vec![
            Token::Keyword { literal: Keyword::Function },
            function_name_tok.clone(),
            Token::LeftParen {},
            param_name_tok.clone(),
            Token::RightParen {},
            Token::NewLine {},
            Token::Keyword { literal: Keyword::Local },
            var_name_tok.clone(),
            Token::Equal {},
            value_token.clone(),
            Token::SemiColon {},
            Token::NewLine {},
            Token::Keyword { literal: Keyword::End },
        ];

        let parser = Parser::new();

        match parser.parse_local_statement(tokens) {
            Ok((tokens, stmt)) => {
                println!("Stmt: {:?}", stmt);
                assert_eq!(tokens.len(), 1);

                // let expected_expr = Expression::Table { value:
                //     Table::Array(vec![
                //         Expression::Number { value: Number::Int(5), token: function_name_tok.clone() },
                //         Expression::Number { value: Number::Int(5), token: function_name_tok.clone() },
                //     ]),
                //     token: Token::LeftCurlyBracket {}
                // };
                // assert_eq!(expected_expr, stmt);
            }
            Err(msg) => {
                assert_eq!(true, false, "Parsing expression failed: {:?}", msg);
                return;
            }
        }
    }
}