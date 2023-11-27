use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

use crate::ast::{Keyword, Token};
use crate::parser::expression::{Expression, ExpressionParsingResult, OperatorPrecedence};

mod expression;

#[derive(Debug, PartialEq, Clone)]
pub struct Statement {
    pub expressions: Vec<Expression>,
}

pub struct LuaCode {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct ParsingError {
    message: String,
}

impl ParsingError {
    fn new(message: &str) -> Self {
        return Self {
            message: message.to_string(),
        };
    }
}

impl Error for ParsingError {}

impl Display for ParsingError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Could not parse it: {}", self.message)
    }
}

pub struct Parser {
}

impl Parser {
    pub fn new() -> Self {
        Self {}
    }

    pub fn parse_lua_ast(&self, mut token_stream: Vec<Token>) -> Result<LuaCode, Box<dyn Error>> {
        let mut statements = vec![];

        loop {
            match self.parse_next_statement(token_stream) {
                Ok((tokens, stmt)) => {
                    println!("Stmt: {:?}", stmt.clone());
                    statements.push(stmt);
                    token_stream = tokens;
                }
                Err(err) => {
                    println!("{}", err);
                    break
                }
            }
        }

        Ok(LuaCode { statements })
    }

    fn parse_next_statement(&self, mut tokens: Vec<Token>) -> Result<(Vec<Token>, Statement), Box<dyn Error>> {
        let mut current_token = tokens.remove(0);
        let nl = Token::NewLine {};

        while current_token == nl {
            current_token = tokens.remove(0);
        }

        match current_token {
            Token::Keyword { literal } => {
                match literal {
                    Keyword::Local => self.parse_local_statement(tokens),
                    _ => Err(Box::new(ParsingError::new("Invalid keyword for statement beginning")))
                }
            }
            Token::Identifier {literal} => {
                let (updated_tokens, path_tokens) = self.parse_identifier_path(tokens)?;
                let ident_token = Token::Identifier {literal: literal + "." + &*path_tokens };
                let operation_token = updated_tokens.get(0).unwrap();

                match operation_token {
                    Token::Equal { .. } => return Err(Box::new(ParsingError::new("Variable assignment is not yet supported"))),
                    Token::LeftParen { .. } => {
                        //
                    },
                    _ => return Err(Box::new(ParsingError::new("Unsupported operation detected")))
                }

                Err(Box::new(ParsingError::new("Cannot handle identifier statements")))
            }
            _ => Err(Box::new(ParsingError::new("Statement has to begin with keyword")))
        }
    }

    fn parse_local_statement(&self, mut tokens: Vec<Token>) -> Result<(Vec<Token>, Statement), Box<dyn Error>> {
        let mut stmt = Statement { expressions: vec![] };
        let current_token = tokens.remove(0);
        match current_token.clone() {
            Token::Identifier { literal } => {
                stmt.expressions.push(Expression::Variable { name: literal, token: current_token })
            }
            Token::Keyword { literal } => {
                return match literal {
                    Keyword::Function => {
                        match self.parse_function(current_token, tokens) {
                            Ok((tokens, expr)) => {
                                stmt.expressions.push(expr);
                                Ok((tokens, stmt))
                            }

                            Err(e) => Err(e)
                        }
                    }
                    _ => Err(Box::new(ParsingError::new("Wrong keyword used after local")))
                }
            }
            _ => return Err(Box::new(ParsingError::new("Local expects identifier")))
        };

        let current_token = tokens.remove(0);
        match current_token {
            Token::Equal { .. } => {}
            _ => return Err(Box::new(ParsingError::new("Expect = after identifier")))
        };

        let (mut tokens, value) = self.parse_expression(tokens, OperatorPrecedence::Lowest)?;
        stmt.expressions.push(value);

        let current_token = tokens.remove(0);
        match current_token {
            Token::SemiColon { .. } => {}
            _ => return Err(Box::new(ParsingError::new("Statement has to end with ;")))
        };

        Ok((tokens, stmt))
    }

    fn parse_identifier_path(&self, mut tokens: Vec<Token>) -> Result<(Vec<Token>, String), Box<dyn Error>> {
        let mut token_path = vec![];
        loop {
            if let Some(peek_token) = tokens.get(0) {
                match peek_token {
                    Token::Identifier { .. } | Token::Dot {} => {
                        let token = tokens.remove(0);
                        token_path.push(token);
                    }
                    _ => {
                        break
                    }
                }
            } else {
                return Err(Box::new(ParsingError::new("End of token stream when expecting more")));
            }
        }

        let path = token_path.iter().map(|t| match t {
            Token::Identifier { literal } => literal.clone(),
            Token::Dot {} => ".".to_string(),
            _ => panic!("This should never happen")
        }).collect::<Vec<String>>().join("");

        return Ok((tokens, path));
    }

    fn get_prefix_parser(&self, token: &Token) -> Option<fn(&Parser, Token, Vec<Token>) -> ExpressionParsingResult> {
        match token {
            Token::String { .. } => Some(Parser::parse_string),
            Token::Number { .. } => Some(Parser::parse_number),
            Token::LeftCurlyBracket { .. } => Some(Parser::parse_table),
            _ => None
        }
    }
}