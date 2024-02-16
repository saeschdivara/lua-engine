use crate::parsing::ast::{AssignmentStatement, Expression, IntExpression, Program, ReturnStatement, Statement};
use crate::parsing::lexer::{Lexer, Token, TokenType};

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    next_token: Token,
}

#[derive(Debug)]
pub struct ParsingError {
    pub message: String
}

impl ParsingError {
    pub fn new(message: String) -> Self {
        return Self {
            message,
        }
    }
}

type StatementParsingResult = Result<Box<dyn Statement>, ParsingError>;
type ExpressionParsingResult = Result<Box<dyn Expression>, ParsingError>;

impl Parser {
    pub fn new(input: String) -> Self {
        let mut p = Self {
            lexer: Lexer::new(input),
            current_token: Token::empty(),
            next_token: Token::empty(),
        };

        p.read_token();

        return p;
    }

    pub fn parse_program(&mut self) -> Program {
        let mut statements = vec![];

        self.read_token();

        while self.current_token.is_not(TokenType::Eof) {
            match self.parse_statement() {
                Ok(stmt) => {
                    statements.push(stmt);
                }
                Err(err) => {
                    eprintln!("Parsing failed of statement: {:?}", err);
                }
            }
            self.read_token();
        }

        return Program {
            statements,
        }
    }

    fn parse_statement(&mut self) -> StatementParsingResult {
        match self.current_token.token_type {
            TokenType::Local => {
                self.parse_local_assignment()
            },
            TokenType::Return => {
                self.parse_return()
            },
            _ => Err(ParsingError::new(format!("Unknown token_type found: {:?}", self.current_token.token_type)))
        }
    }

    fn parse_local_assignment(&mut self) -> StatementParsingResult {
        if self.next_token.is_not(TokenType::Identifier) {
            return Err(ParsingError::new(format!("Next token is not an identifier: {:?}", self.next_token)))
        }

        self.read_token();
        let variable_token = self.current_token.clone();

        if self.next_token.is_not(TokenType::Equal) {
            return Err(ParsingError::new(format!("Next token is not equals: {:?}", self.next_token)))
        }

        self.read_token();

        match self.parse_expression() {
            Ok(expr) => Ok(Box::new(AssignmentStatement::new(variable_token, expr))),
            Err(err) => Err(err),
        }
    }

    fn parse_return(&mut self) -> StatementParsingResult {
        match self.parse_expression() {
            Ok(expr) => Ok(Box::new(ReturnStatement::new(expr))),
            Err(err) => Err(err),
        }
    }

    fn parse_expression(&mut self) -> ExpressionParsingResult {
        match self.next_token.token_type {
            TokenType::Int => {
                self.read_token();
                let tok = self.current_token.clone();
                let value = tok.literal.parse::<i64>();

                if let Ok(number) = value {
                    Ok(Box::new(IntExpression::new(number)))
                } else {
                    Err(ParsingError::new("Failed to parse int".to_string()))
                }
            },
            _ => Err(ParsingError::new("unknown error".to_string()))
        }
    }

    fn read_token(&mut self) {
        let t = self.next_token.clone();
        self.current_token = t;
        self.next_token = self.lexer.next_token();
    }
}


#[cfg(test)]
mod tests {
    use crate::parsing::ast::{AssignmentStatement, IntExpression, ReturnStatement};
    use crate::parsing::lexer::TokenType;
    use crate::parsing::parser::Parser;

    #[test]
    fn parse_simple_assignment_statements() {
        let input = r#"
            local n = 1
            local x = 2
        "#;

        let expected_identifiers = vec!["n", "x"];
        let mut parser = Parser::new(input.to_string());
        let output_program = parser.parse_program();
        let statements = output_program.statements;

        assert_eq!(statements.len(), 2);

        for i in 0..expected_identifiers.len() {
            let expected_ident = expected_identifiers[i];
            let stmt = &statements[i];

            assert!(stmt.as_any().is::<AssignmentStatement>());

            let assignment = stmt.as_any().downcast_ref::<AssignmentStatement>().unwrap();
            assert_eq!(assignment.variable.token_type, TokenType::Identifier);
            assert_eq!(&assignment.variable.literal, expected_ident);
        }
    }

    #[test]
    fn parse_simple_return_statement() {
        let input = r#"
            return 1
        "#;

        let mut parser = Parser::new(input.to_string());
        let output_program = parser.parse_program();
        let statements = output_program.statements;

        assert_eq!(statements.len(), 1);
        let stmt = statements.first().unwrap();

        assert!(stmt.as_any().is::<ReturnStatement>());
        let ret = stmt.as_any().downcast_ref::<ReturnStatement>().unwrap();

        assert!(ret.value.as_any().is::<IntExpression>());
        let int = ret.value.as_any().downcast_ref::<IntExpression>().unwrap();

        assert_eq!(int.value, 1);
    }
}