use crate::parsing::ast::{AssignmentStatement, Expression, get_operator_precedence, IdentifierExpression, InfixExpression, INITIAL_PRECEDENCE, IntExpression, PREFIX_PRECEDENCE, PrefixExpression, Program, ReturnStatement, Statement};
use crate::parsing::lexer::{Lexer, Token, TokenType};

type StatementParsingResult = Result<Box<dyn Statement>, ParsingError>;
type ExpressionParsingResult = Result<Box<dyn Expression>, ParsingError>;

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    next_token: Token,

    prefix_tokens: Vec<TokenType>,
    infix_tokens: Vec<TokenType>,
    // infix_parser: HashMap<TokenType, Box<dyn Fn(&mut Parser) -> ExpressionParsingResult + 'static>>,
    // prefix_parser: HashMap<TokenType, Box<dyn Fn(&mut Parser) -> ExpressionParsingResult + 'static>>,
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

impl Parser {
    pub fn new(input: String) -> Self {
        let mut p = Self {
            lexer: Lexer::new(input),
            current_token: Token::empty(),
            next_token: Token::empty(),
            prefix_tokens: vec![
                TokenType::Int,
                TokenType::Identifier,
                TokenType::Minus,
                TokenType::Tilde,
            ],
            infix_tokens: vec![
                TokenType::Or,
                TokenType::And,
                TokenType::Lower,
                TokenType::Greater,
                TokenType::LowerEqual,
                TokenType::GreaterEqual,
                TokenType::DoubleEquals,
                TokenType::TildeEqual,
                TokenType::Bar,
                TokenType::Tilde,
                TokenType::Ampersand,
                TokenType::ShiftLeft,
                TokenType::ShiftRight,
                TokenType::DoubleDot,
                TokenType::Plus,
                TokenType::Minus,
                TokenType::Star,
                TokenType::Slash,
                TokenType::DoubleSlash,
                TokenType::Percent,
                TokenType::Caret,
            ],
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

        match self.parse_expression(INITIAL_PRECEDENCE) {
            Ok(expr) => Ok(Box::new(AssignmentStatement::new(variable_token, expr))),
            Err(err) => Err(err),
        }
    }

    fn parse_return(&mut self) -> StatementParsingResult {
        match self.parse_expression(INITIAL_PRECEDENCE) {
            Ok(expr) => Ok(Box::new(ReturnStatement::new(expr))),
            Err(err) => Err(err),
        }
    }

    pub fn parse_expression(&mut self, precedence: i8) -> ExpressionParsingResult {
        let token_type = self.next_token.token_type.clone();
        if !self.prefix_tokens.contains(&token_type) {
            return Err(ParsingError::new(format!("{:?} is not a prefix token", token_type)));
        }

        let mut left = self.parse_prefix_expression();
        if left.is_err() { return left; }

        while !self.next_token_is_stop() && precedence < self.peek_precedence() {
            if !self.infix_tokens.contains(&self.next_token.token_type.clone()) { break }

            self.read_token();
            left = self.parse_infix_expression(left.unwrap());
        }

        return left;
    }

    fn parse_prefix_expression(&mut self) -> ExpressionParsingResult {
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
            TokenType::Identifier => {
                self.read_token();
                let tok = self.current_token.clone();
                Ok(Box::new(IdentifierExpression::new(tok.literal)))
            },

            _ => {
                self.read_token();

                let operator = self.current_token.token_type.clone();
                match self.parse_expression(PREFIX_PRECEDENCE) {
                    Ok(expr) => { Ok(Box::new(PrefixExpression::new(operator, expr))) }
                    Err(err) => { Err(err) }
                }
            }
        }
    }

    fn parse_infix_expression(&mut self, left: Box<dyn Expression>) -> ExpressionParsingResult {
        let operator = self.current_token.token_type.clone();
        let current_precedence = self.current_precedence();

        match self.parse_expression(current_precedence) {
            Ok(expr) => { Ok(Box::new(InfixExpression::new(left, operator, expr))) }
            Err(err) => { Err(err) }
        }
    }

    fn peek_precedence(&self) -> i8 {
        return get_operator_precedence(self.next_token.token_type.clone());
    }

    fn current_precedence(&self) -> i8 {
        return get_operator_precedence(self.current_token.token_type.clone());
    }

    fn next_token_is_stop(&self) -> bool {
        return vec![
            TokenType::SemiColon,
            TokenType::Local,
            TokenType::Return,
            TokenType::For,
            TokenType::If,
            TokenType::ElseIf,
            TokenType::Else,
            TokenType::End,
            TokenType::Eof,
        ].contains(&self.next_token.token_type);
    }

    fn read_token(&mut self) {
        let t = self.next_token.clone();
        self.current_token = t;
        self.next_token = self.lexer.next_token();
    }
}


#[cfg(test)]
mod tests {
    use crate::parsing::ast::{AssignmentStatement, IdentifierExpression, IfStatement, InfixExpression, INITIAL_PRECEDENCE, IntExpression, PrefixExpression, ReturnStatement};
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

    #[test]
    fn parse_simple_if_statements() {
        let input = r#"
            if n == 0 then
                return 1
            end
        "#;

        let mut parser = Parser::new(input.to_string());
        let output_program = parser.parse_program();
        let statements = output_program.statements;

        assert_eq!(statements.len(), 1);

        let stmt = statements.first().unwrap();
        assert!(stmt.as_any().is::<IfStatement>());
    }

    #[test]
    fn parse_simple_infix_expressions() {
        let input = vec![
            (
                "1 == 2",
                InfixExpression::new(
                    Box::new(IntExpression::new(1)),
                    TokenType::DoubleEquals,
                    Box::new(IntExpression::new(2))
                )
            ),
        ];

        for (i, expected_expr) in input {
            let mut parser = Parser::new(i.to_string());
            let result = parser.parse_expression(INITIAL_PRECEDENCE);
            assert_eq!(result.is_ok(), true, "{}", result.err().unwrap().message);

            let expr = result.unwrap();
            assert!(expr.as_any().is::<InfixExpression>());

            let infix = expr.as_any().downcast_ref::<InfixExpression>().unwrap();
            assert_eq!(infix.left_value.to_string(), expected_expr.left_value.to_string());
            assert_eq!(infix.operator, expected_expr.operator);
            assert_eq!(infix.right_value.to_string(), expected_expr.right_value.to_string());
        }
    }

    #[test]
    fn parse_simple_prefix_expressions() {
        let input = vec![
            ("-5", PrefixExpression::new(TokenType::Minus, Box::new(IntExpression::new(5)))),
            ("~foobar", PrefixExpression::new(TokenType::Tilde, Box::new(IdentifierExpression::new("foobar".to_string())))),
        ];

        for (i, expected_expr) in input {
            let mut parser = Parser::new(i.to_string());
            let result = parser.parse_expression(INITIAL_PRECEDENCE);
            assert!(result.is_ok());

            let expr = result.unwrap();
            assert!(expr.as_any().is::<PrefixExpression>());

            let prefix = expr.as_any().downcast_ref::<PrefixExpression>().unwrap();
            assert_eq!(prefix.operator, expected_expr.operator);
            assert_eq!(prefix.value.to_string(), expected_expr.value.to_string());
        }
    }
}