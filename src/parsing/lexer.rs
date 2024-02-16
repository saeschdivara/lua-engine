#[derive(Debug, PartialEq)]
pub enum TokenType {
    Illegal,
    Eof,

    Identifier,
    Int,
    Float,

    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    Hash,
    Ampersand,
    Tilde,
    Bar,
    ShiftLeft,
    ShiftRight,
    DoubleSlash,
    DoubleEquals,
    TildeEqual,
    LowerEqual,
    GreaterEqual,
    Lower,
    Greater,
    Equal,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    DoubleColon,
    SemiColon,
    Colon,
    Coma,
    Dot,
    DoubleDot,
    TripleDot,

    // keywords
    And,
    Break,
    Do,
    Else,
    ElseIf,
    End,
    False,
    For,
    Function,
    Goto,
    If,
    In,
    Local,
    Nil,
    Not,
    Or,
    Repeat,
    Return,
    Then,
    True,
    Until,
    While,
}

pub struct Token {
    pub token_type: TokenType,
    pub literal: String,

    pub file_path: String,
    pub line: u64,
    pub column: u64,
}

impl Token {
    fn new(token_type: TokenType, literal: String) -> Self {
        return Self {
            token_type,
            literal,
            file_path: "".to_string(),
            line: 0,
            column: 0,
        };
    }
}

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    // current position in input (points to current char)
    read_pos: usize,
    // current reading position in input (after current char)
    ch: Option<char>,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut l = Self {
            input: input.chars().collect(),
            position: 0,
            read_pos: 0,
            ch: None,
        };

        l.next_char();

        return l;
    }

    pub fn next_token(&mut self) -> Token {
        let tok = if let Some(ch) = self.ch {
            match ch {
                ',' => Token::new(TokenType::Coma, ch.to_string()),
                '=' => Token::new(TokenType::Equal, ch.to_string()),
                '+' => Token::new(TokenType::Plus, ch.to_string()),
                '(' => Token::new(TokenType::LeftParen, ch.to_string()),
                ')' => Token::new(TokenType::RightParen, ch.to_string()),
                '{' => Token::new(TokenType::LeftBrace, ch.to_string()),
                '}' => Token::new(TokenType::RightBrace, ch.to_string()),
                ';' => Token::new(TokenType::SemiColon, ch.to_string()),
                _ => Token::new(TokenType::Illegal, "".to_string()),
            }
        } else {
            Token::new(TokenType::Eof, "".to_string())
        };

        self.next_char();

        return tok;
    }

    fn next_char(&mut self) {
        if self.read_pos >= self.input.len() {
            self.ch = None;
        } else {
            self.ch = self.input.get(self.read_pos).map_or(None, |c| Some(c.clone()));
        }

        self.position = self.read_pos;
        self.read_pos += 1;
    }
}

#[cfg(test)]
mod tests {
    use crate::parsing::lexer::{Lexer, Token, TokenType};

    #[test]
    fn parse_simple_token_string() {
        let input = "=+(){},;";
        let expected_tokens = vec![
            Token::new(TokenType::Equal, "=".to_string()),
            Token::new(TokenType::Plus, "+".to_string()),
            Token::new(TokenType::LeftParen, "(".to_string()),
            Token::new(TokenType::RightParen, ")".to_string()),
            Token::new(TokenType::LeftBrace, "{".to_string()),
            Token::new(TokenType::RightBrace, "}".to_string()),
            Token::new(TokenType::Coma, ",".to_string()),
            Token::new(TokenType::SemiColon, ";".to_string()),
        ];
        let mut lexer = Lexer::new(input.to_string());

        for expected_token in expected_tokens {
            let token = lexer.next_token();

            assert_eq!(token.token_type, expected_token.token_type);
            assert_eq!(token.literal, expected_token.literal);
        }
    }
}