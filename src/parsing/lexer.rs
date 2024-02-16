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

impl TokenType {
    pub fn from_string(input: String) -> Self {
        match input.as_str() {
            "function" => TokenType::Function,
            "if" => TokenType::If,
            "then" => TokenType::Then,
            "else" => TokenType::Else,
            "end" => TokenType::End,
            "return" => TokenType::Return,
            _ => TokenType::Identifier,
        }
    }
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

        l.read_char();

        return l;
    }

    pub fn next_token(&mut self) -> Token {

        self.skip_whitespace();

        let tok = if let Some(ch) = self.ch {
            match ch {
                ',' => Token::new(TokenType::Coma, ch.to_string()),
                '=' => {
                    if let Some(other_character) = self.peek_char() && other_character == '=' {
                        self.read_char();
                        Token::new(TokenType::DoubleEquals, "==".to_string())
                    }
                    else { Token::new(TokenType::Equal, ch.to_string()) }

                },
                '+' => Token::new(TokenType::Plus, ch.to_string()),
                '-' => Token::new(TokenType::Minus, ch.to_string()),
                '*' => Token::new(TokenType::Star, ch.to_string()),
                '(' => Token::new(TokenType::LeftParen, ch.to_string()),
                ')' => Token::new(TokenType::RightParen, ch.to_string()),
                '{' => Token::new(TokenType::LeftBrace, ch.to_string()),
                '}' => Token::new(TokenType::RightBrace, ch.to_string()),
                ';' => Token::new(TokenType::SemiColon, ch.to_string()),
                _ => {
                    if self.is_character(ch) {
                        let identifier = self.read_identifier();
                        Token::new(TokenType::from_string(identifier.clone()), identifier)
                    }
                    else if self.is_digit(ch, false) {
                        let number = self.read_number();
                        // TODO: support floats
                        Token::new(TokenType::Int, number)
                    }
                    else {
                        Token::new(TokenType::Illegal, "".to_string())
                    }
                },
            }
        } else {
            Token::new(TokenType::Eof, "".to_string())
        };

        self.read_char();

        return tok;
    }

    fn skip_whitespace(&mut self) {

        while let Some(ch) = self.ch && ch.is_whitespace() {
            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> String {
        let mut identifier = String::new();

        if let Some(ch) = self.ch {
            identifier.push(ch);
        }

        while let Some(ch) = self.peek_char() {
            if !self.is_character(ch) { break }

            identifier.push(ch);
            self.read_char();
        }

        return identifier;
    }

    fn read_number(&mut self) -> String {
        let mut number = String::new();

        let is_hex_digit = if let Some(ch) = self.ch {
            number.push(ch);

            if let Some(next_char) = self.peek_char() && ch == '0' && next_char == 'x' {
                number.push(next_char);
                self.read_char();
                true
            }
            else { false }
        } else {
            false
        };

        while let Some(ch) = self.peek_char() {
            if !self.is_digit(ch, is_hex_digit) { break }

            number.push(ch);
            self.read_char();
        }

        return number;
    }

    fn read_char(&mut self) {
        if self.read_pos >= self.input.len() {
            self.ch = None;
        } else {
            self.ch = self.input.get(self.read_pos).map_or(None, |c| Some(c.clone()));
        }

        self.position = self.read_pos;
        self.read_pos += 1;
    }

    fn peek_char(&mut self) -> Option<char> {
        return if self.read_pos >= self.input.len() {
            None
        } else {
            self.input.get(self.read_pos).map_or(None, |c| Some(c.clone()))
        }
    }

    fn is_character(&self, c: char) -> bool {
        return c.is_alphabetic() || c == '_';
    }

    fn is_digit(&self, c: char, is_hex: bool) -> bool {
        return c.is_numeric() || (is_hex && c.is_ascii_hexdigit());
    }
}

#[cfg(test)]
mod tests {
    use crate::parsing::lexer::{Lexer, Token, TokenType};

    #[test]
    fn lex_simple_token_string() {
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

    #[test]
    fn lex_identifier() {
        let input = "(a) b == c";
        let expected_tokens = vec![
            Token::new(TokenType::LeftParen, "(".to_string()),
            Token::new(TokenType::Identifier, "a".to_string()),
            Token::new(TokenType::RightParen, ")".to_string()),
            Token::new(TokenType::Identifier, "b".to_string()),
            Token::new(TokenType::DoubleEquals, "==".to_string()),
            Token::new(TokenType::Identifier, "c".to_string()),
        ];
        let mut lexer = Lexer::new(input.to_string());

        for expected_token in expected_tokens {
            let token = lexer.next_token();

            assert_eq!(token.token_type, expected_token.token_type);
            assert_eq!(token.literal, expected_token.literal);
        }
    }

    #[test]
    fn lex_numbers() {
        let input = r#"
        3   345   0xff   0xBEBADA
        "#;

        let expected_tokens = vec![
            Token::new(TokenType::Int, "3".to_string()),
            Token::new(TokenType::Int, "345".to_string()),
            Token::new(TokenType::Int, "0xff".to_string()),
            Token::new(TokenType::Int, "0xBEBADA".to_string()),
        ];
        let mut lexer = Lexer::new(input.to_string());

        for expected_token in expected_tokens {
            let token = lexer.next_token();

            assert_eq!(token.token_type, expected_token.token_type);
            assert_eq!(token.literal, expected_token.literal);
        }
    }

    #[test]
    fn lex_greater_example() {
        let input = r#"
        function fact (n)
            if n == 0 then
                return 1
            else
                return n * fact(n-1)
            end
        end
        "#;
        let expected_tokens = vec![
            Token::new(TokenType::Function, "function".to_string()),
            Token::new(TokenType::Identifier, "fact".to_string()),
            Token::new(TokenType::LeftParen, "(".to_string()),
            Token::new(TokenType::Identifier, "n".to_string()),
            Token::new(TokenType::RightParen, ")".to_string()),
            Token::new(TokenType::If, "if".to_string()),
            Token::new(TokenType::Identifier, "n".to_string()),
            Token::new(TokenType::DoubleEquals, "==".to_string()),
            Token::new(TokenType::Int, "0".to_string()),
            Token::new(TokenType::Then, "then".to_string()),
            Token::new(TokenType::Return, "return".to_string()),
            Token::new(TokenType::Int, "1".to_string()),
            Token::new(TokenType::Else, "else".to_string()),
            Token::new(TokenType::Return, "return".to_string()),
            Token::new(TokenType::Identifier, "n".to_string()),
            Token::new(TokenType::Star, "*".to_string()),
            Token::new(TokenType::Identifier, "fact".to_string()),
            Token::new(TokenType::LeftParen, "(".to_string()),
            Token::new(TokenType::Identifier, "n".to_string()),
            Token::new(TokenType::Minus, "-".to_string()),
            Token::new(TokenType::Int, "1".to_string()),
            Token::new(TokenType::RightParen, ")".to_string()),
            Token::new(TokenType::End, "end".to_string()),
            Token::new(TokenType::End, "end".to_string()),
        ];
        let mut lexer = Lexer::new(input.to_string());

        for expected_token in expected_tokens {
            let token = lexer.next_token();

            assert_eq!(token.token_type, expected_token.token_type);
            assert_eq!(token.literal, expected_token.literal);
        }
    }
}