#[derive(Debug, Eq, Hash, PartialEq, Clone, Copy)]
pub enum TokenType {
    Illegal,
    Eof,
    Comment,

    Identifier,
    Int,
    Float,
    String,

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
    Comma,
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
            "and" => TokenType::And,
            "break" => TokenType::Break,
            "do" => TokenType::Do,
            "else" => TokenType::Else,
            "elseif" => TokenType::ElseIf,
            "end" => TokenType::End,
            "false" => TokenType::False,
            "for" => TokenType::For,
            "function" => TokenType::Function,
            "goto" => TokenType::Goto,
            "if" => TokenType::If,
            "in" => TokenType::In,
            "local" => TokenType::Local,
            "nil" => TokenType::Nil,
            "not" => TokenType::Not,
            "or" => TokenType::Or,
            "repeat" => TokenType::Repeat,
            "return" => TokenType::Return,
            "then" => TokenType::Then,
            "true" => TokenType::True,
            "until" => TokenType::Until,
            "while" => TokenType::While,
            _ => TokenType::Identifier,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,

    pub file_path: String,
    pub line: usize,
    pub column: usize,
}

impl Token {
    // used for testing
    fn new(token_type: TokenType, literal: String) -> Self {
        return Self {
            token_type,
            literal,
            file_path: "".to_string(),
            line: 0,
            column: 0,
        };
    }

    pub fn empty() -> Self {
        return Self {
            token_type: TokenType::Illegal,
            literal: "".to_string(),
            file_path: "".to_string(),
            line: 0,
            column: 0,
        };
    }

    pub fn is(&self, token_type: TokenType) -> bool {
        return self.token_type == token_type;
    }

    pub fn is_not(&self, token_type: TokenType) -> bool {
        return self.token_type != token_type;
    }

    pub fn is_not_one(&self, token_types: Vec<TokenType>) -> bool {
        return !token_types.contains(&self.token_type);
    }
}

pub struct Lexer {
    input: Vec<char>,

    // - debug info -
    pub line: usize,
    pub column: usize,
    // - debug info -

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
            line: 1,
            column: 0,
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
                '"' | '\'' => {
                    let str_val = self.read_string(ch);
                    self.create_token(TokenType::String, str_val)
                },
                ',' => self.create_token(TokenType::Comma, ch.to_string()),
                '=' => {
                    if let Some(other_character) = self.peek_char() && other_character == '=' {
                        self.read_char();
                        self.create_token(TokenType::DoubleEquals, "==".to_string())
                    }
                    else { self.create_token(TokenType::Equal, ch.to_string()) }

                },
                '+' => self.create_token(TokenType::Plus, ch.to_string()),
                '<'  => {
                    if let Some(other_character) = self.peek_char() && other_character == '=' {
                        self.read_char();
                        self.create_token(TokenType::LowerEqual, "<=".to_string())
                    }
                    else if let Some(other_character) = self.peek_char() && other_character == '<' {
                        self.read_char();
                        self.create_token(TokenType::ShiftLeft, "<<".to_string())
                    }
                    else { self.create_token(TokenType::Lower, ch.to_string()) }

                },
                '>'  => {
                    if let Some(other_character) = self.peek_char() && other_character == '=' {
                        self.read_char();
                        self.create_token(TokenType::GreaterEqual, ">=".to_string())
                    }
                    else if let Some(other_character) = self.peek_char() && other_character == '>' {
                        self.read_char();
                        self.create_token(TokenType::ShiftRight, ">>".to_string())
                    }
                    else { self.create_token(TokenType::Greater, ch.to_string()) }

                },
                '-' => {
                    if let Some(peek_char) = self.peek_char() {
                        if peek_char == '-' {
                            while let Some(ch) = self.ch && ch != '\n' {
                                self.read_char();
                            }

                            self.create_token(TokenType::Comment, "".to_string())
                        } else {
                            self.create_token(TokenType::Minus, ch.to_string())
                        }
                    } else {
                        self.create_token(TokenType::Minus, ch.to_string())
                    }
                },
                '*' => self.create_token(TokenType::Star, ch.to_string()),
                '(' => self.create_token(TokenType::LeftParen, ch.to_string()),
                ')' => self.create_token(TokenType::RightParen, ch.to_string()),
                '{' => self.create_token(TokenType::LeftBrace, ch.to_string()),
                '}' => self.create_token(TokenType::RightBrace, ch.to_string()),
                ';' => self.create_token(TokenType::SemiColon, ch.to_string()),
                ':' => self.create_token(TokenType::Colon, ch.to_string()),
                '.' => self.create_token(TokenType::Dot, ch.to_string()),
                '/' => self.create_token(TokenType::Slash, ch.to_string()),
                '#' => self.create_token(TokenType::Hash, ch.to_string()),
                '&' => self.create_token(TokenType::Ampersand, ch.to_string()),
                '~' => self.create_token(TokenType::Tilde, ch.to_string()),
                '%' => self.create_token(TokenType::Percent, ch.to_string()),
                '|' => self.create_token(TokenType::Bar, ch.to_string()),
                '^' => self.create_token(TokenType::Caret, ch.to_string()),
                _ => {
                    if self.is_character(ch, false) {
                        let identifier = self.read_identifier();
                        self.create_token(TokenType::from_string(identifier.clone()), identifier)
                    }
                    else if self.is_digit(ch, false) {
                        let number = self.read_number();
                        // TODO: support floats
                        self.create_token(TokenType::Int, number)
                    }
                    else {
                        self.create_token(TokenType::Illegal, "".to_string())
                    }
                },
            }
        } else {
            self.create_token(TokenType::Eof, "".to_string())
        };

        self.read_char();

        return tok;
    }

    fn skip_whitespace(&mut self) {

        while let Some(ch) = self.ch && ch.is_whitespace() {
            if ch == '\n' {
                self.line += 1;
            }

            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> String {
        let mut identifier = String::new();

        if let Some(ch) = self.ch {
            identifier.push(ch);
        }

        while let Some(ch) = self.peek_char() {
            if !self.is_character(ch, true) { break }

            identifier.push(ch);
            self.read_char();
        }

        return identifier;
    }

    fn read_string(&mut self, end_char: char) -> String {
        let mut string_val = String::new();

        while let Some(ch) = self.peek_char() {
            if ch == end_char { break }

            string_val.push(ch);
            self.read_char();
        }

        self.read_char();

        return string_val;
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

        self.column += 1;
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

    fn is_character(&self, c: char, allow_numbers: bool) -> bool {
        return (!allow_numbers && c.is_alphabetic()) || (allow_numbers && c.is_alphanumeric()) || c == '_';
    }

    fn is_digit(&self, c: char, is_hex: bool) -> bool {
        return c.is_numeric() || (is_hex && c.is_ascii_hexdigit());
    }

    fn create_token(&self, token_type: TokenType, literal: String) -> Token {
        return Token {
            token_type,
            literal,
            file_path: "".to_string(),
            line: self.line,
            column: self.column,
        }
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
            Token::new(TokenType::Comma, ",".to_string()),
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

        function norm (x, y)
            local n2 = x^2 + y^2
            return math.sqrt(n2)
        end

        function twice (x)
            return 2*x
        end
        "#;
        let expected_tokens = vec![
            // function fact
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

            // function norm
            Token::new(TokenType::Function, "function".to_string()),
            Token::new(TokenType::Identifier, "norm".to_string()),
            Token::new(TokenType::LeftParen, "(".to_string()),
            Token::new(TokenType::Identifier, "x".to_string()),
            Token::new(TokenType::Comma, ",".to_string()),
            Token::new(TokenType::Identifier, "y".to_string()),
            Token::new(TokenType::RightParen, ")".to_string()),
            Token::new(TokenType::Local, "local".to_string()),
            Token::new(TokenType::Identifier, "n2".to_string()),
            Token::new(TokenType::Equal, "=".to_string()),
            Token::new(TokenType::Identifier, "x".to_string()),
            Token::new(TokenType::Caret, "^".to_string()),
            Token::new(TokenType::Int, "2".to_string()),
            Token::new(TokenType::Plus, "+".to_string()),
            Token::new(TokenType::Identifier, "y".to_string()),
            Token::new(TokenType::Caret, "^".to_string()),
            Token::new(TokenType::Int, "2".to_string()),
            Token::new(TokenType::Return, "return".to_string()),
            Token::new(TokenType::Identifier, "math".to_string()),
            Token::new(TokenType::Dot, ".".to_string()),
            Token::new(TokenType::Identifier, "sqrt".to_string()),
            Token::new(TokenType::LeftParen, "(".to_string()),
            Token::new(TokenType::Identifier, "n2".to_string()),
            Token::new(TokenType::RightParen, ")".to_string()),
            Token::new(TokenType::End, "end".to_string()),

            // function twice
            Token::new(TokenType::Function, "function".to_string()),
            Token::new(TokenType::Identifier, "twice".to_string()),
            Token::new(TokenType::LeftParen, "(".to_string()),
            Token::new(TokenType::Identifier, "x".to_string()),
            Token::new(TokenType::RightParen, ")".to_string()),
            Token::new(TokenType::Return, "return".to_string()),
            Token::new(TokenType::Int, "2".to_string()),
            Token::new(TokenType::Star, "*".to_string()),
            Token::new(TokenType::Identifier, "x".to_string()),
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