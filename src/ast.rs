
#[derive(Clone, PartialEq, Debug)]
pub struct Location {
    pub file: String,
    pub row: u64,
    pub column: u64,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Keyword {
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

#[derive(Clone, PartialEq, Debug)]
pub enum Number {
    Int(i64),
    Float(f64),
}

#[derive(Clone, PartialEq, Debug)]
pub enum Token {
    Invalid,
    WhiteSpace { count: u8 },
    String { literal: String, quote: char },

    // any string of Latin letters, Arabic-Indic digits, and underscores,
    // not beginning with a digit and not being a reserved word
    Identifier { literal: String },
    Keyword { literal: Keyword },

    Number { literal: String, value: Number },

    Plus {},
    Minus {},
    Star {},
    Slash {},
    Percentage {},
    // ^
    Caret {},
    Hash {},
    BinaryAnd {},
    Tilde {},
    // |
    VerticalBar {},
    // <<
    ShiftLeft {},
    // >>
    ShiftRight {},
    DoubleSlash {},
    DoubleEqual {},
    TildeEqual {},
    LowerEqual {},
    GreaterEqual {},
    Lower {},
    Greater {},
    Equal {},

    // (
    LeftParen {},
    // )
    RightParen {},
    // {
    LeftCurlyBracket {},
    // }
    RightCurlyBracket {},
    // [
    LeftBracket {},
    // ]
    RightBracket {},

    // ::
    DoubleColon {},
    SemiColon {},
    Colon {},
    Comma {},
    Dot {},
    DoubleDot {},
    TripleDot {},
}