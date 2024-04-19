#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Use,
    Public,
    Operation,
    Type,
    Identifier,
    IntegerLiteral,
    CharLiteral,
    StringLiteral,
    TrueLiteral,
    FalseLiteral,
    ExactLiteral,
    Comment,
    Colon,
    AtMark,
    LeftParenthesis,
    RightParenthesis,
    LeftCurlyBrace,
    RightCurlyBrace,
    LeftBlockBrace,
    RightBlockBrace,
    Comma,
    Not,
    Equal,
    Arrow,
    Semicolon,
    If,
    Else,
    For,
    While,
    Match,
    Return,
    Declare,
    Question,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,
    GreaterThan,
    LessThan,
    GreaterThanEqual,
    LessThanEqual,
    EqualTo,
    NotEqualTo,
    And,
    Or,
    None,
    Constant,
}

impl TokenKind {
    pub fn precedence(&self) -> i8 {
        match self {
            Self::Multiply | Self::Divide | Self::Modulus => 6,
            Self::Add | Self::Subtract => 5,
            Self::LessThan | Self::LessThanEqual | Self::GreaterThan | Self::GreaterThanEqual => 4,
            Self::EqualTo | Self::NotEqualTo => 3,
            Self::And => 2,
            Self::Or => 1,
            _ => 0,
        }
    }

    pub fn is_arithmetic(&self) -> bool {
        match self.to_owned() {
            Self::Multiply
            | Self::Divide
            | Self::Modulus
            | Self::Add
            | Self::Subtract
            | Self::LessThan
            | Self::LessThanEqual
            | Self::GreaterThan
            | Self::GreaterThanEqual
            | Self::EqualTo
            | Self::NotEqualTo
            | Self::And
            | Self::Or => true,
            _ => false,
        }
    }

    pub fn is_literal(&self) -> bool {
        match self.to_owned() {
            Self::StringLiteral
            | Self::IntegerLiteral
            | Self::CharLiteral
            | Self::ExactLiteral
            | Self::TrueLiteral
            | Self::FalseLiteral => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ValueKind {
    String(String),
    Number(i64),
    Character(char),
    Nil,
}

#[derive(Debug, Clone)]
pub struct Location {
    pub file: String,
    pub row: usize,
    pub column: usize,
}

impl Location {
    pub fn display(&mut self) -> String {
        return format!("{}:{}:{}", self.file, self.row + 1, self.column + 1);
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub location: Location,
    pub value: ValueKind,
}
