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
    InterpolatedLiteral,
    TrueLiteral,
    FalseLiteral,
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
    ArithmeticOperation,
}

#[derive(Debug, Clone)]
pub enum ValueKind {
    String(String),
    Number(i32),
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
