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
}

impl TokenKind {
    fn precedence(&self) -> i32 {
        match self {
            TokenKind::Multiply | TokenKind::Divide | TokenKind::Modulus => 7,
            TokenKind::Add | TokenKind::Subtract => 6,
            TokenKind::LessThan
            | TokenKind::LessThanEqual
            | TokenKind::GreaterThan
            | TokenKind::GreaterThanEqual => 5,
            TokenKind::EqualTo | TokenKind::NotEqualTo => 4,
            TokenKind::And => 3,
            TokenKind::Or => 2,
            TokenKind::Equal => 1,
            _ => 0,
        }
    }
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
