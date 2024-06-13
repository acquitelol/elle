use crate::compiler::enums::Type;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Use,
    Public,
    Function,
    Type,
    Identifier,
    IntegerLiteral,
    LongLiteral,
    FloatLiteral,
    DoubleLiteral,
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
    AddEqual,
    SubtractEqual,
    MultiplyEqual,
    DivideEqual,
    ModulusEqual,
    AddOne,
    SubtractOne,
    // Exponent,
    RightArrow,
    LeftArrow,
    Semicolon,
    If,
    Else,
    For,
    While,
    Return,
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
    Store,
    Break,
    Continue,
    To,
    Ellipsis,
    Variadic,
    Next,
    Dot,
    Yield,
    Step,
    Defer,
}

impl TokenKind {
    pub fn highest_precedence() -> i8 {
        // Self::Exponent.precedence()
        Self::Multiply.precedence()
    }

    pub fn precedence(&self) -> i8 {
        match self {
            // Self::Exponent => 7,
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
            // | Self::Exponent
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
            | Self::FloatLiteral
            | Self::LongLiteral
            | Self::DoubleLiteral
            | Self::ExactLiteral
            | Self::TrueLiteral
            | Self::FalseLiteral
            | Self::Break
            | Self::Continue => true,
            _ => false,
        }
    }

    pub fn is_declarative(&self) -> bool {
        match self.to_owned() {
            Self::AddEqual
            | Self::SubtractEqual
            | Self::MultiplyEqual
            | Self::DivideEqual
            | Self::ModulusEqual
            | Self::AddOne
            | Self::SubtractOne => true,
            _ => false,
        }
    }

    pub fn is_one_operator(&self) -> bool {
        match self.to_owned() {
            Self::AddOne | Self::SubtractOne => true,
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

impl ValueKind {
    pub fn to_type_string(&self) -> Option<Type> {
        match self.clone() {
            ValueKind::String(val) => match val.as_str() {
                "string" => Some(Type::Pointer(Box::new(Type::Byte))),
                "function" => Some(Type::Byte),
                "int" => Some(Type::Word),
                "long" => Some(Type::Long),
                "single" => Some(Type::Single),
                "float" => Some(Type::Single),
                "double" => Some(Type::Double),
                "char" => Some(Type::Byte),
                "nil" => None,
                _ => None,
            },
            _ => None,
        }
    }

    pub fn is_type(&self) -> bool {
        self.to_type_string().is_some()
    }
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
