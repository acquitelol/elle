use std::fmt;

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
    FloatingPoint,
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
    BitwiseXorEqual,
    BitwiseOrEqual,
    BitwiseAndEqual,
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
    BitwiseAnd,
    BitwiseXor,
    Or,
    BitwiseOr,
    None,
    Constant,
    Store,
    Break,
    Continue,
    To,
    Ellipsis,
    Variadic,
    Dot,
    Yield,
    Step,
    Deref,
    Defer,
    Size,
    Unary,
    ArrayLength,
    External,
    Address,
    ShiftRight,
    ShiftLeft,
    ShiftRightEqual,
    ShiftLeftEqual,
}

impl TokenKind {
    pub fn highest_precedence() -> i8 {
        // Self::Exponent.precedence()
        Self::Multiply.precedence()
    }

    pub fn precedence(&self) -> i8 {
        match self {
            // Self::Exponent => 9,
            Self::Multiply | Self::Divide | Self::Modulus => 8,
            Self::Add | Self::Subtract => 7,
            Self::ShiftLeft | Self::ShiftRight => 6,
            Self::LessThan | Self::LessThanEqual | Self::GreaterThan | Self::GreaterThanEqual => 5,
            Self::EqualTo | Self::NotEqualTo => 4,
            Self::And | Self::BitwiseAnd => 3,
            Self::BitwiseXor => 2,
            Self::Or | Self::BitwiseOr => 1,
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
            | Self::Or
            | Self::BitwiseXor
            | Self::BitwiseOr
            | Self::BitwiseAnd
            | Self::ShiftLeft
            | Self::ShiftRight => true,
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
            | Self::Continue
            | Self::FloatingPoint => true,
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
            | Self::BitwiseXorEqual
            | Self::BitwiseOrEqual
            | Self::BitwiseAndEqual
            | Self::ShiftLeftEqual
            | Self::ShiftRightEqual
            | Self::AddOne
            | Self::SubtractOne => true,
            _ => false,
        }
    }

    pub fn is_comparative(&self) -> bool {
        match self {
            Self::GreaterThan
            | Self::GreaterThanEqual
            | Self::LessThan
            | Self::LessThanEqual
            | Self::EqualTo
            | Self::NotEqualTo => true,
            _ => false,
        }
    }

    pub fn is_unary_context(&self) -> bool {
        match self {
            Self::LeftParenthesis
            | Self::LeftCurlyBrace
            | Self::LeftBlockBrace
            | Self::Comma
            | Self::Colon
            | Self::Not
            | Self::Semicolon
            | Self::Return
            | Self::While
            | Self::For
            | Self::If
            | Self::Equal => true,
            other if other.is_declarative() => true,
            other if other.is_arithmetic() => true,
            _ => false,
        }
    }

    pub fn to_non_declarative(&self) -> TokenKind {
        match self {
            Self::AddEqual => TokenKind::Add,
            Self::SubtractEqual => TokenKind::Subtract,
            Self::MultiplyEqual => TokenKind::Multiply,
            Self::DivideEqual => TokenKind::Divide,
            Self::ModulusEqual => TokenKind::Modulus,
            Self::BitwiseXorEqual => TokenKind::BitwiseXor,
            Self::BitwiseAndEqual => TokenKind::BitwiseAnd,
            Self::BitwiseOrEqual => TokenKind::BitwiseOr,
            Self::ShiftLeftEqual => TokenKind::ShiftLeft,
            Self::ShiftRightEqual => TokenKind::ShiftRight,
            other => panic!("Invalid identifier operation {:?}", other),
        }
    }

    pub fn is_one_operator(&self) -> bool {
        match self.to_owned() {
            Self::AddOne | Self::SubtractOne => true,
            _ => false,
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", format!("{:?}", self).to_lowercase())
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
                "string" => Some(Type::Pointer(Box::new(Type::Char))),
                "fun" => Some(Type::Byte), // Cannot be the same as fn
                "i8" => Some(Type::Byte),
                "i16" => Some(Type::Halfword),
                "i32" => Some(Type::Word),
                "i64" => Some(Type::Long),
                "f32" => Some(Type::Single),
                "f64" => Some(Type::Double),
                "char" => Some(Type::Char),
                "bool" => Some(Type::Byte),
                // Arbitrary because it will be turned into `long` anyway when used as void*`
                "void" => Some(Type::Void),
                "nil" => None,
                _ => None,
            },
            _ => None,
        }
    }

    pub fn is_base_type(&self) -> bool {
        self.to_type_string().is_some()
            && match self.to_type_string().unwrap() {
                Type::Aggregate(_) => false,
                _ => true,
            }
    }
}

#[derive(Debug, Clone)]
pub struct Location {
    pub file: String,
    pub row: usize,
    pub column: usize,
}

impl Location {
    pub fn display(&self) -> String {
        return format!("{}:{}:{}", self.file, self.row + 1, self.column + 1);
    }

    pub fn error(&self, message: String) -> String {
        let upper = format!("{}[{}]{}", "-".repeat(20), self.display(), "-".repeat(20));

        return format!(
            "\n\n{}\n{}\n{}\n\n",
            upper,
            message,
            "-".repeat(upper.len())
        );
    }

    pub fn default(file: String) -> Location {
        Location {
            file,
            row: 0,
            column: 0,
        }
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.row + 1, self.column + 1)
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub location: Location,
    pub value: ValueKind,
}

#[derive(Debug, Clone)]
pub enum ParseResult {
    Float(f64),
    Int(i64),
}
