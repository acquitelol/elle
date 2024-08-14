use std::fmt;

use crate::compiler::enums::Type;
use crate::lexer::colors::*;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Use,
    Public,
    Function,
    Identifier,
    BoolLiteral,
    IntegerLiteral,
    LongLiteral,
    FloatingPoint,
    FloatLiteral,
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
    Define, // Used for both structs and enums
    ShiftRight,
    ShiftLeft,
    ShiftRightEqual,
    ShiftLeftEqual,
    Global,
    Local,
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
            | Self::ShiftRightEqual => true,
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
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", format!("{:?}", self).to_lowercase())
    }
}

#[derive(Debug, Clone)]
pub enum ValueKind {
    String(String),
    Number(i128),
    Character(char),
    Nil,
}

impl ValueKind {
    pub fn to_type_string(&self) -> Option<Type> {
        match self.clone() {
            ValueKind::String(val) => match val.as_str() {
                "string" => Some(Type::Pointer(Box::new(Type::Char))),
                "i8" => Some(Type::Byte),
                "u8" => Some(Type::UnsignedByte),
                "i16" => Some(Type::Halfword),
                "u16" => Some(Type::UnsignedHalfword),
                "i32" => Some(Type::Word),
                "u32" => Some(Type::UnsignedWord),
                "i64" => Some(Type::Long),
                "u64" => Some(Type::UnsignedLong),
                "f32" => Some(Type::Single),
                "f64" => Some(Type::Double),
                "char" => Some(Type::Word),
                "bool" => Some(Type::Boolean),
                // Arbitrary because it will be turned into `long` anyway when used as void*`
                "void" => Some(Type::Void),
                "nil" => None,
                other => Some(Type::Struct(other.into())),
            },
            _ => None,
        }
    }

    pub fn is_base_type(&self) -> bool {
        self.to_type_string().is_some()
            && match self.to_type_string().unwrap() {
                Type::Struct(_) => false,
                _ => true,
            }
    }

    pub fn get_string_inner(&self) -> Option<String> {
        match self.clone() {
            Self::String(val) => Some(val),
            _ => None,
        }
    }
}

#[derive(Clone)]
pub struct Location {
    pub file: String,
    pub row: usize,
    pub column: usize,
    pub ctx: String,
    pub above: Option<String>,
    pub length: usize,
}

impl Location {
    pub fn display(&self, is_warning: bool) -> String {
        return format!(
            "{BOLD}{UNDERLINE}{GREEN}{}{RESET}:{UNDERLINE}{fmt}{}{RESET}:{UNDERLINE}{YELLOW}{}{RESET}",
            self.file,
            self.row + 1,
            self.column + 1,
            fmt = if is_warning { YELLOW } else { RED }
        );
    }

    pub fn display_plain(&self) -> String {
        return format!("{}:{}:{}", self.file, self.row + 1, self.column + 1);
    }

    pub fn get_expr_lead(&self) -> String {
        let ident = self.column - (self.ctx.len() - self.ctx.trim_start().len());

        let left = if ident >= self.length {
            ident - self.length
        } else {
            ident
        };

        self.ctx.trim_start().split_at(left).1.into()
    }

    fn trim_indentation(&self, ctx: String, above: String) -> (String, String) {
        let lines: Vec<&str> = ctx.lines().chain(above.lines()).collect();

        let min_indent = lines
            .iter()
            .filter(|line| !line.trim().is_empty())
            .map(|line| line.chars().take_while(|c| c.is_whitespace()).count())
            .min()
            .unwrap_or(0);

        let trim_string = |input: String| {
            input
                .lines()
                .map(|line| {
                    if line.trim().is_empty() {
                        String::from(line)
                    } else {
                        String::from(&line[min_indent..])
                    }
                })
                .collect::<Vec<String>>()
                .join("\n")
        };

        let trimmed_ctx = trim_string(ctx);
        let trimmed_above = trim_string(above);

        (trimmed_ctx, trimmed_above)
    }

    pub fn display_pretty(&self, message: impl Into<String>, is_warning: bool) -> String {
        let (ctx, above) = if let Some(above) = self.above.clone() {
            self.trim_indentation(self.ctx.clone(), above)
        } else {
            (self.ctx.trim_start().to_string(), "".into())
        };

        let upper = format!(
            "{fmt}{}{RESET}[{}]{fmt}{}{RESET}",
            "―".repeat(20),
            self.display(is_warning),
            "―".repeat(20),
            fmt = if is_warning { YELLOW } else { RED }
        );

        // Used for calculating the bottom width
        let upper_plain = format!(
            "{}[{}]{}",
            "-".repeat(20),
            self.display_plain(),
            "-".repeat(20)
        );

        let padding = 2;
        let ident = self.column - (self.ctx.len() - ctx.len());

        let left = if ident >= self.length {
            ident - self.length
        } else {
            ident
        };

        let string = ctx.split_at(left);
        let fallback_char = format!("{}", string.1.chars().nth(0).unwrap());
        let mut fallback_rest = string.1.to_string();
        fallback_rest.remove(0);

        let lhs = string.0;
        let issue = string.1.get(0..self.length).unwrap_or(&fallback_char);
        let rhs = string.1.get(self.length..).unwrap_or(&fallback_rest);

        let line = format!("{} | ", self.row + 1);

        return format!(
            "\n{upper}\n{user_mesage}\n\n{above}{line_number}{}{lhs}{BOLD}{fmt}{UNDERLINE}{issue}{RESET}{rhs}\n{}{}{BOLD}{GREEN}^{}{RESET}\n{fmt}{}{RESET}\n",
            " ".repeat(padding),
            " ".repeat(padding + format!("{} | ", self.row + 1).len()),
            " ".repeat(left),
            "~".repeat(self.length.checked_sub(1).unwrap_or(0)),
            "―".repeat(upper_plain.len()),
            above = if !above.is_empty() {
                format!(
                    "{} | {}{}\n",
                    self.row,
                    " ".repeat(padding),
                    above
                )
            } else {
                "".into()
            },
            user_mesage = message.into(),
            line_number = line,
            fmt = if is_warning { YELLOW } else { RED }
        );
    }

    pub fn warning(&self, message: impl Into<String>) -> String {
        self.display_pretty(message, true)
    }

    pub fn error(&self, message: impl Into<String>) -> String {
        self.display_pretty(message, false)
    }

    pub fn default(file: String) -> Location {
        Location {
            file,
            row: 0,
            column: 0,
            ctx: "".into(),
            above: None,
            length: 0,
        }
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.row + 1, self.column + 1)
    }
}

impl fmt::Debug for Location {
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
