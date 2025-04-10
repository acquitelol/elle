use std::fmt;
use std::rc::Rc;

use crate::compiler::qbe::r#type::Type;
use crate::misc::colors::*;
use crate::{elle_error, ISSUE_URL};

#[derive(Debug, Eq, PartialEq, Clone)]
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
    DoubleColon,
    AtMark,
    LeftParenthesis,
    RightParenthesis,
    LeftCurlyBrace,
    RightCurlyBrace,
    LeftBlockBrace,
    RightBlockBrace,
    Comma,
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
    BitwiseAnd,
    BitwiseXor,
    BitwiseNot,
    BitwiseOr,
    Not,
    And,
    Or,
    Concat,
    ConcatEqual,
    None,
    Constant,
    Store,
    Break,
    Continue,
    // To,
    Ellipsis,
    Variadic,
    Dot,
    Yield,
    // Step,
    In,
    Deref,
    Defer,
    Size,
    Unary,
    ArrayLength,
    External,
    Address,
    Struct,
    ShiftRight,
    ShiftLeft,
    ShiftRightEqual,
    ShiftLeftEqual,
    Global,
    Attribute,
    Namespace,
    Hashtag,
    IndexOf,
    Let,
    Range,
    RangeEqual,
    Environment,
    Alloc,
    Realloc,
    Free,
    SetAllocator,
    ResetAllocator,
}

impl TokenKind {
    pub fn highest_precedence() -> i8 {
        // Self::Exponent.precedence()
        Self::Multiply.precedence()
    }

    pub fn precedence(&self) -> i8 {
        match self {
            // Self::Exponent => 11,
            Self::Multiply | Self::Divide | Self::Modulus => 11,
            Self::Add | Self::Concat | Self::Subtract => 10,
            Self::Range | Self::RangeEqual => 9,
            Self::ShiftLeft | Self::ShiftRight => 8,
            Self::LessThan | Self::LessThanEqual | Self::GreaterThan | Self::GreaterThanEqual => 7,
            Self::EqualTo | Self::NotEqualTo => 6,
            Self::BitwiseAnd => 5,
            Self::BitwiseXor => 4,
            Self::BitwiseOr => 3,
            Self::And => 2,
            Self::Or => 1,
            _ => 0,
        }
    }

    pub fn is_ternary_start(&self) -> bool {
        self == &TokenKind::Question
    }

    pub fn is_ternary_end(&self) -> bool {
        self == &TokenKind::Colon
    }

    pub fn is_arithmetic(&self) -> bool {
        match self.to_owned() {
            Self::Multiply
            // | Self::Exponent
            | Self::Divide
            | Self::Modulus
            | Self::Add
            | Self::Concat
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
            | Self::ShiftRight
            | Self::Range
            | Self::RangeEqual => true,
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
            | Self::ConcatEqual
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

    pub fn is_brace(&self) -> bool {
        match self {
            Self::LeftParenthesis
            | Self::RightParenthesis
            | Self::LeftBlockBrace
            | Self::RightBlockBrace
            | Self::LeftCurlyBrace
            | Self::RightCurlyBrace => true,
            _ => false,
        }
    }

    pub fn is_unary_context(&self) -> bool {
        match self {
            Self::LeftParenthesis
            | Self::LeftCurlyBrace
            | Self::RightCurlyBrace
            | Self::LeftBlockBrace
            | Self::Comma
            | Self::Colon
            | Self::Not
            | Self::Semicolon
            | Self::Return
            | Self::While
            | Self::For
            | Self::If
            | Self::Equal
            | Self::Question
            | Self::In => true,
            other if other.is_declarative() => true,
            other if other.is_arithmetic() => true,
            _ => false,
        }
    }

    pub fn to_non_declarative(&self) -> TokenKind {
        match self {
            Self::AddEqual => TokenKind::Add,
            Self::ConcatEqual => TokenKind::Concat,
            Self::SubtractEqual => TokenKind::Subtract,
            Self::MultiplyEqual => TokenKind::Multiply,
            Self::DivideEqual => TokenKind::Divide,
            Self::ModulusEqual => TokenKind::Modulus,
            Self::BitwiseXorEqual => TokenKind::BitwiseXor,
            Self::BitwiseAndEqual => TokenKind::BitwiseAnd,
            Self::BitwiseOrEqual => TokenKind::BitwiseOr,
            Self::ShiftLeftEqual => TokenKind::ShiftLeft,
            Self::ShiftRightEqual => TokenKind::ShiftRight,
            other => {
                elle_error!(Location::base()
                    .internal_error(format!("Invalid identifier operation {other:?}")))
            }
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", format!("{:?}", self).to_lowercase())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ValueKind {
    String(String),
    Number(i128),
    Character(char),
    Nil,
}

impl ValueKind {
    pub fn to_type_string(&self, is_struct: bool) -> Option<Type> {
        match self.clone() {
            ValueKind::String(val) => match val.as_str() {
                "string" => Some(Type::Pointer(Box::new(Type::Char))),
                "any" => Some(Type::Pointer(Box::new(Type::Void))),
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
                "char" => Some(Type::Char),
                "bool" => Some(Type::Boolean),
                // Arbitrary because it will be turned into `long` anyway when used as void*`
                "void" => Some(Type::Void),
                other => Some(if is_struct {
                    Type::Struct(other.into())
                } else {
                    Type::Unknown(other.into())
                }),
            },
            _ => None,
        }
    }

    pub fn similar_mapping(ty: String) -> Option<String> {
        match ty.as_str() {
            "short" => Some("i16".into()),
            "int" => Some("i32".into()),
            "long" => Some("i64".into()),
            "float" => Some("f32".into()),
            "double" => Some("f64".into()),
            _ => None,
        }
    }

    pub fn is_base_type(&self) -> bool {
        self.to_type_string(false).is_some()
            && match self.to_type_string(false).unwrap() {
                Type::Unknown(_) | Type::Struct(_) => false,
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

impl fmt::Display for ValueKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::String(val) => write!(f, "{}", val),
            Self::Number(val) => write!(f, "{}", val),
            Self::Character(val) => write!(f, "{}", val),
            Self::Nil => write!(f, ""),
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Location {
    pub file: Rc<str>,
    pub row: usize,
    pub column: usize,
    pub length: usize,
    pub ctx: Rc<str>,
    pub above: Option<Rc<str>>,
    pub extra_info: Rc<str>,
}

impl Location {
    pub fn with_extra_info(&self, extra_info: impl Into<String>) -> Self {
        let mut owned = (*self).clone();
        owned.extra_info = Rc::from(extra_info.into());
        owned
    }

    pub fn display(&self, is_warning: bool) -> String {
        return format!(
            "{BOLD}{UNDERLINE}{GREEN}{}{RESET}:{UNDERLINE}{fmt}{}{RESET}:{UNDERLINE}{YELLOW}{}{RESET}",
            self.file,
            self.row + 1,
            self.column + 1,
            GREEN = get_GREEN!(),
            RESET = get_RESET!(),
            BOLD = get_BOLD!(),
            UNDERLINE = get_UNDERLINE!(),
            YELLOW = get_YELLOW!(),
            fmt = if is_warning { get_YELLOW!() } else { get_RED!() }
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

        let ctx = self.ctx.trim_start();
        let mut split_index = 0;

        for (i, c) in ctx.char_indices().take(left) {
            split_index = i + c.len_utf8();
        }

        ctx.split_at(split_index).1.into()
    }

    fn trim_indentation(&self, ctx: Rc<str>, above: Rc<str>) -> (String, String) {
        let lines: Vec<&str> = ctx.lines().chain(above.lines()).collect();

        let min_indent = lines
            .iter()
            .filter(|line| !line.trim().is_empty())
            .map(|line| line.chars().take_while(|c| c.is_whitespace()).count())
            .min()
            .unwrap_or(0);

        let trim_string = |input: Rc<str>| {
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
            RESET = get_RESET!(),
            fmt = if is_warning {
                get_YELLOW!()
            } else {
                get_RED!()
            }
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

        let split_index = ctx
            .char_indices()
            .nth(left)
            .map(|(i, _)| i)
            .unwrap_or_else(|| ctx.len());

        let (lhs, rhs) = ctx.split_at(split_index);

        let split_index = rhs
            .char_indices()
            .nth(self.length)
            .map(|(i, _)| i)
            .unwrap_or_else(|| rhs.len());

        let issue = &rhs[..split_index];
        let rhs = &rhs[split_index..];
        let line = format!("{} | ", self.row + 1);

        return format!(
            "\n{upper}\n{user_message}\n\n{above}{line_number}{}{lhs}{BOLD}{fmt}{UNDERLINE}{issue}{RESET}{rhs}\n{}{}{BOLD}{GREEN}^{}{}{RESET}\n{fmt}{}{RESET}\n",
            " ".repeat(padding),
            " ".repeat(padding + format!("{} | ", self.row + 1).len()),
            " ".repeat(left),
            "~".repeat(self.length.checked_sub(1).unwrap_or(0)),
            if !self.extra_info.is_empty() { format!(" {}", self.extra_info) } else { "".into() },
            "―".repeat(upper_plain.len()),
            above = if !above.is_empty() {
                format!(
                    "{:<2} | {}{}\n",
                    self.row,
                    " ".repeat(padding),
                    above
                )
            } else {
                "".into()
            },
            user_message = message.into(),
            line_number = line,
            BOLD = get_BOLD!(),
            UNDERLINE = get_UNDERLINE!(),
            GREEN = get_GREEN!(),
            RESET = get_RESET!(),
            fmt = if is_warning { get_YELLOW!() } else { get_RED!() }
        );
    }

    pub fn display_pretty_no_lines(&self, message: impl Into<String>, is_warning: bool) -> String {
        let lines = format!(
            "{fmt}{}{RESET}",
            "―".repeat(50),
            RESET = get_RESET!(),
            fmt = if is_warning {
                get_YELLOW!()
            } else {
                get_RED!()
            }
        );

        return format!("\n{lines}\n{}\n{lines}\n", message.into());
    }

    pub fn warning(&self, message: impl Into<String>) -> String {
        self.display_pretty(message, true)
    }

    pub fn error(&self, message: impl Into<String>) -> String {
        self.display_pretty(message, false)
    }

    pub fn basic_error(&self, message: impl Into<String>) -> String {
        self.display_pretty_no_lines(message, false)
    }

    pub fn internal_error(&self, message: impl Into<String>) -> String {
        self.display_pretty_no_lines(format!(
            "An internal error occured during compilation:\n{}\n\nPlease report this so that it can be fixed:\n{MAGENTA}{ISSUE_URL}{RESET}",
            message.into(),
            MAGENTA = get_MAGENTA!(),
            RESET = get_RESET!()
        ), false)
    }

    pub fn default(file: String) -> Location {
        Location {
            file: Rc::from(file),
            row: 0,
            column: 0,
            ctx: Rc::from("_"),
            above: None,
            length: 1,
            extra_info: Rc::from(""),
        }
    }

    pub fn base() -> Location {
        Location {
            file: Rc::from("_"),
            row: 0,
            column: 0,
            ctx: Rc::from("_"),
            above: None,
            length: 1,
            extra_info: Rc::from(""),
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
    pub value: ValueKind,
    pub location: Rc<Location>,
}

#[derive(Debug, Clone)]
pub enum Attribute {
    // Allows an external function marked by this to be renamed to another symbol
    Alias,
    // Ensures a function marked by this is not cleaned up if it is never used
    Volatile,
    // Ensures no formatter is set by default on a struct marked by this
    NoFormat,
    // Automatically runs the formatter on every parameter of a function marked by this
    Format,
}

impl Token {
    /// Ensures an attribute is valid and returns its enum variant
    pub fn parse_attribute(&self) -> Attribute {
        if self.kind != TokenKind::Identifier {
            elle_error!(self
                .location
                .error("Tried to parse an attribute on a non-identifier token"));
        }

        let attribute = self.value.get_string_inner().unwrap();

        match attribute.as_str() {
            "alias" => Attribute::Alias,
            "volatile" => Attribute::Volatile,
            "nofmt" => Attribute::NoFormat,
            "fmt" => Attribute::Format,
            _ => todo!("more attributes: {attribute}"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParseResult {
    Float(f64),
    Int(i64),
}
