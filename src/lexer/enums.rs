use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::compiler::qbe::r#type::Type;
use crate::misc::colors::*;
use crate::misc::constants::{get_RAW_ERRORS, RAW_ERRORS};
use crate::{elle_error, ISSUE_URL};

pub type MutRc<T> = Rc<RefCell<T>>;

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
    Cast,
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
    pub fn to_type_string(
        &self,
        is_struct: bool,
        is_enum: bool,
        inner: Option<Type>,
    ) -> Option<Type> {
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
                // Treat fn as void* so it becomes printable in structs
                "fn" => Some(Type::Void),
                other => Some(if is_struct {
                    Type::Struct(other.into())
                } else if is_enum {
                    Type::Enum(other.into(), Box::new(inner))
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
        self.to_type_string(false, false, None).is_some()
            && match self.to_type_string(false, false, None).unwrap() {
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Position {
    pub row: usize,
    pub column: usize,
}

impl Position {
    pub fn from_tuple(x: (usize, usize)) -> Self {
        Position {
            row: x.0,
            column: x.1,
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Location {
    pub file: Rc<str>,
    // so we can report the import location instead
    // of the real location for errors that weren't
    // in the current file
    pub alt_start: Rc<Position>,
    pub alt_end: Rc<Position>,
    pub start: Rc<Position>,
    pub end: Rc<Position>,
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

    pub fn contains(&self, pos: &Position) -> bool {
        *self.start <= *pos && *pos <= *self.end
    }

    pub fn display(&self, is_warning: bool) -> String {
        return format!(
            "{BOLD}{UNDERLINE}{GREEN}{}{RESET}:{UNDERLINE}{fmt}{}{RESET}:{UNDERLINE}{YELLOW}{}{RESET}",
            self.file,
            self.start.row + 1,
            self.start.column + 1,
            GREEN = get_GREEN!(),
            RESET = get_RESET!(),
            BOLD = get_BOLD!(),
            UNDERLINE = get_UNDERLINE!(),
            YELLOW = get_YELLOW!(),
            fmt = if is_warning { get_YELLOW!() } else { get_RED!() }
        );
    }

    pub fn display_plain(&self, end: bool) -> String {
        return format!(
            "{}:{}:{}",
            self.file,
            (if end { self.end.row } else { self.start.row }) + 1,
            (if end {
                self.end.column
            } else {
                self.start.column
            }) + 1
        );
    }

    pub fn display_alt(&self, end: bool) -> String {
        return format!(
            "{}:{}:{}",
            self.file,
            (if end {
                self.alt_end.row
            } else {
                self.alt_start.row
            }) + 1,
            (if end {
                self.alt_end.column
            } else {
                self.alt_start.column
            }) + 1
        );
    }

    /// This function can be slightly confusing in its usecase.
    /// I'll write some basic documentation here because I keep
    /// refactoring and then forgetting how this function works
    /// and it ends up breaking ElleMeta exprs formatting
    ///
    /// Its primary purpose is to get a new string based on
    /// `self.ctx` starting at `self.start.column`, where:
    ///
    /// For an expression such as `a(b($dbg(foo)))` if the parameter
    /// to search is `foo` inside of $dbg(), the returned expression
    /// is `foo)))`.
    ///
    /// This can then be properly parsed later.
    pub fn get_expr_lead(&self) -> String {
        let trimmed = self.ctx.trim_start();
        let trimmed_chars = self.ctx.chars().count() - trimmed.chars().count();
        let start_col = self.start.column.saturating_sub(trimmed_chars);

        let mut byte_offset = 0;
        for (i, c) in trimmed.char_indices().take(start_col) {
            byte_offset = i + c.len_utf8();
        }

        // return everything from `byte_offset` to the end
        trimmed.get(byte_offset..).unwrap_or("").to_string()
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
            self.display_plain(false),
            "-".repeat(20)
        );

        let padding = 2;
        let ident = self.start.column.saturating_sub(self.ctx.len() - ctx.len());

        let left = if ident >= self.end.column.saturating_sub(self.start.column) {
            ident - self.end.column.saturating_sub(self.start.column)
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
            .nth(self.end.column.saturating_sub(self.start.column))
            .map(|(i, _)| i)
            .unwrap_or_else(|| rhs.len());

        let issue = &rhs[..split_index];
        let rhs = &rhs[split_index..];
        let line = format!("{} | ", self.start.row + 1);

        return format!(
            "\n{upper}\n{user_message}\n\n{above}{line_number}{}{lhs}{BOLD}{fmt}{UNDERLINE}{issue}{RESET}{rhs}\n{}{}{BOLD}{GREEN}^{}{}{RESET}\n{fmt}{}{RESET}\n",
            " ".repeat(padding),
            " ".repeat(padding + format!("{} | ", self.start.row + 1).len()),
            " ".repeat(left),
            "~".repeat(self.end.column.saturating_sub(self.start.column).checked_sub(1).unwrap_or(0)),
            if !self.extra_info.is_empty() { format!(" {}", self.extra_info) } else { "".into() },
            "―".repeat(upper_plain.len()),
            above = if !above.is_empty() {
                format!(
                    "{:<2} | {}{}\n",
                    self.start.row,
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
        if get_RAW_ERRORS!() {
            return format!(
                "warning\n{}\n{}\n{}\n{}\n{}\n",
                self.display_plain(false),
                self.display_plain(true),
                self.display_alt(false),
                self.display_alt(true),
                message.into()
            );
        }

        self.display_pretty(message, true)
    }

    pub fn basic_warning(&self, message: impl Into<String>) -> String {
        if get_RAW_ERRORS!() {
            return format!(
                "warning\n{}\n{}\n{}\n{}\n{}\n",
                self.display_plain(false),
                self.display_plain(true),
                self.display_alt(false),
                self.display_alt(true),
                message.into()
            );
        }

        self.display_pretty_no_lines(message, true)
    }

    pub fn error(&self, message: impl Into<String>) -> String {
        if get_RAW_ERRORS!() {
            return format!(
                "error\n{}\n{}\n{}\n{}\n{}\n",
                self.display_plain(false),
                self.display_plain(true),
                self.display_alt(false),
                self.display_alt(true),
                message.into()
            );
        }

        self.display_pretty(message, false)
    }

    pub fn basic_error(&self, message: impl Into<String>) -> String {
        if get_RAW_ERRORS!() {
            return format!(
                "error\n{}\n{}\n{}\n{}\n{}\n",
                self.display_plain(false),
                self.display_plain(true),
                self.display_alt(false),
                self.display_alt(true),
                message.into()
            );
        }

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
            alt_start: Rc::new(Position { row: 0, column: 0 }),
            alt_end: Rc::new(Position { row: 0, column: 1 }),
            start: Rc::new(Position { row: 0, column: 0 }),
            end: Rc::new(Position { row: 0, column: 1 }),
            ctx: Rc::from("_"),
            above: None,
            extra_info: Rc::from(""),
        }
    }

    pub fn base() -> Location {
        Location {
            file: Rc::from("_"),
            alt_start: Rc::new(Position { row: 0, column: 0 }),
            alt_end: Rc::new(Position { row: 0, column: 1 }),
            start: Rc::new(Position { row: 0, column: 0 }),
            end: Rc::new(Position { row: 0, column: 1 }),
            ctx: Rc::from("_"),
            above: None,
            extra_info: Rc::from(""),
        }
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}:{}:{}",
            self.file,
            self.start.row + 1,
            self.start.column + 1
        )
    }
}

impl fmt::Debug for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}:{}:{}",
            self.file,
            self.start.row + 1,
            self.start.column + 1
        )
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub value: ValueKind,
    pub location: MutRc<Location>,
    pub tagged: bool,
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
                .borrow()
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

    pub fn from_ident(ident: &str) -> Self {
        return Token {
            kind: TokenKind::Identifier,
            value: ValueKind::String(ident.into()),
            location: Rc::new(RefCell::new(Location::base())),
            tagged: false,
        };
    }
}

#[derive(Debug, Clone)]
pub enum ParseResult {
    Float(f64),
    Int(i64),
}
