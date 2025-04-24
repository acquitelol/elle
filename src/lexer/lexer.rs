use std::{cell::RefCell, rc::Rc};

use crate::{
    elle_error, misc::constants::get_INTROSPECTION_LOCATION, INTROSPECTION_LOCATION,
    RESERVED_KEYWORDS,
};

use super::enums::{Location, ParseResult, Position, Token, TokenKind, ValueKind};

pub struct Lexer<'a> {
    file: String,
    input: &'a str,
    position: usize,
    row: usize,
    bol: usize,
    prev_token: Option<Token>,
    line_starts: Vec<usize>,
    has_tagged: bool, // whether a token was tagged for introspection yet
}

impl Lexer<'_> {
    pub fn new(file: String, input: &str, has_tagged: bool) -> Lexer {
        let mut line_starts = vec![0];
        let mut char_index = 0;

        for c in input.chars() {
            char_index += 1;
            if c == '\n' {
                line_starts.push(char_index);
            }
        }

        Lexer {
            file,
            input,
            position: 0,
            row: 0,
            bol: 0,
            prev_token: None,
            line_starts,
            has_tagged,
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        // For calculating length of token
        let token = self.internal_next_token();
        self.prev_token = token.clone();

        return token;
    }

    fn internal_next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();

        let start_row = self.row;
        let start_col = self.position - self.bol;

        if self.is_eof() {
            return None;
        }

        let c = self.current_char();

        if c.is_alphabetic() || c == '_' {
            let (kind, value) = self.consume_identifier(start_row, start_col);
            let location = self.get_location(start_row, start_col);
            let mut tagged =
                location.contains(&Position::from_tuple(get_INTROSPECTION_LOCATION!()));

            if tagged {
                if self.has_tagged {
                    tagged = false;
                } else {
                    self.has_tagged = true;
                }
            }

            return Some(Token {
                kind,
                value,
                location: Rc::new(RefCell::new(location)),
                tagged,
            });
        }

        if c.is_digit(10) {
            let (kind, value) = self.consume_number_literal(start_row, start_col);
            let location = self.get_location(start_row, start_col);
            let mut tagged =
                location.contains(&Position::from_tuple(get_INTROSPECTION_LOCATION!()));

            if tagged {
                if self.has_tagged {
                    tagged = false;
                } else {
                    self.has_tagged = true;
                }
            }

            return Some(Token {
                kind,
                value,
                location: Rc::new(RefCell::new(location)),
                tagged,
            });
        }

        let (kind, value) = match c {
            ':' => {
                self.advance();

                match self.current_char() {
                    ':' => {
                        self.advance();
                        (TokenKind::DoubleColon, ValueKind::Nil)
                    }
                    _ => (TokenKind::Colon, ValueKind::Nil),
                }
            }
            '@' => {
                self.advance();

                if self.current_char().is_alphabetic() {
                    (TokenKind::Attribute, ValueKind::Nil)
                } else {
                    (TokenKind::AtMark, ValueKind::Nil)
                }
            }
            '(' => {
                self.advance();
                (TokenKind::LeftParenthesis, ValueKind::Nil)
            }
            ')' => {
                self.advance();
                (TokenKind::RightParenthesis, ValueKind::Nil)
            }
            '{' => {
                self.advance();
                (TokenKind::LeftCurlyBrace, ValueKind::Nil)
            }
            '}' => {
                self.advance();
                (TokenKind::RightCurlyBrace, ValueKind::Nil)
            }
            '[' => {
                self.advance();
                (TokenKind::LeftBlockBrace, ValueKind::Nil)
            }
            ']' => {
                self.advance();
                (TokenKind::RightBlockBrace, ValueKind::Nil)
            }
            ',' => {
                self.advance();
                (TokenKind::Comma, ValueKind::Nil)
            }
            '~' => {
                self.advance();
                (TokenKind::BitwiseNot, ValueKind::Nil)
            }
            '!' => {
                self.advance();

                match self.current_char() {
                    '=' => {
                        self.advance();
                        (TokenKind::NotEqualTo, ValueKind::Nil)
                    }
                    _ => (TokenKind::Not, ValueKind::Nil),
                }
            }
            '=' => {
                self.advance();

                match self.current_char() {
                    '=' => {
                        self.advance();
                        (TokenKind::EqualTo, ValueKind::Nil)
                    }
                    _ => (TokenKind::Equal, ValueKind::Nil),
                }
            }
            '-' => {
                self.advance();

                if self.current_char() == '>' {
                    self.advance();
                    (TokenKind::RightArrow, ValueKind::Nil)
                } else if self.current_char() == '=' {
                    self.advance();
                    (TokenKind::SubtractEqual, ValueKind::Nil)
                } else if self.current_char() == '-' {
                    self.advance();

                    elle_error!(
                        self.get_location(start_row, start_col).error(
                            format!("Invalid token: Elle does not support '--' incrementing.\nPlease use '-= 1' for incrementing instead.")
                        )
                    )
                } else {
                    if self.is_unary_context() {
                        (TokenKind::Unary, ValueKind::Number(-1))
                    } else {
                        (TokenKind::Subtract, ValueKind::Nil)
                    }
                }
            }
            ';' => {
                self.advance();
                (TokenKind::Semicolon, ValueKind::Nil)
            }
            '?' => {
                self.advance();
                (TokenKind::Question, ValueKind::Nil)
            }
            '*' => {
                self.advance();

                if self.current_char() == '=' {
                    self.advance();
                    (TokenKind::MultiplyEqual, ValueKind::Nil)
                // } else if self.current_char() == '*' {
                //     self.advance();
                //     (TokenKind::Exponent, ValueKind::Nil)
                } else {
                    if self.is_unary_context() {
                        (TokenKind::Deref, ValueKind::Nil)
                    } else {
                        (TokenKind::Multiply, ValueKind::Nil)
                    }
                }
            }
            '^' => {
                self.advance();

                match self.current_char() {
                    '=' => {
                        self.advance();
                        (TokenKind::BitwiseXorEqual, ValueKind::Nil)
                    }
                    _ => (TokenKind::BitwiseXor, ValueKind::Nil),
                }
            }
            '/' => {
                self.advance();

                match self.current_char() {
                    '/' => (
                        TokenKind::Comment,
                        ValueKind::String(self.consume_comment()),
                    ),
                    '=' => {
                        self.advance();
                        (TokenKind::DivideEqual, ValueKind::Nil)
                    }
                    _ => (TokenKind::Divide, ValueKind::Nil),
                }
            }
            '+' => {
                self.advance();

                if self.current_char() == '=' {
                    self.advance();
                    (TokenKind::AddEqual, ValueKind::Nil)
                } else if self.current_char() == '+' {
                    self.advance();

                    elle_error!(
                        self.get_location(start_row, start_col).error(
                            format!("Invalid token: Elle does not support '++' incrementing.\nPlease use '+= 1' for incrementing instead.")
                        )
                    )
                } else {
                    if self.is_unary_context() {
                        (TokenKind::Unary, ValueKind::Number(1))
                    } else {
                        (TokenKind::Add, ValueKind::Nil)
                    }
                }
            }
            '%' => {
                self.advance();

                if self.current_char() == '=' {
                    self.advance();
                    (TokenKind::ModulusEqual, ValueKind::Nil)
                } else {
                    (TokenKind::Modulus, ValueKind::Nil)
                }
            }
            '&' => {
                self.advance();

                match self.current_char() {
                    '&' => {
                        self.advance();
                        (TokenKind::And, ValueKind::Nil)
                    }
                    '=' => {
                        self.advance();
                        (TokenKind::BitwiseAndEqual, ValueKind::Nil)
                    }
                    _ => {
                        if self.is_unary_context() {
                            (TokenKind::Address, ValueKind::Nil)
                        } else {
                            (TokenKind::BitwiseAnd, ValueKind::Nil)
                        }
                    }
                }
            }
            '|' => {
                self.advance();

                match self.current_char() {
                    '|' => {
                        self.advance();
                        (TokenKind::Or, ValueKind::Nil)
                    }
                    '=' => {
                        self.advance();
                        (TokenKind::BitwiseOrEqual, ValueKind::Nil)
                    }
                    _ => {
                        if self.is_unary_context() {
                            (TokenKind::None, ValueKind::Nil)
                        } else {
                            (TokenKind::BitwiseOr, ValueKind::Nil)
                        }
                    }
                }
            }
            '"' => (
                TokenKind::StringLiteral,
                ValueKind::String(self.consume_string_literal()),
            ),
            '\'' => (
                TokenKind::CharLiteral,
                ValueKind::Character(self.consume_char_literal(start_row, start_col)),
            ),
            '>' => {
                self.advance();

                match self.current_char() {
                    '>' => {
                        self.advance();

                        match self.current_char() {
                            '=' => {
                                self.advance();
                                (TokenKind::ShiftRightEqual, ValueKind::Nil)
                            }
                            _ => (TokenKind::ShiftRight, ValueKind::Nil),
                        }
                    }
                    '=' => {
                        self.advance();
                        (TokenKind::GreaterThanEqual, ValueKind::Nil)
                    }
                    _ => (TokenKind::GreaterThan, ValueKind::Nil),
                }
            }
            '<' => {
                self.advance();

                match self.current_char() {
                    '<' => {
                        self.advance();

                        match self.current_char() {
                            '=' => {
                                self.advance();
                                (TokenKind::ShiftLeftEqual, ValueKind::Nil)
                            }
                            _ => (TokenKind::ShiftLeft, ValueKind::Nil),
                        }
                    }
                    '=' => {
                        self.advance();
                        (TokenKind::LessThanEqual, ValueKind::Nil)
                    }
                    '-' => {
                        self.advance();
                        (TokenKind::LeftArrow, ValueKind::Nil)
                    }
                    '>' => {
                        self.advance();

                        match self.current_char() {
                            '=' => {
                                self.advance();
                                (TokenKind::ConcatEqual, ValueKind::Nil)
                            }
                            _ => (TokenKind::Concat, ValueKind::Nil),
                        }
                    }
                    _ => (TokenKind::LessThan, ValueKind::Nil),
                }
            }
            '$' => self.consume_identifier(start_row, start_col),
            '`' => (
                TokenKind::ExactLiteral,
                ValueKind::String(self.consume_exact_literal()),
            ),
            '.' => {
                self.advance();

                match self.current_char() {
                    '.' => {
                        self.advance();

                        match self.current_char() {
                            '.' => {
                                self.advance();
                                (TokenKind::Ellipsis, ValueKind::Nil)
                            }
                            '=' => {
                                self.advance();
                                (TokenKind::RangeEqual, ValueKind::Nil)
                            }
                            _ => (TokenKind::Range, ValueKind::Nil),
                        }
                    }
                    _ => (TokenKind::Dot, ValueKind::Nil),
                }
            }
            '#' => {
                self.advance();

                match self.current_char() {
                    '[' => (TokenKind::Hashtag, ValueKind::Nil),
                    _ => {
                        let (_, value) = self.consume_identifier(start_row, start_col);

                        match value {
                            ValueKind::String(val) => match val.as_str() {
                                "size" => (TokenKind::Size, ValueKind::Nil),
                                "len" => (TokenKind::ArrayLength, ValueKind::Nil),
                                "i" => (TokenKind::IndexOf, ValueKind::Nil),
                                "env" => (TokenKind::Environment, ValueKind::Nil),
                                "alloc" => (TokenKind::Alloc, ValueKind::Nil),
                                "realloc" => (TokenKind::Realloc, ValueKind::Nil),
                                "free" => (TokenKind::Free, ValueKind::Nil),
                                "set_allocator" => (TokenKind::SetAllocator, ValueKind::Nil),
                                "reset_allocator" => (TokenKind::ResetAllocator, ValueKind::Nil),
                                "cast" => (TokenKind::Cast, ValueKind::Nil),
                                other => elle_error!(self
                                    .get_location(start_row, start_col)
                                    .error(format!("Unimplemented directive: '{}'", other))),
                            },
                            _ => unreachable!(),
                        }
                    }
                }
            }
            _ => {
                self.advance();
                elle_error!(self
                    .get_location(start_row, start_col)
                    .error(format!("Unexpected character: '{}'", c)))
            }
        };

        if kind == TokenKind::None {
            return None;
        }

        let location = self.get_location(start_row, start_col);
        let mut tagged = location.contains(&Position::from_tuple(get_INTROSPECTION_LOCATION!()));

        if tagged {
            if self.has_tagged {
                tagged = false;
            } else {
                self.has_tagged = true;
            }
        }

        return Some(Token {
            kind,
            value,
            location: Rc::new(RefCell::new(location)),
            tagged,
        });
    }

    fn is_eof(&self) -> bool {
        self.position >= self.input.len()
    }

    fn current_char(&self) -> char {
        self.input[self.position..].chars().next().unwrap_or('\0')
    }

    fn next_char(&self) -> Option<char> {
        self.input[self.position..].chars().nth(1)
    }

    fn previous_char(&self) -> Option<char> {
        self.input[..self.position].chars().rev().next()
    }

    fn advance(&mut self) {
        if !self.is_eof() {
            let current = self.current_char();
            self.position += current.len_utf8();

            if current == '\n' {
                self.bol = self.position;
                self.row += 1;
            }
        }
    }

    fn skip_whitespace(&mut self) {
        while !self.is_eof() && self.current_char().is_whitespace() {
            self.advance();
        }
    }

    fn get_location(&mut self, start_row: usize, start_col: usize) -> Location {
        Location {
            file: Rc::from(self.file.clone()),
            alt_start: Rc::new(Position { row: 0, column: 0 }),
            alt_end: Rc::new(Position { row: 0, column: 1 }),
            start: Rc::new(Position {
                row: start_row,
                column: start_col,
            }),
            end: Rc::new(Position {
                row: self.row,
                column: self.position - self.bol,
            }),
            ctx: Rc::from(self.get_line(self.row).unwrap_or("".into())),
            above: if self.row == 0 {
                None
            } else {
                self.get_line(self.row - 1).map(Rc::from)
            },
            extra_info: Rc::from(""),
        }
    }

    fn get_line(&self, at: usize) -> Option<&str> {
        let start = *self.line_starts.get(at)?;
        let end = *self.line_starts.get(at + 1).unwrap_or(&self.input.len());

        let line = self.input.get(start..end)?;

        Some(
            line.strip_suffix("\r\n")
                .or_else(|| line.strip_suffix('\n'))
                .or_else(|| line.strip_suffix('\r'))
                .unwrap_or(line),
        )
    }

    fn is_unary_context(&self) -> bool {
        if let Some(ref prev_token) = self.prev_token {
            prev_token.kind.is_unary_context()
        } else {
            true
        }
    }

    fn consume_identifier(&mut self, start_row: usize, start_col: usize) -> (TokenKind, ValueKind) {
        let start = self.position;

        while !self.is_eof()
            && (self.current_char().is_alphanumeric()
                || self.current_char() == '_'
                || self.current_char() == '$')
        {
            self.advance();
        }

        let identifier = &self.input[start..self.position];

        if RESERVED_KEYWORDS.contains(&identifier) {
            elle_error!(
                self.get_location(start_row, start_col).error(format!(
                    "Use of the reserved keyword '{}' is disallowed.\nThis keyword is currently not in use, but it is reserved\nbecause it may be used in the language in the future.",
                    identifier
                ))
            )
        }

        let kind = match identifier {
            "use" => TokenKind::Use,
            "pub" => TokenKind::Public,
            "fn" => TokenKind::Function,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "for" => TokenKind::For,
            "while" => TokenKind::While,
            "const" => TokenKind::Constant,
            "return" => TokenKind::Return,
            "true" => TokenKind::TrueLiteral,
            "false" => TokenKind::FalseLiteral,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            // "to" => TokenKind::To,
            "yield" => TokenKind::Yield,
            // "step" => TokenKind::Step,
            "variadic" => TokenKind::Variadic,
            "defer" => TokenKind::Defer,
            "external" => TokenKind::External,
            "struct" => TokenKind::Struct,
            "global" => TokenKind::Global,
            "namespace" => TokenKind::Namespace,
            "in" => TokenKind::In,
            "let" => TokenKind::Let,
            "enum" => TokenKind::Enum,
            // WHEN ADDING A KEYWORD HERE DON'T FORGET TO
            // POTENTIALLY UPDATE THE RESERVED KEYWORD LIST
            _ => TokenKind::Identifier,
        };

        let val = ValueKind::String(identifier.into());
        (
            // if val.is_base_type() {
            //     TokenKind::Type
            // } else {
            //     kind
            // },
            kind, val,
        )
    }

    fn consume_comment(&mut self) -> String {
        let mut string = String::new();
        self.advance();

        while !self.is_eof() && self.current_char() != '\n' {
            string.push(self.current_char());
            self.advance();
        }

        self.advance();
        string
    }

    fn consume_number_literal(
        &mut self,
        start_row: usize,
        start_col: usize,
    ) -> (TokenKind, ValueKind) {
        let start = self.position;
        let mut float = false;
        let mut scientific = false;
        let mut radix = 10;

        while !self.is_eof()
            && (self.current_char().is_digit(radix)
                || self.current_char() == '.'
                || self.current_char() == '_')
            || vec!['x', 'o', 'b', 'e'].contains(&self.current_char())
        {
            if self.current_char() == '.' {
                if self.next_char().is_some_and(|c| !c.is_digit(radix)) {
                    break;
                }

                float = true;
            }

            // Don't want to set the radix again
            // after it was first set to something
            // via 0x 0o 0b etc
            if radix == 10 {
                match self.current_char() {
                    'x' => {
                        radix = 16;
                    }
                    'o' => {
                        radix = 8;
                    }
                    'b' => {
                        radix = 2;
                    }
                    'e' => {
                        scientific = true;
                    }
                    _ => {}
                }
            }

            self.advance();
        }

        let unparsed_literal: String = self.input[start..self.position].chars().collect();
        let mut literal = unparsed_literal.replace("_", "");

        if radix != 10 {
            if self.current_char().is_digit(10) {
                elle_error!(self.get_location(start_row, start_col).error(format!(
                    "Character '{}' is not a valid digit of radix {}.",
                    self.current_char(),
                    radix
                )));
            }

            if float {
                elle_error!(self.get_location(start_row, start_col).error(format!(
                    "Cannot have a floating point or scientific literal of a base other than 10."
                )))
            }

            literal = format!("{:?}", u128::from_str_radix(&literal[2..], radix).unwrap());
        }

        if scientific {
            let base_string = literal.split("e").next().unwrap_or_else(|| {
                elle_error!(self.get_location(start_row, start_col).error(format!(
                    "Failed to get the base string of {} for scientific literal",
                    literal
                )))
            });

            let base = if float {
                base_string
                    .parse::<f64>()
                    .map(ParseResult::Float)
                    .map_err(|e| e.to_string())
            } else {
                base_string
                    .parse::<i64>()
                    .map(ParseResult::Int)
                    .map_err(|e| e.to_string())
            }
            .unwrap_or_else(|err| {
                elle_error!(self.get_location(start_row, start_col).error(format!(
                    "Failed to parse the base {} of scientific literal into a number\n{err}",
                    base_string
                )))
            });

            let exponent_base = literal.split("e").skip(1).next().unwrap_or_else(|| {
                elle_error!(self.get_location(start_row, start_col).error(format!(
                    "Failed to get the base string of {} for scientific literal",
                    literal
                )))
            });

            let exponent = exponent_base.parse::<i64>().unwrap_or_else(|err| {
                elle_error!(self.get_location(start_row, start_col).error(format!(
                    "Failed to parse the exponent {} of scientific literal into an integer\n{err}",
                    exponent_base
                )))
            });

            match base {
                ParseResult::Float(val) => {
                    literal = (val * 10_f64.powf(exponent as f64)).to_string();

                    if !literal.contains('.') {
                        literal.push_str(".0");
                    }
                }
                ParseResult::Int(val) => {
                    literal = (val * 10_i64.pow(exponent as u32)).to_string();
                }
            };
        }

        if float {
            (
                TokenKind::FloatingPoint,
                ValueKind::String(format!("{}", literal)),
            )
        } else {
            (
                // 10 = INT_MAX digits
                if literal.len() > 10 {
                    TokenKind::LongLiteral
                } else {
                    TokenKind::IntegerLiteral
                },
                ValueKind::Number(literal.parse().unwrap()),
            )
        }
    }

    fn consume_string_literal(&mut self) -> String {
        let mut string = String::new();
        self.advance();

        while !self.is_eof() {
            if self.current_char() == '"'
                && self.previous_char().is_some()
                && self.previous_char().unwrap() != '\\'
            {
                break;
            }

            string.push(self.current_char());
            self.advance();
        }

        self.advance();
        string
    }

    fn consume_exact_literal(&mut self) -> String {
        let mut string = String::new();
        self.advance();

        loop {
            string.push(self.current_char());
            self.advance();

            if self.current_char() == '`' || self.is_eof() {
                break;
            }
        }

        self.advance();
        string
    }

    fn consume_char_literal(&mut self, start_row: usize, start_col: usize) -> char {
        self.advance(); // First advance to get the character

        let mut character = self.current_char();

        self.advance(); // Advance again to ensure that the next character is the closing of the char expr

        if character == '\\' {
            character = match self.current_char() {
                'a' => '\x07',
                'b' => '\x08',
                'f' => '\x0C',
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                'v' => '\x0B',
                '0' => '\0',
                '\'' => '\'',
                _ => elle_error!(self
                    .get_location(start_row, start_col)
                    .error(format!("Invalid escape sequence: '{}'", character))),
            };

            self.advance();
        }

        if self.current_char() != '\'' {
            elle_error!(self.get_location(start_row, start_col).error(format!(
                "Using single quotes is for single characters only.\nExpected the end of a character literal, got '{}'",
                self.current_char()
            )));
        }

        self.advance(); // Advance once more to leave the char expr

        character
    }
}
