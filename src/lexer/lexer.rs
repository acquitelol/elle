use std::f32::consts::PI;

use super::enums::{Location, ParseResult, Token, TokenKind, ValueKind};

pub struct Lexer {
    file: String,
    input: Vec<char>,
    position: usize,
    row: usize,
    bol: usize,
    prev_token: Option<Token>,
    position_no_whitespace: usize,
    prev_position_no_whitespace: usize,
}

impl Lexer {
    pub fn new(file: String, input: &str) -> Lexer {
        Lexer {
            file,
            input: input.chars().collect(),
            position: 0,
            row: 0,
            bol: 0,
            prev_token: None,
            position_no_whitespace: 0,
            prev_position_no_whitespace: 0,
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        // For calculating length of token
        self.prev_position_no_whitespace = self.position_no_whitespace;
        let token = self.internal_next_token();
        self.prev_token = token.clone();

        return token;
    }

    fn internal_next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();

        if self.is_eof() {
            return None;
        }

        let c = self.current_char();

        if c.is_alphabetic() || c == '_' {
            let (kind, value) = self.consume_identifier();

            return Some(Token {
                kind,
                value,
                location: self.get_location(),
            });
        }

        if c.is_digit(10) {
            let (kind, value) = self.consume_number_literal();

            return Some(Token {
                kind,
                value,
                location: self.get_location(),
            });
        }

        let (kind, value) = match c {
            ':' => {
                self.advance();
                (TokenKind::Colon, ValueKind::Nil)
            }
            '@' => {
                self.advance();
                (TokenKind::AtMark, ValueKind::Nil)
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
                    (TokenKind::SubtractOne, ValueKind::Nil)
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
                    (TokenKind::AddOne, ValueKind::Nil)
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
                ValueKind::Character(self.consume_char_literal()),
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
                    _ => (TokenKind::LessThan, ValueKind::Nil),
                }
            }
            '$' => {
                self.advance();

                match self.current_char() {
                    '$' => {
                        let res = self.consume_exact_literal();
                        (TokenKind::ExactLiteral, ValueKind::String(res))
                    }
                    other => {
                        let mut location = self.get_location();
                        location.column += 1;

                        panic!(
                            "{}",
                            location.error(
                                format!("Invalid token: expected '$$' for exact literal opening but got '{}'", other)
                            )
                        )
                    }
                }
            }
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
                            _ => {
                                panic!(
                                    "{}",
                                    self.get_location().error(
                                        "Invalid token: expected \".\" or \"...\" but got \"..\"."
                                            .to_string()
                                    )
                                )
                            }
                        }
                    }
                    _ => (TokenKind::Dot, ValueKind::Nil),
                }
            }
            '#' => {
                self.advance();

                let (_, value) = self.consume_identifier();

                match value {
                    ValueKind::String(val) => match val.as_str() {
                        "size" => (TokenKind::Size, ValueKind::Nil),
                        "arrlen" => (TokenKind::ArrayLength, ValueKind::Nil),
                        other => panic!(
                            "{}",
                            self.get_location()
                                .error(format!("Unimplemented directive: '{}'", other))
                        ),
                    },
                    _ => unreachable!(),
                }
            }
            _ => {
                self.advance();

                panic!(
                    "{}",
                    self.get_location()
                        .error(format!("Unexpected character: '{}'", c))
                )
            }
        };

        if kind == TokenKind::None {
            return None;
        }

        return Some(Token {
            kind,
            value,
            location: self.get_location(),
        });
    }

    fn is_eof(&self) -> bool {
        self.position >= self.input.len()
    }

    fn current_char(&self) -> char {
        self.input[self.position]
    }

    fn advance(&mut self) {
        if !self.is_eof() {
            let current = self.current_char();
            self.position += 1;

            if !current.is_whitespace() {
                self.position_no_whitespace += 1;
            }

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

    fn get_location(&mut self) -> Location {
        Location {
            file: self.file.clone(),
            row: self.row,
            column: self.position - self.bol,
            ctx: self.get_line(self.row).unwrap(),
            above: if self.row == 0 {
                None
            } else {
                self.get_line(self.row - 1)
            },
            length: self.position_no_whitespace - self.prev_position_no_whitespace,
        }
    }

    fn get_line(&self, at: usize) -> Option<String> {
        self.input
            .iter()
            .collect::<String>()
            .lines()
            .enumerate()
            .find(|l| l.0 == at)
            .map(|i| i.1.to_string())
    }

    fn is_unary_context(&self) -> bool {
        if let Some(ref prev_token) = self.prev_token {
            prev_token.kind.is_unary_context()
        } else {
            true
        }
    }

    fn consume_identifier(&mut self) -> (TokenKind, ValueKind) {
        let start = self.position;

        while !self.is_eof()
            && (self.current_char().is_alphanumeric() || self.current_char() == '_')
        {
            self.advance();
        }

        let identifier: String = self.input[start..self.position].iter().collect();

        let kind = match identifier.as_str() {
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
            "store" => TokenKind::Store,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "to" => TokenKind::To,
            "yield" => TokenKind::Yield,
            "step" => TokenKind::Step,
            "variadic" => TokenKind::Variadic,
            "defer" => TokenKind::Defer,
            "external" => TokenKind::External,
            "def" => TokenKind::Define,
            "global" => TokenKind::Global,
            "local" => TokenKind::Local,
            _ => TokenKind::Identifier,
        };

        let val = ValueKind::String(identifier);
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

    fn consume_number_literal(&mut self) -> (TokenKind, ValueKind) {
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

        let unparsed_literal: String = self.input[start..self.position].iter().collect();
        let mut literal = unparsed_literal.replace("_", "");

        if radix != 10 {
            if self.current_char().is_digit(10) {
                panic!(
                    "{}",
                    self.get_location().error(format!(
                        "Character '{}' is not a valid digit of radix {}.",
                        self.current_char(),
                        radix
                    ))
                );
            }

            if float {
                panic!(
                    "{}",
                    self.get_location().error(format!(
                        "Cannot have a floating point or scientific literal of a base other than 10."
                    ))
                )
            }

            literal = format!("{:?}", u128::from_str_radix(&literal[2..], radix).unwrap());
        }

        if scientific {
            let base_string =
                literal
                    .split("e")
                    .next()
                    .expect(&self.get_location().error(format!(
                        "Failed to get the base string of {} for scientific literal",
                        literal
                    )));

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
            .expect(&self.get_location().error(format!(
                "Failed to parse the base {} of scientific literal into a number",
                base_string
            )));

            let exponent_base =
                literal
                    .split("e")
                    .skip(1)
                    .next()
                    .expect(&self.get_location().error(format!(
                        "Failed to get the base string of {} for scientific literal",
                        literal
                    )));

            let exponent = exponent_base
                .parse::<i64>()
                .expect(&self.get_location().error(format!(
                    "Failed to parse the exponent {} of scientific literal into an integer",
                    exponent_base
                )));

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

        while !self.is_eof() && self.current_char() != '"' {
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

            if self.current_char() == '$' {
                self.advance();

                if self.current_char() == '$' {
                    break;
                } else {
                    string.push('$');
                }
            }

            if self.is_eof() {
                break;
            }
        }

        self.advance();
        string
    }

    fn consume_char_literal(&mut self) -> char {
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
                _ => panic!("Invalid escape sequence: '{}'", character),
            };

            self.advance();
        }

        if self.current_char() != '\'' {
            panic!("Using single quotes is for single characters only. Expected the end of a character literal, got '{}'", self.current_char());
        }

        self.advance(); // Advance once more to leave the char expr

        character
    }
}
