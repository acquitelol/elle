use super::enums::{Location, Token, TokenKind, ValueKind};

pub struct Lexer {
    file: String,
    input: Vec<char>,
    position: usize,
    row: usize,
    bol: usize,
}

impl Lexer {
    pub fn new(file: String, input: &str) -> Lexer {
        Lexer {
            file,
            input: input.chars().collect(),
            position: 0,
            row: 0,
            bol: 0,
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
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
                    match self.current_char().is_digit(10) {
                        true => {
                            let (kind, value) = self.consume_number_literal();

                            let result = match value {
                                ValueKind::Number(value) => ValueKind::Number(-value),
                                ValueKind::String(value) => {
                                    ValueKind::String(format!("-{}", value))
                                }
                                _ => todo!(),
                            };

                            return Some(Token {
                                kind,
                                value: result,
                                location: self.get_location(),
                            });
                        }
                        false => (TokenKind::Subtract, ValueKind::Nil),
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
                }
                /* else if self.current_char() == '*' {
                    self.advance();
                    (TokenKind::Exponent, ValueKind::Nil)
                } */
                else {
                    (TokenKind::Multiply, ValueKind::Nil)
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
                    match self.current_char().is_digit(10) {
                        true => {
                            let (kind, value) = self.consume_number_literal();

                            return Some(Token {
                                kind,
                                value,
                                location: self.get_location(),
                            });
                        }
                        false => (TokenKind::Add, ValueKind::Nil),
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
                    _ => (TokenKind::None, ValueKind::Nil),
                }
            }
            '|' => {
                self.advance();

                match self.current_char() {
                    '|' => {
                        self.advance();
                        (TokenKind::Or, ValueKind::Nil)
                    }
                    _ => (TokenKind::None, ValueKind::Nil),
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
            '#' => {
                let res = self.consume_exact_literal();
                (TokenKind::ExactLiteral, ValueKind::String(res))
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
                            _ => panic!("Invalid token: expected (.) or (...) but got (..)."),
                        }
                    }
                    _ => (TokenKind::Dot, ValueKind::Nil),
                }
            }
            '$' => {
                self.advance();
                (TokenKind::Size, ValueKind::Nil)
            }
            _ => panic!("Unexpected character: {:?}", c),
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
            "next" => TokenKind::Next,
            "yield" => TokenKind::Yield,
            "step" => TokenKind::Step,
            "variadic" => TokenKind::Variadic,
            "defer" => TokenKind::Defer,
            _ => TokenKind::Identifier,
        };

        let val = ValueKind::String(identifier);
        (if val.is_type() { TokenKind::Type } else { kind }, val)
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

        while !self.is_eof() && (self.current_char().is_digit(10) || self.current_char() == '.') {
            if self.current_char() == '.' {
                float = true;
            }

            self.advance();
        }

        let literal: String = self.input[start..self.position].iter().collect();
        if float {
            (
                TokenKind::FloatLiteral,
                ValueKind::String(format!("{}", literal)),
            )
        } else {
            (
                TokenKind::IntegerLiteral,
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

        while !self.is_eof() && self.current_char() != '#' {
            string.push(self.current_char());
            self.advance();
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
