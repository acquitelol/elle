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

        if c.is_alphabetic() {
            let (kind, value) = self.consume_identifier();

            return Some(Token {
                kind,
                value,
                location: self.get_location(),
            });
        }

        if c.is_digit(10) {
            let (kind, value) = self.consume_integer_literal();

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
                    (TokenKind::Arrow, ValueKind::Nil)
                } else {
                    (TokenKind::Subtract, ValueKind::Nil)
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
                (TokenKind::Multiply, ValueKind::Nil)
            }
            '/' => {
                self.advance();

                match self.current_char() {
                    '/' => (
                        TokenKind::Comment,
                        ValueKind::String(self.consume_comment()),
                    ),
                    _ => (TokenKind::Divide, ValueKind::Nil),
                }
            }
            '+' => {
                self.advance();
                (TokenKind::Add, ValueKind::Nil)
            }
            '%' => {
                self.advance();
                (TokenKind::Modulus, ValueKind::Nil)
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
            '`' => (
                TokenKind::InterpolatedLiteral,
                ValueKind::String(self.consume_interpolated_literal()),
            ),
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
                    _ => (TokenKind::LessThan, ValueKind::Nil),
                }
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

        while !self.is_eof() && self.current_char().is_alphanumeric() {
            self.advance();
        }

        let identifier: String = self.input[start..self.position].iter().collect();

        let kind = match identifier.as_str() {
            "use" => TokenKind::Use,
            "pub" => TokenKind::Public,
            "op" => TokenKind::Operation,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "for" => TokenKind::For,
            "while" => TokenKind::While,
            "match" => TokenKind::Match,
            "ret" => TokenKind::Return,
            "let" => TokenKind::Declare,
            "true" => TokenKind::TrueLiteral,
            "false" => TokenKind::FalseLiteral,
            _ if identifier
                .chars()
                .next()
                .map(char::is_uppercase)
                .unwrap_or(false) =>
            {
                TokenKind::Type
            }
            _ => TokenKind::Identifier,
        };

        (kind, ValueKind::String(identifier))
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

    fn consume_integer_literal(&mut self) -> (TokenKind, ValueKind) {
        let start = self.position;

        while !self.is_eof() && self.current_char().is_digit(10) {
            self.advance();
        }

        let literal: String = self.input[start..self.position].iter().collect();
        (
            TokenKind::IntegerLiteral,
            ValueKind::Number(literal.parse().unwrap()),
        )
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

    fn consume_char_literal(&mut self) -> char {
        self.advance(); // First advance to get the character

        let character = self.current_char();

        self.advance(); // Advance again to ensure that the next character is the closing of the char expr

        if self.current_char() != '\'' {
            panic!("Using single quotes is for single characters only. Expected the end of a character literal, got '{}'", self.current_char());
        }

        self.advance(); // Advance once more to leave the char expr

        character
    }

    fn consume_interpolated_literal(&mut self) -> String {
        let mut string = String::new();
        self.advance();

        while !self.is_eof() && self.current_char() != '`' {
            string.push(self.current_char());
            self.advance();
        }

        self.advance();
        string
    }
}
