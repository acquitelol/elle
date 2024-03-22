#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Require,
    Expose,
    Operation,
    Type(String),
    Identifier(String),
    IntegerLiteral(i32),
    CharLiteral(char),
    StringLiteral(String),
    InterpolatedLiteral(String),
    Comment(String),
    Colon,
    AtMark,
    LeftParentheis,
    RightParenthesis,
    LeftCurlyBrace,
    RightCurlyBrace,
    LeftBlockBrace,
    RightBlockBrace,
    Comma,
    Equal,
    Arrow,
    Semicolon,
    If,
    Else,
    For,
    While,
    Match,
    Ret,
    Question,
    Multiply,
    Divide,
    Add,
    Subtract,
}

pub struct Lexer {
    input: Vec<char>,
    position: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Lexer {
        Lexer {
            input: input.chars().collect(),
            position: 0,
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();

        if self.is_eof() {
            return None;
        }

        let c = self.current_char();

        if c.is_alphabetic() {
            return Some(self.consume_identifier());
        }

        if c.is_digit(10) {
            return Some(self.consume_integer_literal());
        }

        match c {
            ':' => {
                self.advance();
                Some(Token::Colon)
            }
            '@' => {
                self.advance();
                Some(Token::AtMark)
            }
            '(' => {
                self.advance();
                Some(Token::LeftParentheis)
            }
            ')' => {
                self.advance();
                Some(Token::RightParenthesis)
            }
            '{' => {
                self.advance();
                Some(Token::LeftCurlyBrace)
            }
            '}' => {
                self.advance();
                Some(Token::RightCurlyBrace)
            }
            '[' => {
                self.advance();
                Some(Token::LeftBlockBrace)
            }
            ']' => {
                self.advance();
                Some(Token::RightBlockBrace)
            }
            ',' => {
                self.advance();
                Some(Token::Comma)
            }
            '=' => {
                self.advance();
                Some(Token::Equal)
            }
            '-' => {
                self.advance();

                if self.current_char() == '>' {
                    self.advance();
                    Some(Token::Arrow)
                } else {
                    Some(Token::Subtract)
                }
            }
            ';' => {
                self.advance();
                Some(Token::Semicolon)
            }
            '?' => {
                self.advance();
                Some(Token::Question)
            }
            '*' => {
                self.advance();
                Some(Token::Multiply)
            }
            '/' => {
                self.advance();

                match self.current_char() {
                    '/' => Some(Token::Comment(self.consume_comment())),
                    _ => Some(Token::Divide),
                }
            }
            '+' => {
                self.advance();
                Some(Token::Add)
            }
            '`' => Some(Token::InterpolatedLiteral(
                self.consume_interpolated_literal(),
            )),
            '"' => Some(Token::StringLiteral(self.consume_string_literal())),
            '\'' => Some(Token::CharLiteral(self.consume_char_literal())),
            _ => panic!("Unexpected character: {:?}", c),
        }
    }

    fn is_eof(&self) -> bool {
        self.position >= self.input.len()
    }

    fn current_char(&self) -> char {
        self.input[self.position]
    }

    fn advance(&mut self) {
        self.position += 1;
    }

    fn skip_whitespace(&mut self) {
        while !self.is_eof() && self.current_char().is_whitespace() {
            self.advance();
        }
    }

    fn consume_identifier(&mut self) -> Token {
        let start = self.position;

        while !self.is_eof() && self.current_char().is_alphanumeric() {
            self.advance();
        }

        let identifier: String = self.input[start..self.position].iter().collect();

        match identifier.as_str() {
            "require" => Token::Require,
            "expose" => Token::Expose,
            "op" => Token::Operation,
            "if" => Token::If,
            "else" => Token::Else,
            "for" => Token::For,
            "while" => Token::While,
            "match" => Token::Match,
            "ret" => Token::Ret,
            _ if identifier
                .chars()
                .next()
                .map(char::is_uppercase)
                .unwrap_or(false) =>
            {
                Token::Type(identifier)
            }
            _ => Token::Identifier(identifier),
        }
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

    fn consume_integer_literal(&mut self) -> Token {
        let start = self.position;

        while !self.is_eof() && self.current_char().is_digit(10) {
            self.advance();
        }

        let literal: String = self.input[start..self.position].iter().collect();
        Token::IntegerLiteral(literal.parse().unwrap())
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
