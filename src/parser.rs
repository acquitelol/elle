use std::vec::Vec;

use crate::lexer::Token;

pub struct Parser {
    input: Vec<Token>,
    position: usize,
}

impl Parser {
    pub fn new(input: Vec<Token>) -> Self {
        Parser { input, position: 0 }
    }

    fn current_token(&self) -> Token {
        self.input[self.position].clone()
    }

    fn advance(&mut self) {
        self.position += 1;
    }

    fn match_token(&mut self, expected: Token) -> bool {
        if self.current_token() == expected.clone() {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect_token(&self, expected: Token) {
        if std::mem::discriminant(&self.current_token())
            != std::mem::discriminant(&expected.clone())
        {
            panic!(
                "\nExpected {:?}, found {:?}\n\nToken Stack:\n\n{:?}",
                expected,
                self.current_token(),
                self.input
            );
        } else {
            println!(
                "√ Expected {:?}, found {:?}",
                expected,
                self.current_token()
            );
        }
    }

    pub fn parse(&mut self) {
        while self.position != self.input.len() {
            if self.match_token(Token::Require) {
                self.parse_require();
            }
        }
    }

    fn parse_require(&mut self) {
        self.expect_token(Token::Identifier("Library name".to_owned()));
        self.advance();

        self.expect_token(Token::Colon);
        self.advance();

        self.expect_token(Token::Identifier("Module name".to_owned()));
        self.advance();

        if self.match_token(Token::AtMark) {
            self.expect_token(Token::LeftCurlyBrace);
            self.advance();

            while self.current_token() != Token::RightCurlyBrace {
                self.expect_token(Token::Identifier("".to_owned()));
                self.advance();
            }

            self.expect_token(Token::RightCurlyBrace);
            self.advance();
        }

        self.expect_token(Token::Semicolon);
        self.advance();
    }
}
