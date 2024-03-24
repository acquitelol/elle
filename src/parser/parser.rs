use crate::{
    lexer::enums::{Token, TokenKind, ValueKind},
    parser::operation::Operation,
};

use super::{enums::Primitive, r#use::Use};

pub struct Parser {
    pub tokens: Vec<Token>,
    pub position: usize,
    tree: Vec<Primitive>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            position: 0,
            tree: vec![],
        }
    }

    pub fn current_token(&self) -> Token {
        self.tokens[self.position].clone()
    }

    pub fn advance(&mut self) {
        if self.is_eof() {
            println!("The position of {:?} is the last index of the token stack. Staying at the same position.", self.position);
        } else {
            self.position += 1;
        }
    }

    pub fn is_eof(&mut self) -> bool {
        self.position >= self.tokens.len() - 1
    }

    pub fn match_token(&mut self, expected: TokenKind, advance: bool) -> bool {
        if self.current_token().kind == expected {
            match advance {
                true => self.advance(),
                _ => {}
            };

            true
        } else {
            false
        }
    }

    pub fn expect_token(&self, expected: TokenKind) {
        if self.current_token().kind != expected {
            panic!(
                "[{}] Expected {:?}, found {:?}",
                self.current_token().location.display(),
                expected,
                self.current_token().kind
            );
        }
    }

    pub fn get(&mut self, expected: TokenKind) -> String {
        self.expect_token(expected.clone());

        let identifier = if let Token {
            value: ValueKind::String(identifier),
            ..
        } = self.current_token()
        {
            identifier.clone()
        } else {
            panic!(
                "Expected {:?} for function name, got {:?}",
                expected.clone(),
                self.current_token()
            );
        };

        identifier
    }

    pub fn get_identifier(&mut self) -> String {
        self.get(TokenKind::Identifier)
    }

    pub fn get_type(&mut self) -> String {
        self.get(TokenKind::Type)
    }

    pub fn parse(&mut self) {
        loop {
            match self.current_token().kind {
                TokenKind::Use => {
                    let mut r#use = Use::new(self);
                    let statement = r#use.parse();
                    self.tree.push(statement);
                }
                TokenKind::Operation => {
                    let mut operation = Operation::new(self);
                    let statement = operation.parse(false);
                    self.tree.push(statement);
                }
                TokenKind::Public => {
                    self.advance();

                    match self.current_token().kind {
                        TokenKind::Operation => {
                            let mut operation = Operation::new(self);
                            let statement = operation.parse(true);
                            self.tree.push(statement);
                        }
                        _ => todo!(),
                    }
                }
                _ => {
                    break;
                }
            }
        }

        dbg!(&self.tree);
    }
}
