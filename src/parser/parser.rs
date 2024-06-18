use crate::{
    compiler::enums::Type,
    lexer::enums::{Token, TokenKind, ValueKind},
    parser::{constant::Constant, function::Function},
};

use super::{enums::Primitive, r#use::Use};

pub struct Parser {
    pub tokens: Vec<Token>,
    pub position: usize,
    pub tree: Vec<Primitive>,
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

    fn next_token(&mut self) -> Option<Token> {
        match self.is_eof() {
            true => None,
            false => Some(self.tokens[self.position + 1].clone()),
        }
    }

    pub fn advance(&mut self) {
        if self.is_eof() {
            println!("The position of {:?} is the last index of the token stack. Staying at the same position.", self.position);
        } else {
            self.position += 1;
        }
    }

    pub fn advance_opt(&mut self) -> Option<()> {
        if self.is_eof() {
            None
        } else {
            self.position += 1;
            Some(())
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

    pub fn get_type(&mut self) -> Type {
        let mut ty = ValueKind::String(self.get(TokenKind::Type))
            .to_type_string()
            .unwrap();

        loop {
            let tmp = self.next_token();

            if tmp.is_some() {
                match tmp.unwrap().kind {
                    TokenKind::Multiply => {
                        ty = Type::Pointer(Box::new(ty));
                        self.advance();
                    }
                    _ => break,
                }
            }
        }

        ty
    }

    pub fn parse(&mut self) -> Vec<Primitive> {
        let mut public = false;
        let mut external = false;

        loop {
            match self.current_token().kind {
                TokenKind::Public => {
                    public = true;
                    self.advance();
                }
                TokenKind::External => {
                    external = true;
                    self.advance();
                }
                TokenKind::Use => {
                    let mut r#use = Use::new(self);
                    let statement = r#use.parse();
                    self.tree.push(statement);
                }
                TokenKind::Function => {
                    let mut function = Function::new(self);
                    let statement = function.parse(public, external);
                    self.tree.push(statement);

                    public = false;
                    external = false;
                }
                TokenKind::Constant => {
                    let mut constant = Constant::new(self);
                    let statement = constant.parse(public, external);
                    self.tree.push(statement);

                    public = false;
                    external = false;
                }
                _ => {
                    dbg!(self.current_token().kind);
                    break;
                }
            }
        }

        return self.tree.clone();
    }
}
