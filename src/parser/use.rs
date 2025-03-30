use crate::{
    elle_error,
    lexer::enums::{TokenKind, ValueKind},
};

use super::{enums::Primitive, parser::Parser};

pub struct Use<'a> {
    parser: &'a mut Parser,
}

impl<'a> Use<'a> {
    pub fn new(parser: &'a mut Parser) -> Self {
        Use { parser }
    }

    fn get_string(&self) -> String {
        match self.parser.current_token().value {
            ValueKind::String(val) => val,
            _ => elle_error!(self
                .parser
                .current_token()
                .location
                .error("Token is not a string")),
        }
    }

    pub fn parse(&mut self) -> Primitive {
        self.parser.advance();
        let mut module = String::new();
        let location = self.parser.current_token().location;
        let valid = [
            TokenKind::Identifier,
            TokenKind::Divide,
            TokenKind::Range,
            TokenKind::Dot,
        ];

        while valid.contains(&self.parser.current_token().kind) {
            match self.parser.current_token().kind {
                TokenKind::Range => {
                    // Allow for ../foo/bar
                    module.push_str("..");
                    self.parser.advance();
                }

                TokenKind::Dot => {
                    // Allow for ./foo/bar
                    module.push_str(".");
                    self.parser.advance();
                }

                TokenKind::Divide => {
                    self.parser.advance();

                    // Allow for foo////bar to parse as foo/bar
                    while self.parser.current_token().kind == TokenKind::Divide {
                        self.parser.advance();
                    }

                    match self.parser.current_token().kind {
                        // Allow for foo/../bar
                        TokenKind::Range => module.push_str("/.."),

                        // Allow for foo/./bar
                        TokenKind::Dot => module.push_str("/."),
                        _ => module.push_str(&format!("/{}", self.get_string())),
                    }

                    self.parser.advance();
                }

                _ => {
                    module.push_str(&self.get_string());
                    self.parser.advance();
                }
            }
        }

        self.parser.expect_tokens(vec![TokenKind::Semicolon]);
        self.parser.advance();

        Primitive::Use { module, location }
    }
}
