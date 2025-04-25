use std::fmt::Write as _;
use std::rc::Rc;

use crate::{
    elle_error,
    lexer::enums::{TokenKind, ValueKind},
    set_end,
};

use super::{
    enums::{Primitive, UseSource},
    parser::Parser,
};

pub struct Use<'a> {
    parser: &'a mut Parser,
}

impl<'a> Use<'a> {
    pub const fn new(parser: &'a mut Parser) -> Self {
        Use { parser }
    }

    fn get_string(&self) -> String {
        if let ValueKind::String(val) = self.parser.current_token().value {
            val
        } else {
            elle_error!(self
                .parser
                .current_token()
                .location
                .borrow()
                .error("Token is not a string"))
        }
    }

    pub fn parse(&mut self) -> Primitive {
        let location = self.parser.current_token().location;
        self.parser.advance();
        let mut module = String::new();
        let valid = [
            TokenKind::Identifier,
            TokenKind::Divide,
            TokenKind::Range,
            TokenKind::Dot,
        ];

        while valid.contains(&self.parser.current_token().kind) && !self.parser.is_eof() {
            match self.parser.current_token().kind {
                TokenKind::Range => {
                    // Allow for ../foo/bar
                    module.push_str("..");
                    self.parser.advance();
                }

                TokenKind::Dot => {
                    // Allow for ./foo/bar
                    module.push('.');
                    self.parser.advance();
                }

                TokenKind::Divide => {
                    self.parser.advance();

                    // Allow for foo////bar to parse as foo/bar
                    while self.parser.current_token().kind == TokenKind::Divide
                        && !self.parser.is_eof()
                    {
                        self.parser.advance();
                    }

                    match self.parser.current_token().kind {
                        // Allow for foo/../bar
                        TokenKind::Range => module.push_str("/.."),

                        // Allow for foo/./bar
                        TokenKind::Dot => module.push_str("/."),
                        _ => write!(module, "/{}", self.get_string())
                            .expect("could not write to string"),
                    }

                    self.parser.advance();
                }

                _ => {
                    module.push_str(&self.get_string());
                    self.parser.advance();
                }
            }
        }

        set_end!(location, self.parser);
        self.parser.expect_tokens(&[TokenKind::Semicolon]);
        self.parser.advance();

        Primitive::Use(UseSource {
            module: Rc::new(module),
            location,
        })
    }
}
