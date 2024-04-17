use crate::lexer::enums::{TokenKind, ValueKind};

use super::{enums::Primitive, parser::Parser};

pub struct Constant<'a> {
    parser: &'a mut Parser,
}

impl<'a> Constant<'a> {
    pub fn new(parser: &'a mut Parser) -> Self {
        Constant { parser }
    }

    pub fn parse(&mut self, public: bool) -> Primitive {
        self.parser.advance();

        let name = self.parser.get_identifier();

        self.parser.advance();

        let unparsed_type = match self.parser.current_token().kind {
            TokenKind::Colon => {
                self.parser.advance();
                let r#type = self.parser.get_type();
                self.parser.advance();

                self.parser.expect_token(TokenKind::Equal);
                self.parser.advance();

                Some(r#type)
            }

            TokenKind::Equal => {
                self.parser.advance();
                None
            }
            _ => None,
        };

        if !self.parser.current_token().kind.is_literal() {
            panic!("Constants can only be literal expressions.");
        }

        let value = self.parser.current_token().value;

        self.parser.advance();
        self.parser.expect_token(TokenKind::Semicolon);
        self.parser.advance();

        let r#type = match unparsed_type {
            Some(r#type) => r#type,
            None => match value {
                ValueKind::String(_) => "String".to_string(),
                ValueKind::Number(_) => "Int".to_string(),
                ValueKind::Character(_) => "Char".to_string(),
                ValueKind::Nil => "Nil".to_string(),
            },
        };

        Primitive::Constant {
            name,
            public,
            r#type,
            value,
        }
    }
}
