use crate::lexer::enums::TokenKind;

use super::{
    enums::{Argument, Primitive},
    parser::Parser,
};

pub struct Struct<'a> {
    parser: &'a mut Parser,
}

impl<'a> Struct<'a> {
    pub fn new(parser: &'a mut Parser) -> Self {
        Struct { parser }
    }

    pub fn parse(&mut self, public: bool) -> Primitive {
        self.parser.advance();

        let name = self.parser.get_identifier();
        let location = self.parser.current_token().location.clone();
        self.parser.advance();

        self.parser.expect_tokens(vec![TokenKind::LeftCurlyBrace]);
        self.parser.advance();

        let mut members = vec![];

        loop {
            if self.parser.current_token().kind == TokenKind::RightCurlyBrace {
                break;
            }

            let ty = self.parser.get_type();
            self.parser.advance();

            let name = self.parser.get_identifier();
            self.parser.advance();

            self.parser.expect_tokens(vec![TokenKind::Semicolon]);
            self.parser.advance();

            members.push(Argument { name, r#type: ty })
        }

        self.parser.expect_tokens(vec![TokenKind::RightCurlyBrace]);
        self.parser.advance();

        self.parser.expect_tokens(vec![TokenKind::Semicolon]);
        self.parser.advance();

        self.parser.struct_pool.insert(name.clone());

        Primitive::Struct {
            name,
            public,
            usable: true,
            imported: false,
            members,
            location,
        }
    }
}
