use crate::{hashmap, lexer::enums::TokenKind, misc::colors::*};

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

    pub fn parse(&mut self, public: bool, namespace: bool) -> Primitive {
        let keyword_location = self.parser.current_token().location.clone();
        self.parser.advance();

        let name = self.parser.get_identifier();
        let location = self.parser.current_token().location.clone();
        self.parser.advance();

        if namespace {
            match self.parser.current_token().kind {
                TokenKind::LeftCurlyBrace => {
                    let mut location = self.parser.current_token().location;
                    location.length = location.ctx.len() - location.column + 1;
                    location.column += location.ctx.len() - location.column;

                    panic!(
                        "{}",
                        location.with_extra_info("Remove this part").error(format!(
                            "Cannot declare members on a namespace.\nTo declare members, use the '{GREEN}struct{RESET}' keyword instead."))
                    )
                }
                _ => self.parser.expect_tokens(vec![TokenKind::Semicolon]),
            };

            self.parser.advance();
            self.parser
                .struct_pool
                .borrow_mut()
                .insert(name.clone(), (vec![], vec![], location.clone()));

            return Primitive::Struct {
                name,
                public,
                usable: true,
                imported: false,
                generics: vec![],
                known_generics: hashmap![],
                members: vec![],
                keyword_location,
                location,
                ignore_empty: namespace,
            };
        }

        let mut generics = vec![];

        if self.parser.current_token().kind == TokenKind::LessThan {
            self.parser.advance();

            while self.parser.current_token().kind != TokenKind::GreaterThan {
                generics.push(self.parser.get_identifier());
                self.parser.advance();

                if self.parser.current_token().kind == TokenKind::Comma {
                    self.parser.advance();
                }
            }

            self.parser.expect_tokens(vec![TokenKind::GreaterThan]);
            self.parser.advance();
        }

        self.parser.expect_tokens(vec![TokenKind::LeftCurlyBrace]);
        self.parser.advance();

        let mut members = vec![];

        loop {
            if self.parser.current_token().kind == TokenKind::RightCurlyBrace {
                break;
            }

            let ty = self.parser.get_type(Some(&generics));
            self.parser.advance();

            let name = self.parser.get_identifier();
            self.parser.advance();

            self.parser.expect_tokens(vec![TokenKind::Semicolon]);
            self.parser.advance();

            members.push(Argument {
                name,
                r#type: ty,
                manual: false,
            })
        }

        self.parser.struct_pool.borrow_mut().insert(
            name.clone(),
            (generics.clone(), members.clone(), location.clone()),
        );

        self.parser.expect_tokens(vec![TokenKind::RightCurlyBrace]);
        self.parser.advance();

        self.parser.expect_tokens(vec![TokenKind::Semicolon]);
        self.parser.advance();

        Primitive::Struct {
            name,
            public,
            usable: true,
            imported: false,
            generics,
            known_generics: hashmap![],
            members,
            keyword_location,
            location,
            ignore_empty: namespace,
        }
    }
}
