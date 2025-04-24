use std::cell::RefCell;

use crate::{
    lexer::enums::{Location, MutRc, TokenKind},
    set_end,
};

use super::{
    enums::{AstNode, ConstantSource, Conversion, Primitive},
    parser::Parser,
    statement::{Shared, Statement},
};

pub struct Constant<'a> {
    parser: &'a mut Parser,
}

impl<'a> Constant<'a> {
    pub fn new(parser: &'a mut Parser) -> Self {
        Constant { parser }
    }

    pub fn parse(
        &mut self,
        public: bool,
        should_parse: bool,
        location: MutRc<Location>,
    ) -> Option<Primitive> {
        self.parser.advance();

        if !should_parse {
            self.parser.yield_tokens_wrapped_with_semi();
            self.parser.expect_tokens(vec![TokenKind::Semicolon]);
            self.parser.advance();

            return None;
        }

        macro_rules! yield_ty {
            () => {{
                let ty = Some(self.parser.get_type(None));
                self.parser.advance();

                ty
            }};
        }

        let ty = if self.parser.current_token().kind == TokenKind::Identifier
            || self.parser.current_token().kind == TokenKind::ExactLiteral
        {
            if let Some(next) = self.parser.tokens.get(self.parser.position + 1) {
                if next.kind == TokenKind::Equal {
                    None
                } else {
                    yield_ty!()
                }
            } else {
                yield_ty!()
            }
        } else {
            yield_ty!()
        };

        let name = self.parser.get_identifier();
        let name_token = self.parser.current_token();
        self.parser.advance();

        self.parser.expect_tokens(vec![TokenKind::Equal]);
        self.parser.advance();

        let value_location = self.parser.current_token().location;
        let tokens = self.parser.yield_tokens_wrapped_with_semi();

        // `end` here is AFTER yielding all the tokens
        set_end!(value_location, self.parser);
        self.parser.advance();

        let body: RefCell<Vec<AstNode>> = RefCell::new(vec![]);
        let value = Statement::new(
            tokens,
            0,
            &body,
            &Shared {
                struct_pool: &self.parser.struct_pool,
                enum_pool: &self.parser.enum_pool,
                tree: &self.parser.tree,
                generics: &vec![],
                known_generics: &vec![],
            },
        )
        .parse()
        .0;

        set_end!(location, self.parser);

        Some(Primitive::Constant(ConstantSource {
            name_token,
            name,
            public,
            r#type: ty.clone(),
            value: Box::new(if let Some(ty) = ty {
                AstNode::Conversion(Conversion {
                    r#type: Some(ty),
                    value: Box::new(value),
                    location: value_location,
                    explicit: false,
                })
            } else {
                value
            }),
            usable: true,
            imported: false,
            location,
        }))
    }
}
