use std::cell::RefCell;

use crate::{
    elle_error,
    lexer::enums::{Location, MutRc, Token, TokenKind, ValueKind},
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
    pub const fn new(parser: &'a mut Parser) -> Self {
        Self { parser }
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
            self.parser.expect_tokens(&[TokenKind::Semicolon]);
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
                if [TokenKind::Equal, TokenKind::DoubleColon].contains(&next.kind) {
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

        let mut name = self.parser.get_identifier();
        let mut namespace_token = Token::from_ident("");
        let mut name_token = self.parser.current_token();
        self.parser.advance();

        if self.parser.current_token().kind == TokenKind::Dot {
            elle_error!(self.parser.current_token().location.borrow().error(
                "Cannot create a namespaced constant called using '.'\nPlease use '::' instead."
            ))
        }

        if self.parser.current_token().kind == TokenKind::DoubleColon {
            if !(self.parser.struct_pool.borrow().contains_key(&name)
                // TODO: If I ever find out a way to allow namespaced
                // constants but only for lambda defs, uncomment this:
                // || self.parser.enum_pool.borrow().contains_key(&name)
                || ValueKind::String(name.clone()).is_base_type())
            {
                elle_error!(
                    name_token.location.borrow().error(format!(
                        "Cannot create a namespaced constant for '{name}' because it isn't a struct or primitive type.\n{}",
                        ValueKind::similar_mapping(&name)
                            .map_or_else(
                                || format!("Are you sure you spelt '{name}' correctly?"),
                                |map| format!("A similar type exists which might be what you need: '{map}'")
                            )
                    ))
                )
            }

            self.parser.advance();

            let identifier = self.parser.get_identifier();
            name = format!("{name}.{identifier}");

            namespace_token = name_token;
            name_token = self.parser.current_token();
            self.parser.advance();
        }

        self.parser.expect_tokens(&[TokenKind::Equal]);
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
                addr_only: false,
            },
        )
        .parse()
        .0;

        set_end!(location, self.parser);

        Some(Primitive::Constant(ConstantSource {
            namespace_token,
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
