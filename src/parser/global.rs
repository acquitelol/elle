use std::cell::RefCell;

use crate::{
    elle_error,
    lexer::enums::{Attribute, Location, MutRc, Token, TokenKind, ValueKind},
    parser::enums::GlobalSource,
    set_end, INTERNAL_GLOBAL_INIT_FORMAT,
};

use super::{
    enums::{AstNode, Conversion, Primitive},
    parser::Parser,
    statement::{Shared, Statement},
};

pub struct Global<'a> {
    parser: &'a mut Parser,
}

impl<'a> Global<'a> {
    pub const fn new(parser: &'a mut Parser) -> Self {
        Self { parser }
    }

    pub fn parse(
        &mut self,
        public: bool,
        external: bool,
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
                || self.parser.enum_pool.borrow().contains_key(&name)
                || ValueKind::String(name.clone()).is_base_type())
            {
                elle_error!(
                    name_token.location.borrow().error(format!(
                        "Cannot create a namespaced constant for '{name}' because it isn't a struct, enum or primitive type.\n{}",
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

        let mut ty = None;

        if external {
            self.parser.expect_tokens(&[TokenKind::Colon]);
        }

        if self.parser.current_token().kind == TokenKind::Colon {
            self.parser.advance();

            ty = Some(self.parser.get_type(None));
            self.parser.advance();
        }

        let mut expand_main = false;

        if self.parser.match_token(TokenKind::Attribute, false) {
            while self.parser.current_token().kind == TokenKind::Attribute && !self.parser.is_eof()
            {
                self.parser.advance();
                let attribute = self.parser.current_token().parse_attribute();

                match attribute {
                    Attribute::ExpandMain => {
                        expand_main = true;
                        self.parser.advance();
                    }
                    _ => elle_error!(self.parser.current_token().location.borrow().error(format!(
                        "Unknown attribute for global '{}'",
                        self.parser
                            .current_token()
                            .value
                            .get_string_inner()
                            .unwrap()
                    ))),
                }
            }
        }

        if external {
            self.parser.expect_tokens(&[TokenKind::Semicolon]);
            self.parser.advance();

            set_end!(location, self.parser);
            return Some(Primitive::Global(GlobalSource {
                namespace_token,
                name_token,
                method_name: format!(INTERNAL_GLOBAL_INIT_FORMAT!(), name),
                name,
                public,
                r#type: ty.clone(),
                value: None,
                usable: true,
                imported: false,
                external: true,
                expand_main,
                location,
            }));
        }

        let mut value = None;

        if self.parser.current_token().kind == TokenKind::Equal || ty.is_none() {
            self.parser.expect_tokens(&[TokenKind::Equal]);
            self.parser.advance();

            let value_location = self.parser.current_token().location;
            let tokens = self.parser.yield_tokens_wrapped_with_semi();

            // `end` here is AFTER yielding all the tokens
            set_end!(value_location, self.parser);
            self.parser.advance();

            let body: RefCell<Vec<AstNode>> = RefCell::new(vec![]);
            value = Some(
                Statement::new(
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
                        tmp_counter: &RefCell::new(0),
                    },
                )
                .parse()
                .0,
            );
        } else {
            self.parser.expect_tokens(&[TokenKind::Semicolon]);
            self.parser.advance();
        }

        set_end!(location, self.parser);

        Some(Primitive::Global(GlobalSource {
            namespace_token,
            name_token,
            method_name: format!(INTERNAL_GLOBAL_INIT_FORMAT!(), name),
            name,
            public,
            r#type: ty.clone(),
            value: value.map(|value| {
                Box::new(if let Some(ty) = ty {
                    AstNode::Conversion(Conversion {
                        r#type: Some(ty),
                        value: Box::new(value),
                        location: location.clone(),
                        explicit: false,
                    })
                } else {
                    value
                })
            }),
            usable: true,
            imported: false,
            external: false,
            expand_main,
            location,
        }))
    }
}
