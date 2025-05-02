use std::collections::HashSet;

use crate::{
    compiler::qbe::r#type::Type,
    elle_error, enum_hover,
    lexer::enums::{Attribute, Location, MutRc, Token, TokenKind, ValueKind},
    misc::{
        colors::{get_GREEN, get_RESET, GREEN, RESET},
        constants::{EQUALS_CONSTANT, FORMAT_CONSTANT},
    },
    parser::enums::{
        Argument, AstNode, BinaryOperation, Conversion, FunctionSource, IfStatement, Literal,
        Return, Variant,
    },
    set_end,
};

use super::{
    enums::{EnumSource, Primitive},
    parser::Parser,
};

pub struct Enum<'a> {
    parser: &'a mut Parser,
}

impl<'a> Enum<'a> {
    pub const fn new(parser: &'a mut Parser) -> Self {
        Self { parser }
    }

    pub fn parse(
        &mut self,
        public: bool,
        should_parse: bool,
        location: MutRc<Location>,
    ) -> Option<(Primitive, Vec<Primitive>)> {
        self.parser.advance();

        if !should_parse {
            self.parser.yield_tokens_wrapped_with_semi();
            self.parser.advance();

            return None;
        }

        self.parser.expect_tokens(&[TokenKind::Identifier]);
        let name_token = self.parser.current_token();
        let name = self.parser.get_identifier();
        self.parser.advance();

        let mut ty = None;
        let mut should_add_fmt_builtin = true;

        if self.parser.match_token(TokenKind::Attribute, false) {
            while self.parser.current_token().kind == TokenKind::Attribute && !self.parser.is_eof()
            {
                self.parser.advance();
                let attribute = self.parser.current_token().parse_attribute();

                match attribute {
                    Attribute::NoFormat => {
                        should_add_fmt_builtin = false;
                        self.parser.advance();
                    }
                    Attribute::Repr => {
                        self.parser.advance();
                        self.parser.expect_tokens(&[TokenKind::LeftParenthesis]);
                        self.parser.advance();
                        ty = Some(self.parser.get_type(None));
                        self.parser.advance();
                        self.parser.expect_tokens(&[TokenKind::RightParenthesis]);
                        self.parser.advance();
                    }
                    _ => elle_error!(self.parser.current_token().location.borrow().error(format!(
                        "Unknown attribute for enum '{}'",
                        self.parser
                            .current_token()
                            .value
                            .get_string_inner()
                            .unwrap()
                    ))),
                }
            }
        }

        self.parser.expect_tokens(&[TokenKind::LeftCurlyBrace]);
        self.parser.advance();

        let mut variants = vec![];
        let mut seen = HashSet::new();
        let mut offset_kind = TokenKind::IntegerLiteral;
        let mut offset = None;

        while self.parser.current_token().kind != TokenKind::RightCurlyBrace {
            let variant = self.parser.get_identifier();
            let variant_token = self.parser.current_token();
            let mut inner = None;

            if seen.contains(&variant) {
                elle_error!(self
                    .parser
                    .current_token()
                    .location
                    .borrow()
                    .error(format!("Cannot redefine variant '{variant}'")))
            } else {
                seen.insert(variant.clone());
            }

            self.parser.advance();

            if self.parser.current_token().kind == TokenKind::Equal {
                self.parser.advance();
                self.parser.expect_tokens(&[
                    TokenKind::StringLiteral,
                    TokenKind::IntegerLiteral,
                    TokenKind::CharLiteral,
                ]);

                match self.parser.current_token().kind {
                    TokenKind::StringLiteral => {
                        ty = Some(Type::Pointer(Box::new(Type::Char)));
                        inner = Some(self.parser.current_token());
                    }
                    x @ TokenKind::IntegerLiteral => {
                        inner = Some(self.parser.current_token());
                        offset_kind = x;
                        offset = Some(
                            self.parser
                                .current_token()
                                .value
                                .get_number_inner()
                                .unwrap()
                                .saturating_sub(variants.len() as i128),
                        );
                    }
                    x @ TokenKind::CharLiteral => {
                        inner = Some(self.parser.current_token());
                        offset_kind = x;
                        offset = Some(
                            (self.parser.current_token().value.get_char_inner().unwrap() as i128)
                                .saturating_sub(variants.len() as i128),
                        );
                    }
                    _ => {}
                }

                self.parser.advance();
            }

            let value = inner.unwrap_or_else(|| {
                let make_token = || Token {
                    kind: offset_kind,
                    value: if offset_kind == TokenKind::CharLiteral {
                        ValueKind::Character(
                            (variants.len() as u8 + offset.unwrap_or(0) as u8) as char,
                        )
                    } else {
                        ValueKind::Number(variants.len() as i128 + offset.unwrap_or(0))
                    },
                    location: location.clone(),
                    tagged: false,
                };

                ty.as_ref().map_or_else(
                    make_token,
                    |ty| {
                        if ty.is_string() {
                            elle_error!(variant_token.location.borrow().error(
                                format!("Expected every variant to be filled in an enum with repr({GREEN}string{RESET}).", GREEN = get_GREEN!(), RESET = get_RESET!())
                            ))
                        } else {
                            make_token()
                        }
                    },
                )
            });

            variants.push(Variant {
                name: variant,
                name_token: variant_token,
                value,
            });

            if self.parser.current_token().kind != TokenKind::RightCurlyBrace {
                self.parser.expect_tokens(&[TokenKind::Comma]);
                self.parser.advance();
            }
        }

        self.parser.expect_tokens(&[TokenKind::RightCurlyBrace]);
        self.parser.advance();

        set_end!(location, self.parser);

        self.parser.expect_tokens(&[TokenKind::Semicolon]);
        self.parser.advance();

        let mut builtins = vec![];

        builtins.push(Primitive::Function(FunctionSource {
            namespace_token: Token::from_ident(&name),
            name_token: Token::from_ident(EQUALS_CONSTANT),
            name: format!("{name}.{EQUALS_CONSTANT}"),
            public,
            usable: true,
            imported: false,
            variadic: false,
            external: false,
            builtin: true,
            volatile: false,
            format: false,
            unaliased: None,
            generics: vec![],
            arguments: vec![
                Argument {
                    name: "self".into(),
                    r#type: Type::Enum(name.clone(), Box::new(ty.clone())),
                    no_fmt: false,
                },
                Argument {
                    name: "other".into(),
                    r#type: Type::Enum(name.clone(), Box::new(ty.clone())),
                    no_fmt: false,
                },
            ],
            r#return: None,
            body: vec![AstNode::Return(Return {
                value: Box::new(AstNode::BinaryOperation(BinaryOperation {
                    left: Box::new(AstNode::Conversion(Conversion {
                        r#type: ty.clone().or(Some(Type::Word)),
                        value: Box::new(AstNode::token_to_literal(Token::from_ident("self"))),
                        location: location.clone(),
                        explicit: true,
                    })),
                    right: Box::new(AstNode::Conversion(Conversion {
                        r#type: ty.clone().or(Some(Type::Word)),
                        value: Box::new(AstNode::token_to_literal(Token::from_ident("other"))),
                        location: location.clone(),
                        explicit: true,
                    })),
                    operator: TokenKind::EqualTo,
                    treat_as_string: true,
                    dunder_methods: true,
                    location: location.clone(),
                })),
                location: location.clone(),
            })],
            location: location.clone(),
            return_location: location.clone(),
        }));

        if should_add_fmt_builtin {
            builtins.push(Primitive::Function(FunctionSource {
                namespace_token: Token::from_ident(&name),
                name_token: Token::from_ident(FORMAT_CONSTANT),
                name: format!("{name}.{FORMAT_CONSTANT}"),
                public,
                usable: true,
                imported: false,
                variadic: false,
                external: false,
                builtin: true,
                volatile: false,
                format: false,
                unaliased: None,
                generics: vec![],
                arguments: vec![
                    Argument {
                        name: "self".into(),
                        r#type: Type::Enum(name.clone(), Box::new(ty.clone())),
                        no_fmt: false,
                    },
                    Argument {
                        name: "nesting".into(),
                        r#type: Type::Word,
                        no_fmt: false,
                    },
                ],
                r#return: None,
                body: variants
                    .iter()
                    .map(|x| {
                        AstNode::IfStatement(IfStatement {
                            condition: Box::new(AstNode::BinaryOperation(BinaryOperation {
                                left: Box::new(AstNode::token_to_literal(Token::from_ident(
                                    "self",
                                ))),
                                right: Box::new(AstNode::Conversion(Conversion {
                                    r#type: ty.clone().or(Some(Type::Word)),
                                    value: Box::new(AstNode::token_to_literal(x.value.clone())),
                                    location: location.clone(),
                                    explicit: true,
                                })),
                                operator: TokenKind::EqualTo,
                                treat_as_string: true,
                                dunder_methods: true,
                                location: location.clone(),
                            })),
                            body: vec![AstNode::Return(Return {
                                value: Box::new(AstNode::Literal(Literal {
                                    kind: TokenKind::StringLiteral,
                                    value: ValueKind::String(x.name.clone()),
                                    location: location.clone(),
                                    tagged: false,
                                })),
                                location: location.clone(),
                            })],
                            elifs: vec![],
                            else_body: vec![],
                            location: location.clone(),
                        })
                    })
                    .collect::<Vec<AstNode>>(),
                location: location.clone(),
                return_location: location.clone(),
            }));
        }

        self.parser
            .enum_pool
            .borrow_mut()
            .insert(name.clone(), (variants.clone(), ty));

        enum_hover!(name_token, name, variants);

        Some((
            Primitive::Enum(EnumSource {
                name,
                name_token,
                public,
                usable: true,
                imported: false,
                variants,
                location,
            }),
            builtins,
        ))
    }
}
