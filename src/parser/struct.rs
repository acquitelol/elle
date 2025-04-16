use std::rc::Rc;

use crate::{
    compiler::qbe::r#type::Type,
    elle_error, hashmap,
    lexer::enums::{Attribute, Location, Token, TokenKind, ValueKind},
    misc::{colors::*, interleave_with},
    FORMAT_CONSTANT, GENERIC_END, GENERIC_IDENTIFIER, INTERNAL_FORMATTER,
};

use super::{
    enums::{
        Argument, AstNode, BinaryOperation, Declare, FieldAccess, FunctionCall, FunctionSource,
        Literal, Primitive, Return, StructSource,
    },
    parser::Parser,
};

pub struct Struct<'a> {
    parser: &'a mut Parser,
}

impl<'a> Struct<'a> {
    pub fn new(parser: &'a mut Parser) -> Self {
        Struct { parser }
    }

    pub fn parse(
        &mut self,
        public: bool,
        namespace: bool,
        should_parse: bool,
        mut location: Location,
    ) -> Option<(Primitive, Vec<Primitive>)> {
        if !should_parse {
            if namespace {
                while self.parser.current_token().kind != TokenKind::Semicolon
                    && !self.parser.is_eof()
                {
                    self.parser.advance();
                }
            } else {
                while self.parser.current_token().kind != TokenKind::RightCurlyBrace
                    && !self.parser.is_eof()
                {
                    self.parser.advance();
                }

                self.parser.expect_tokens(vec![TokenKind::RightCurlyBrace]);
                self.parser.advance();
            }

            self.parser.expect_tokens(vec![TokenKind::Semicolon]);
            self.parser.advance();

            return None;
        }

        let keyword_location = self.parser.current_token().location.clone();
        self.parser.advance();

        let name = self.parser.get_identifier();
        let name_token = self.parser.current_token();
        location.end = self.parser.current_token().location.end.clone();
        self.parser.advance();

        if namespace {
            match self.parser.current_token().kind {
                TokenKind::LeftCurlyBrace => {
                    elle_error!(
                        location.error(format!(
                            "Cannot declare members on a namespace.\nTo declare members, use the '{GREEN}struct{RESET}' keyword instead.",
                            GREEN = get_GREEN!(),
                            RESET = get_RESET!()
                        ))
                    )
                }
                _ => self.parser.expect_tokens(vec![TokenKind::Semicolon]),
            };

            location.end = self.parser.current_token().location.end.clone();
            self.parser.advance();
            self.parser
                .struct_pool
                .borrow_mut()
                .insert(name.clone(), (vec![], vec![], Rc::new(location.clone())));

            return Some((
                Primitive::Struct(StructSource {
                    name,
                    name_token,
                    public,
                    usable: true,
                    imported: false,
                    generics: vec![],
                    known_generics: hashmap![],
                    members: vec![],
                    keyword_location,
                    location: Rc::new(location.clone()),
                    ignore_empty: namespace,
                }),
                vec![],
            ));
        }

        let mut generics = vec![];

        if self.parser.current_token().kind == TokenKind::LessThan {
            self.parser.advance();

            while self.parser.current_token().kind != TokenKind::GreaterThan
                && !self.parser.is_eof()
            {
                generics.push(self.parser.get_identifier());
                self.parser.advance();

                if self.parser.current_token().kind == TokenKind::Comma {
                    self.parser.advance();
                }
            }

            self.parser.expect_tokens(vec![TokenKind::GreaterThan]);
            self.parser.advance();
        }

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
                    _ => elle_error!(self.parser.current_token().location.error(format!(
                        "Unknown attribute for struct '{}'",
                        self.parser
                            .current_token()
                            .value
                            .get_string_inner()
                            .unwrap()
                    ))),
                }
            }
        }

        self.parser.expect_tokens(vec![TokenKind::LeftCurlyBrace]);
        self.parser.advance();

        let mut members = vec![];

        self.parser.struct_pool.borrow_mut().insert(
            name.clone(),
            (generics.clone(), members.clone(), Rc::new(location.clone())),
        );

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
                no_fmt: false,
            })
        }

        self.parser.expect_tokens(vec![TokenKind::RightCurlyBrace]);
        self.parser.advance();

        self.parser.expect_tokens(vec![TokenKind::Semicolon]);
        location.end = self.parser.current_token().location.end.clone();
        self.parser.advance();

        self.parser.struct_pool.borrow_mut().insert(
            name.clone(),
            (generics.clone(), members.clone(), Rc::new(location.clone())),
        );

        let mut builtins = vec![];

        if should_add_fmt_builtin {
            let parameters = members
                .iter()
                .cloned()
                .map(|member| {
                    let field = AstNode::FieldAccess(FieldAccess {
                        left: Box::new(AstNode::Literal(Literal {
                            kind: TokenKind::Identifier,
                            value: ValueKind::String("self".into()),
                            location: Rc::new(location.clone()),
                            tagged: false,
                        })),
                        right: Box::new(AstNode::Literal(Literal {
                            kind: TokenKind::Identifier,
                            value: ValueKind::String(member.name),
                            location: Rc::new(location.clone()),
                            tagged: false,
                        })),
                        value: None,
                        location: Rc::new(location.clone()),
                    });

                    (
                        Rc::new(location.clone()),
                        AstNode::FunctionCall(FunctionCall {
                            name: FORMAT_CONSTANT.into(),
                            generics: vec![],
                            parameters: vec![
                                (Rc::new(location.clone()), field),
                                (
                                    Rc::new(location.clone()),
                                    AstNode::BinaryOperation(BinaryOperation {
                                        left: Box::new(AstNode::Literal(Literal {
                                            kind: TokenKind::Identifier,
                                            value: ValueKind::String("nesting".into()),
                                            location: Rc::new(location.clone()),
                                            tagged: false,
                                        })),
                                        right: Box::new(AstNode::Literal(Literal {
                                            kind: TokenKind::IntegerLiteral,
                                            value: ValueKind::Number(1),
                                            location: Rc::new(location.clone()),
                                            tagged: false,
                                        })),
                                        operator: TokenKind::Add,
                                        treat_as_string: false,
                                        dunder_methods: true,
                                        location: Rc::new(location.clone()),
                                    }),
                                ),
                            ],
                            type_method: true,
                            ignore_no_def: false,
                            location: Rc::new(location.clone()),
                        }),
                    )
                })
                .collect::<Vec<(Rc<Location>, AstNode)>>();

            let mut interleaved = interleave_with(
                parameters,
                (
                    Rc::new(location.clone()),
                    AstNode::Literal(Literal {
                        kind: TokenKind::Identifier,
                        value: ValueKind::String("spacing".into()),
                        location: Rc::new(location.clone()),
                        tagged: false,
                    }),
                ),
            );

            interleaved.insert(
                0,
                (
                    Rc::new(location.clone()),
                    AstNode::Literal(Literal {
                        kind: TokenKind::StringLiteral,
                        value: ValueKind::String(format!(
                            "{name} {{{{\n{}\n{{}}}}",
                            members
                                .iter()
                                .cloned()
                                .map(|member| format!("    {{}}{} = {{}}", member.name))
                                .collect::<Vec<String>>()
                                .join("\n")
                        )),
                        location: Rc::new(location.clone()),
                        tagged: false,
                    }),
                ),
            );

            // Spacing for the last curly brace
            interleaved.push((
                Rc::new(location.clone()),
                AstNode::Literal(Literal {
                    kind: TokenKind::Identifier,
                    value: ValueKind::String("spacing".into()),
                    location: Rc::new(location.clone()),
                    tagged: false,
                }),
            ));

            builtins.push(Primitive::Function(FunctionSource {
                namespace_token: Token::from_ident(&name),
                name_token: Token::from_ident(FORMAT_CONSTANT),
                name: format!("{}.{FORMAT_CONSTANT}", name),
                public,
                usable: true,
                imported: false,
                variadic: false,
                external: false,
                builtin: true,
                volatile: false,
                format: false,
                unaliased: None,
                generics: generics.clone(),
                arguments: vec![
                    Argument {
                        name: "self".into(),
                        r#type: Type::Struct(if generics.len() > 0 {
                            format!(
                                "{name}.{GENERIC_IDENTIFIER}.{}.{GENERIC_END}",
                                generics
                                    .iter()
                                    .cloned()
                                    .map(|ty| Type::Unknown(ty).to_internal_id().to_string())
                                    .collect::<Vec<String>>()
                                    .join(".")
                            )
                        } else {
                            name.clone()
                        }),
                        no_fmt: false,
                    },
                    Argument {
                        name: "nesting".into(),
                        r#type: Type::Word,
                        no_fmt: false,
                    },
                ],
                r#return: Some(Type::Pointer(Box::new(Type::Char))),
                body: vec![
                    AstNode::Declare(Declare {
                        name: Token::from_ident("spacing"),
                        r#type: Some(Type::Pointer(Box::new(Type::Char))),
                        value: Some(Box::new(AstNode::FunctionCall(FunctionCall {
                            name: "string.repeat".into(),
                            generics: vec![],
                            parameters: vec![
                                (
                                    Rc::new(location.clone()),
                                    AstNode::Literal(Literal {
                                        kind: TokenKind::StringLiteral,
                                        value: ValueKind::String(" ".into()),
                                        location: Rc::new(location.clone()),
                                        tagged: false,
                                    }),
                                ),
                                (
                                    Rc::new(location.clone()),
                                    AstNode::BinaryOperation(BinaryOperation {
                                        left: Box::new(AstNode::Literal(Literal {
                                            kind: TokenKind::Identifier,
                                            value: ValueKind::String("nesting".into()),
                                            location: Rc::new(location.clone()),
                                            tagged: false,
                                        })),
                                        right: Box::new(AstNode::Literal(Literal {
                                            kind: TokenKind::IntegerLiteral,
                                            value: ValueKind::Number(4),
                                            location: Rc::new(location.clone()),
                                            tagged: false,
                                        })),
                                        operator: TokenKind::Multiply,
                                        treat_as_string: false,
                                        dunder_methods: true,
                                        location: Rc::new(location.clone()),
                                    }),
                                ),
                            ],
                            type_method: false,
                            ignore_no_def: false,
                            location: Rc::new(location.clone()),
                        }))),
                        location: Rc::new(location.clone()),
                        value_location: Rc::new(location.clone()),
                    }),
                    AstNode::Return(Return {
                        value: Box::new(AstNode::FunctionCall(FunctionCall {
                            name: format!("string.{}", INTERNAL_FORMATTER).into(),
                            generics: vec![],
                            parameters: interleaved,
                            type_method: false,
                            ignore_no_def: false,
                            location: Rc::new(location.clone()),
                        })),
                        location: Rc::new(location.clone()),
                    }),
                ],
                location: Rc::new(location.clone()),
                return_location: Rc::new(location.clone()),
            }));
        }

        Some((
            Primitive::Struct(StructSource {
                name: name.clone(),
                name_token,
                public,
                usable: true,
                imported: false,
                generics,
                known_generics: hashmap![],
                members,
                keyword_location,
                location: Rc::new(location.clone()),
                ignore_empty: namespace,
            }),
            builtins,
        ))
    }
}
