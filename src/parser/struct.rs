use crate::{
    compiler::enums::Type,
    hashmap,
    lexer::enums::{Attribute, Location, TokenKind, ValueKind},
    misc::{colors::*, interleave_with},
    GENERIC_END, GENERIC_IDENTIFIER, INTERNAL_FORMATTER,
};

use super::{
    enums::{Argument, AstNode, Primitive},
    parser::Parser,
};

pub struct Struct<'a> {
    parser: &'a mut Parser,
}

impl<'a> Struct<'a> {
    pub fn new(parser: &'a mut Parser) -> Self {
        Struct { parser }
    }

    pub fn parse(&mut self, public: bool, namespace: bool) -> (Primitive, Vec<Primitive>) {
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

            return (
                Primitive::Struct {
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
                },
                vec![],
            );
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

        let mut should_add_fmt_builtin = true;

        if self.parser.match_token(TokenKind::Attribute, false) {
            while self.parser.current_token().kind == TokenKind::Attribute {
                self.parser.advance();
                let attribute = self.parser.current_token().parse_attribute();

                match attribute {
                    Attribute::NoFormat => {
                        should_add_fmt_builtin = false;
                        self.parser.advance();
                    }
                    _ => panic!(
                        "{}",
                        self.parser.current_token().location.error(format!(
                            "Unknown attribute for struct '{}'",
                            self.parser
                                .current_token()
                                .value
                                .get_string_inner()
                                .unwrap()
                        ))
                    ),
                }
            }
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

        let mut builtins = vec![];

        if should_add_fmt_builtin {
            let parameters = members
                .iter()
                .cloned()
                .map(|member| {
                    let field = AstNode::FieldStatement {
                        left: Box::new(AstNode::LiteralStatement {
                            kind: TokenKind::Identifier,
                            value: ValueKind::String("self".into()),
                            location: location.clone(),
                        }),
                        right: Box::new(AstNode::LiteralStatement {
                            kind: TokenKind::Identifier,
                            value: ValueKind::String(member.name),
                            location: location.clone(),
                        }),
                        value: None,
                        location: location.clone(),
                    };

                    (
                        location.clone(),
                        AstNode::FunctionCall {
                            name: "__fmt__".into(),
                            generics: vec![],
                            parameters: vec![
                                (location.clone(), field),
                                (
                                    location.clone(),
                                    AstNode::ArithmeticOperation {
                                        left: Box::new(AstNode::LiteralStatement {
                                            kind: TokenKind::Identifier,
                                            value: ValueKind::String("nesting".into()),
                                            location: location.clone(),
                                        }),
                                        right: Box::new(AstNode::LiteralStatement {
                                            kind: TokenKind::IntegerLiteral,
                                            value: ValueKind::Number(1),
                                            location: location.clone(),
                                        }),
                                        operator: TokenKind::Add,
                                        treat_as_string: false,
                                        location: location.clone(),
                                    },
                                ),
                            ],
                            type_method: true,
                            ignore_no_def: false,
                            location: location.clone(),
                        },
                    )
                })
                .collect::<Vec<(Location, AstNode)>>();

            let mut interleaved = interleave_with(
                parameters,
                (
                    location.clone(),
                    AstNode::LiteralStatement {
                        kind: TokenKind::Identifier,
                        value: ValueKind::String("spacing".into()),
                        location: location.clone(),
                    },
                ),
            );

            interleaved.insert(
                0,
                (
                    location.clone(),
                    AstNode::LiteralStatement {
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
                        location: location.clone(),
                    },
                ),
            );

            // Spacing for the last curly brace
            interleaved.push((
                location.clone(),
                AstNode::LiteralStatement {
                    kind: TokenKind::Identifier,
                    value: ValueKind::String("spacing".into()),
                    location: location.clone(),
                },
            ));

            builtins.push(Primitive::Function {
                name: format!("{}.__fmt__", name),
                public,
                usable: true,
                imported: false,
                variadic: false,
                manual: false,
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
                        manual: false,
                    },
                    Argument {
                        name: "nesting".into(),
                        r#type: Type::Word,
                        manual: false,
                    },
                ],
                r#return: Some(Type::Pointer(Box::new(Type::Char))),
                body: vec![
                    AstNode::DeclareStatement {
                        name: "spacing".into(),
                        r#type: Some(Type::Pointer(Box::new(Type::Char))),
                        value: Box::new(AstNode::FunctionCall {
                            name: "string.repeat".into(),
                            generics: vec![],
                            parameters: vec![
                                (
                                    location.clone(),
                                    AstNode::LiteralStatement {
                                        kind: TokenKind::StringLiteral,
                                        value: ValueKind::String(" ".into()),
                                        location: location.clone(),
                                    },
                                ),
                                (
                                    location.clone(),
                                    AstNode::ArithmeticOperation {
                                        left: Box::new(AstNode::LiteralStatement {
                                            kind: TokenKind::Identifier,
                                            value: ValueKind::String("nesting".into()),
                                            location: location.clone(),
                                        }),
                                        right: Box::new(AstNode::LiteralStatement {
                                            kind: TokenKind::IntegerLiteral,
                                            value: ValueKind::Number(4),
                                            location: location.clone(),
                                        }),
                                        operator: TokenKind::Multiply,
                                        treat_as_string: false,
                                        location: location.clone(),
                                    },
                                ),
                            ],
                            type_method: false,
                            ignore_no_def: false,
                            location: location.clone(),
                        }),
                        location: location.clone(),
                        value_location: location.clone(),
                    },
                    AstNode::ReturnStatement {
                        value: Box::new(AstNode::FunctionCall {
                            name: format!("string.{}", INTERNAL_FORMATTER).into(),
                            generics: vec![],
                            parameters: interleaved,
                            type_method: false,
                            ignore_no_def: false,
                            location: location.clone(),
                        }),
                        location: location.clone(),
                    },
                ],
                location: location.clone(),
                return_location: location.clone(),
            });
        }

        (
            Primitive::Struct {
                name: name.clone(),
                public,
                usable: true,
                imported: false,
                generics,
                known_generics: hashmap![],
                members,
                keyword_location,
                location: location.clone(),
                ignore_empty: namespace,
            },
            builtins,
        )
    }
}
