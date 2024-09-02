use std::cell::RefCell;

use crate::{
    lexer::enums::{Attribute, TokenKind, ValueKind},
    parser::statement::Shared,
    Warning,
};

use super::{
    enums::{Argument, AstNode, Primitive},
    parser::Parser,
    statement::Statement,
};

pub struct Function<'a> {
    parser: &'a mut Parser,
}

impl<'a> Function<'a> {
    pub fn new(parser: &'a mut Parser) -> Self {
        Function { parser }
    }

    pub fn parse(&mut self, public: bool, external: bool) -> Primitive {
        self.parser.advance();

        let mut name = self.parser.get_identifier();
        let location = self.parser.current_token().location.clone();

        self.parser.advance();

        if self.parser.current_token().kind == TokenKind::Dot {
            panic!(
                "{}",
                self.parser.current_token().location.error(format!(
                    "Cannot create a method for '{}' using '.'\nPlease use '::' instead.",
                    name
                ))
            )
        }

        if self.parser.current_token().kind == TokenKind::DoubleColon {
            if !(self.parser.struct_pool.contains(&name)
                || self.parser.generic_keys.contains(&name)
                || ValueKind::String(name.clone()).is_base_type())
            {
                panic!(
                    "{}",
                    location.error(format!(
                        "Cannot create a method for '{}' because it isn't a struct or primitive type.\n{}",
                        name.clone(), if let Some(map) = ValueKind::similar_mapping(name.clone()) {
                            format!("A similar type exists which might be what you need: '{}'", map)
                        } else {
                            format!("Are you sure you spelt '{}' correctly?", name)
                        }
                    ))
                )
            }

            self.parser.advance();

            if self.parser.generic_keys.contains(&name) {
                name = self
                    .parser
                    .external_generics
                    .get(
                        self.parser
                            .generic_keys
                            .iter()
                            .position(|item| item == &name)
                            .unwrap(),
                    )
                    .unwrap()
                    .id()
            }

            let identifier = self.parser.get_identifier();
            name = format!("{}.{}", name, identifier);
            self.parser.advance();
        }

        self.parser.expect_tokens(vec![TokenKind::LeftParenthesis]);
        self.parser.advance();

        let mut arguments = vec![];
        let mut variadic = false;
        let mut manual = false;

        let ty_name = self
            .parser
            .current_token()
            .value
            .get_string_inner()
            .unwrap_or("".into());

        if self.parser.current_token().kind == TokenKind::Identifier
            && (self.parser.current_token().value.is_base_type()
                || self.parser.struct_pool.contains(&ty_name)
                || self.parser.generic_keys.contains(&ty_name))
        {
            while self.parser.current_token().kind != TokenKind::RightParenthesis {
                if self.parser.current_token().kind == TokenKind::Ellipsis {
                    self.parser.advance();
                    variadic = true;
                    break;
                }

                let r#type = self.parser.get_type();

                self.parser.advance();

                let name = match self.parser.current_token().kind {
                    TokenKind::Identifier => self.parser.get_identifier(),
                    TokenKind::ExactLiteral => self.parser.get(TokenKind::ExactLiteral),
                    other => panic!(
                        "{}",
                        self.parser
                            .current_token()
                            .location
                            .error(format!("Invalid token type: {:?}", other))
                    ),
                };

                self.parser.advance();
                self.parser.match_token(TokenKind::Comma, true);

                arguments.push(Argument { r#type, name })
            }
        }

        self.parser.expect_tokens(vec![TokenKind::RightParenthesis]);
        self.parser.advance();

        let mut r#return = None;
        let mut unaliased = None;
        let mut volatile = false;

        if self.parser.match_token(TokenKind::Attribute, false) {
            while self.parser.current_token().kind == TokenKind::Attribute {
                self.parser.advance();
                let location = self.parser.current_token().location.clone();
                let attribute = self.parser.current_token().parse_attribute();

                match attribute {
                    Attribute::Alias => {
                        self.parser.advance();
                        self.parser.expect_tokens(vec![TokenKind::LeftParenthesis]);
                        self.parser.advance();
                        self.parser.expect_tokens(vec![TokenKind::StringLiteral]);

                        let alias = self
                            .parser
                            .current_token()
                            .value
                            .get_string_inner()
                            .unwrap();

                        if external {
                            unaliased = Some(name);
                            name = alias.replace("::", ".");
                        } else {
                            if self.parser.warnings.has_warning(Warning::InvalidAlias) {
                                println!(
                                    "{}",
                                    location.warning(format!(
                                        "Can't assign aliases to non-external functions\nSkipping alias '{}' for function '{}'",
                                        alias, name.replace(".", "::")
                                    ))
                                )
                            }
                        }

                        self.parser.advance();
                        self.parser.expect_tokens(vec![TokenKind::RightParenthesis]);
                        self.parser.advance();
                    }
                    Attribute::Volatile => {
                        volatile = true;
                        self.parser.advance();
                    }
                }
            }
        }

        let mut location = self.parser.current_token().location.clone();

        if self.parser.match_token(TokenKind::RightArrow, true) {
            location = self.parser.current_token().location.clone();
            r#return = Some(self.parser.get_type());
            self.parser.advance();
        }

        if external {
            self.parser.expect_tokens(vec![TokenKind::Semicolon]);
            self.parser.advance();

            return Primitive::Function {
                public,
                variadic,
                manual,
                name,
                external,
                builtin: false,
                volatile: false,
                unaliased,
                arguments,
                r#return,
                body: vec![],
                usable: true,
                imported: false,
                location: self.parser.current_token().location,
            };
        }

        self.parser.expect_tokens(vec![TokenKind::LeftCurlyBrace]);

        let body: RefCell<Vec<AstNode>> = RefCell::new(vec![]);

        loop {
            self.parser.advance();

            let current = self.parser.current_token();

            match current.kind {
                TokenKind::RightCurlyBrace => {
                    self.parser.advance();
                    break;
                }
                _ => {
                    let (node, position, tokens) = Statement::new(
                        self.parser.tokens.clone(),
                        self.parser.position.clone(),
                        &body,
                        &Shared {
                            struct_pool: self.parser.struct_pool.clone(),
                            external_generics: &self.parser.external_generics,
                            generic_keys: &self.parser.generic_keys,
                            generic_defaults: &self.parser.generic_defaults,
                        },
                    )
                    .parse();

                    body.borrow_mut().push(node);
                    self.parser.position = position;
                    self.parser.tokens = tokens;
                }
            };
        }

        let mut res = body.borrow_mut().to_owned().clone();
        let mut deferred: Vec<AstNode> = vec![];

        res.retain(|node| match node.clone() {
            AstNode::DeferStatement { value, .. } => {
                deferred.push(*value.clone());
                false
            }
            _ => true,
        });

        fn insert_deferred_statements(
            nodes: &mut Vec<AstNode>,
            deferred: &Vec<AstNode>,
            root: bool,
        ) {
            let mut new_nodes = vec![];
            let mut found_return = false;

            for node in nodes.drain(..) {
                match node {
                    AstNode::ReturnStatement { .. } => {
                        new_nodes.extend(deferred.clone());
                        new_nodes.push(node);
                        found_return = true;
                    }
                    AstNode::WhileLoop {
                        condition,
                        step,
                        body,
                        location,
                    } => {
                        let mut new_body = body;
                        insert_deferred_statements(&mut new_body, deferred, false);

                        new_nodes.push(AstNode::WhileLoop {
                            condition,
                            step,
                            body: new_body,
                            location,
                        });
                    }
                    AstNode::BlockStatement { body, location } => {
                        let mut new_body = body;
                        insert_deferred_statements(&mut new_body, deferred, false);

                        new_nodes.push(AstNode::BlockStatement {
                            body: new_body,
                            location,
                        });
                    }
                    AstNode::IfStatement {
                        condition,
                        body,
                        else_body,
                        location,
                    } => {
                        let mut new_body = body;
                        let mut new_else_body = else_body;

                        insert_deferred_statements(&mut new_body, deferred, false);
                        insert_deferred_statements(&mut new_else_body, deferred, false);

                        new_nodes.push(AstNode::IfStatement {
                            condition,
                            body: new_body,
                            else_body: new_else_body,
                            location,
                        });
                    }
                    _ => new_nodes.push(node),
                }
            }

            if !found_return && root {
                new_nodes.extend(deferred.clone());
            }

            *nodes = new_nodes;
        }

        insert_deferred_statements(&mut res, &deferred, true);

        res.retain(|node| match node.clone() {
            AstNode::LiteralStatement { kind, value, .. } => match kind {
                TokenKind::ExactLiteral => match value {
                    ValueKind::String(val) => match val.as_str() {
                        "__MANUAL_RETURN__" => {
                            manual = true;
                            false
                        }
                        _ => true,
                    },
                    _ => true,
                },
                _ => true,
            },
            _ => true,
        });

        for (i, item) in self.parser.tree.iter().cloned().rev().enumerate() {
            match item {
                Primitive::Constant { name, r#type, .. } => {
                    res.insert(
                        0,
                        AstNode::DeclareStatement {
                            // Variable names in Elle cannot contain "."
                            // so this name is valid
                            name: format!("constant.{}", i),
                            r#type,
                            value: Box::new(AstNode::LiteralStatement {
                                kind: TokenKind::Identifier,
                                value: ValueKind::String(name),
                                location: self.parser.current_token().location,
                            }),
                            location: self.parser.current_token().location,
                        },
                    );
                }
                _ => {}
            }
        }

        Primitive::Function {
            public,
            variadic,
            manual,
            name,
            external,
            builtin: false,
            volatile,
            unaliased,
            arguments,
            r#return,
            body: res,
            usable: true,
            imported: false,
            location,
        }
    }
}
