use std::cell::RefCell;

use crate::lexer::enums::{TokenKind, ValueKind};

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

        let name = self.parser.get_identifier();

        self.parser.advance();
        self.parser.expect_token(TokenKind::LeftParenthesis);
        self.parser.advance();

        let mut arguments = vec![];
        let mut variadic = false;
        let mut manual = false;

        if self.parser.match_token(TokenKind::Type, false) {
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
                        "[{}] Invalid token type {:?}",
                        self.parser.current_token().location.display(),
                        other
                    ),
                };

                self.parser.advance();
                self.parser.match_token(TokenKind::Comma, true);

                arguments.push(Argument { r#type, name })
            }
        }

        self.parser.expect_token(TokenKind::RightParenthesis);
        self.parser.advance();

        let mut r#return = None;

        if self.parser.match_token(TokenKind::RightArrow, true) {
            r#return = Some(self.parser.get_type());
            self.parser.advance();
        }

        if external {
            self.parser.expect_token(TokenKind::Semicolon);
            self.parser.advance();

            return Primitive::Function {
                public,
                variadic,
                manual,
                name,
                external,
                arguments,
                r#return,
                body: vec![],
            };
        }

        self.parser.expect_token(TokenKind::LeftCurlyBrace);

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
                    )
                    .parse();

                    let mut body_ref = body.borrow_mut();
                    let res = body_ref.iter().position(|item| match item.clone() {
                        AstNode::LiteralStatement { kind, value } => {
                            if kind.clone() == TokenKind::ExactLiteral {
                                match value.clone() {
                                    ValueKind::String(val) => {
                                        if val == "__<#insert#>__".to_owned() {
                                            true
                                        } else {
                                            false
                                        }
                                    }
                                    _ => false,
                                }
                            } else {
                                false
                            }
                        }
                        _ => false,
                    });

                    if res.is_some() {
                        match node {
                            AstNode::DeclareStatement {
                                name: _,
                                r#type: _,
                                value: _,
                            } => body_ref[res.unwrap()] = node,
                            _ => body_ref.push(node),
                        }
                    } else {
                        body_ref.push(node);
                    }

                    self.parser.position = position;
                    self.parser.tokens = tokens;
                }
            };
        }

        let mut res = body.borrow_mut().to_owned().clone();
        let mut deferred: Vec<AstNode> = Vec::new();

        res.retain(|node| match node.clone() {
            AstNode::DeferStatement { value } => {
                deferred.push(*value.clone());
                false
            }
            AstNode::LiteralStatement { kind, value } => {
                if kind.clone() == TokenKind::ExactLiteral {
                    match value.clone() {
                        ValueKind::String(val) => {
                            if val == "__<#insert#>__".to_owned() {
                                false
                            } else {
                                true
                            }
                        }
                        _ => true,
                    }
                } else {
                    true
                }
            }
            _ => true,
        });

        fn insert_deferred_statements(
            nodes: &mut Vec<AstNode>,
            deferred: &Vec<AstNode>,
            root: bool,
        ) {
            let mut new_nodes = Vec::new();
            let mut found_return = false;

            for node in nodes.drain(..) {
                match node {
                    AstNode::ReturnStatement { .. } => {
                        new_nodes.extend(deferred.clone());
                        new_nodes.push(node);
                        found_return = true;
                    }
                    AstNode::WhileLoop {
                        condition, body, ..
                    } => {
                        let mut new_body = body;
                        insert_deferred_statements(&mut new_body, deferred, false);
                        new_nodes.push(AstNode::WhileLoop {
                            condition,
                            body: new_body,
                        });
                    }
                    AstNode::BlockStatement { body, .. } => {
                        let mut new_body = body;
                        insert_deferred_statements(&mut new_body, deferred, false);
                        new_nodes.push(AstNode::BlockStatement { body: new_body });
                    }
                    AstNode::IfStatement {
                        condition,
                        body,
                        else_body,
                        ..
                    } => {
                        let mut new_body = body;
                        let mut new_else_body = else_body;

                        insert_deferred_statements(&mut new_body, deferred, false);
                        insert_deferred_statements(&mut new_else_body, deferred, false);

                        new_nodes.push(AstNode::IfStatement {
                            condition,
                            body: new_body,
                            else_body: new_else_body,
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
            AstNode::LiteralStatement { kind, value } => match kind {
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

        // Initialize pointer constants on every function call
        for (i, item) in self.parser.tree.iter().cloned().rev().enumerate() {
            match item {
                Primitive::Constant { name, r#type, .. } => {
                    if r#type.is_some() && r#type.clone().unwrap().is_pointer() {
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
                                }),
                            },
                        );
                    }
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
            arguments,
            r#return,
            body: res,
        }
    }
}
