use std::cell::RefCell;

use crate::lexer::enums::{Token, TokenKind};

use super::{
    enums::{AstNode, Primitive},
    parser::Parser,
    statement::Statement,
};

pub struct Constant<'a> {
    parser: &'a mut Parser,
}

impl<'a> Constant<'a> {
    pub fn new(parser: &'a mut Parser) -> Self {
        Constant { parser }
    }

    fn yield_tokens_with_delimiters(&mut self, delimiters: Vec<TokenKind>) -> Vec<Token> {
        let mut tokens = vec![];

        if delimiters.contains(&self.parser.current_token().kind) {
            panic!(
                "[{}] expected expression but got {:?}",
                self.parser.current_token().location.display(),
                self.parser.current_token().kind
            );
        }

        loop {
            tokens.push(self.parser.current_token());
            let res = self.parser.advance_opt();

            if delimiters.contains(&self.parser.current_token().kind) {
                break;
            }

            if self.parser.is_eof() {
                if res.is_some() {
                    tokens.push(self.parser.current_token());
                }

                break;
            }
        }

        tokens
    }

    pub fn parse(&mut self, public: bool, _: bool) -> Primitive {
        self.parser.advance();

        let ty = self.parser.get_type();
        self.parser.advance();

        let name = self.parser.get_identifier();
        self.parser.advance();

        self.parser.expect_token(TokenKind::Equal);
        self.parser.advance();

        let tokens = self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon]);
        self.parser.advance();

        let body: RefCell<Vec<AstNode>> = RefCell::new(vec![]);
        let value = Statement::new(tokens, 0, &body).parse().0;

        Primitive::Constant {
            name,
            public,
            r#type: Some(ty.clone()),
            value: Box::new(AstNode::ConversionStatement {
                r#type: Some(ty),
                value: Box::new(value),
            }),
        }
    }
}
