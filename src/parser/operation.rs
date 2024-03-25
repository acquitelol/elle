use crate::lexer::enums::TokenKind;

use super::{
    enums::{Argument, AstNode, Primitive},
    parser::Parser,
    statement::Statement,
};

pub struct Operation<'a> {
    parser: &'a mut Parser,
}

impl<'a> Operation<'a> {
    pub fn new(parser: &'a mut Parser) -> Self {
        Operation { parser }
    }

    pub fn parse(&mut self, public: bool) -> Primitive {
        self.parser.advance();

        let name = self.parser.get_identifier();

        self.parser.advance();
        self.parser.expect_token(TokenKind::LeftParenthesis);
        self.parser.advance();

        let mut arguments = vec![];

        if self.parser.match_token(TokenKind::Type, false) {
            while self.parser.current_token().kind != TokenKind::RightParenthesis {
                let r#type = self.parser.get_type();

                self.parser.advance();

                let name = self.parser.get_identifier();

                self.parser.advance();
                self.parser.match_token(TokenKind::Comma, true);

                arguments.push(Argument { r#type, name })
            }
        }

        self.parser.expect_token(TokenKind::RightParenthesis);
        self.parser.advance();

        let mut r#return: String = "Nil".to_owned();

        if self.parser.match_token(TokenKind::Arrow, true) {
            r#return = self.parser.get_type();
            self.parser.advance();
        }

        self.parser.expect_token(TokenKind::LeftCurlyBrace);

        let mut body: Vec<AstNode> = vec![];

        loop {
            self.parser.advance();

            let current = self.parser.current_token();

            match current.kind {
                TokenKind::RightCurlyBrace => {
                    self.parser.advance();
                    break;
                }
                _ => {
                    let (node, position) =
                        Statement::new(self.parser.tokens.clone(), self.parser.position.clone())
                            .parse();

                    body.push(node);
                    self.parser.position = position;
                }
            };
        }

        Primitive::Operation {
            public,
            name,
            arguments,
            r#return,
            body,
        }
    }
}
