use crate::lexer::enums::{Token, TokenKind, ValueKind};

use super::{enums::Primitive, parser::Parser};

pub struct Use<'a> {
    parser: &'a mut Parser,
}

impl<'a> Use<'a> {
    pub fn new(parser: &'a mut Parser) -> Self {
        Use { parser }
    }

    pub fn parse(&mut self) -> Primitive {
        self.parser.advance();

        let library = self.parser.get_identifier();

        self.parser.advance();

        self.parser.expect_token(TokenKind::Colon);
        self.parser.advance();

        let module = self.parser.get_identifier();

        self.parser.advance();

        let mut functions = vec![];

        if self.parser.match_token(TokenKind::AtMark, true) {
            self.parser.expect_token(TokenKind::LeftCurlyBrace);
            self.parser.advance();

            while self.parser.current_token().kind != TokenKind::RightCurlyBrace {
                self.parser.expect_token(TokenKind::Identifier);

                let function = self.parser.get_identifier();
                functions.push(function);
                self.parser.advance();

                if self.parser.current_token().kind == TokenKind::Comma {
                    self.parser.advance();
                    continue;
                }
            }

            self.parser.expect_token(TokenKind::RightCurlyBrace);
            self.parser.advance();
        }

        self.parser.expect_token(TokenKind::Semicolon);
        self.parser.advance();

        Primitive::Use {
            library,
            module,
            functions,
        }
    }
}
