use crate::lexer::enums::{TokenKind, ValueKind};

use super::{enums::Primitive, parser::Parser};

pub struct Use<'a> {
    parser: &'a mut Parser,
}

impl<'a> Use<'a> {
    pub fn new(parser: &'a mut Parser) -> Self {
        Use { parser }
    }

    fn get_string(&self) -> String {
        match self.parser.current_token().value {
            ValueKind::String(val) => val,
            _ => panic!(
                "{}",
                self.parser
                    .current_token()
                    .location
                    .error("Token is not a string".to_string())
            ),
        }
    }

    pub fn parse(&mut self) -> Primitive {
        self.parser.advance();
        let mut module = self.get_string();
        let location = self.parser.current_token().location;
        self.parser.advance();

        // Keep merging until the next token isn't a "/" anymore
        while self.parser.current_token().kind == TokenKind::Divide {
            self.parser.advance();
            module.push_str(&format!("/{}", self.get_string()));
            self.parser.advance();
        }

        let mut functions = vec![];

        if self.parser.match_token(TokenKind::AtMark, true) {
            self.parser.expect_tokens(vec![TokenKind::LeftCurlyBrace]);
            self.parser.advance();

            while self.parser.current_token().kind != TokenKind::RightCurlyBrace {
                self.parser.expect_tokens(vec![TokenKind::Identifier]);

                let function = self.parser.get_identifier();
                functions.push(function);
                self.parser.advance();

                if self.parser.current_token().kind == TokenKind::Comma {
                    self.parser.advance();
                    continue;
                }
            }

            self.parser.expect_tokens(vec![TokenKind::RightCurlyBrace]);
            self.parser.advance();
        }

        self.parser.expect_tokens(vec![TokenKind::Semicolon]);
        self.parser.advance();

        Primitive::Use {
            module,
            functions,
            location,
        }
    }
}
