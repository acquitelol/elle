use crate::lexer::enums::{Token, TokenKind, ValueKind};

use super::enums::AstNode;

pub struct Statement {
    tokens: Vec<Token>,
    position: usize,
}

impl Statement {
    pub fn new(tokens: Vec<Token>, position: usize) -> Self {
        Statement { tokens, position }
    }

    pub fn advance(&mut self) {
        if self.is_eof() {
            println!("The position of {:?} is the last index of the token stack. Staying at the same position.", self.position);
        } else {
            self.position += 1;
        }
    }

    fn current_token(&self) -> Token {
        self.tokens[self.position].clone()
    }

    fn is_eof(&mut self) -> bool {
        self.position + 1 >= self.tokens.len()
    }

    fn expect_token(&self, expected: TokenKind) {
        if self.current_token().kind != expected {
            panic!(
                "[{}] Expected {:?}, found {:?}",
                self.current_token().location.display(),
                expected,
                self.current_token().kind
            );
        }
    }

    fn get(&mut self, expected: TokenKind) -> String {
        self.expect_token(expected.clone());

        let identifier = if let Token {
            value: ValueKind::String(identifier),
            ..
        } = self.current_token()
        {
            identifier.clone()
        } else {
            panic!(
                "Expected {:?}, got {:?}",
                expected.clone(),
                self.current_token()
            );
        };

        identifier
    }

    pub fn get_identifier(&mut self) -> String {
        self.get(TokenKind::Identifier)
    }

    pub fn get_type(&mut self) -> String {
        self.get(TokenKind::Type)
    }

    fn parse_declare(&mut self) -> AstNode {
        self.advance();

        let name = self.get_identifier();

        self.advance();

        self.expect_token(TokenKind::Colon);
        self.advance();

        let r#type = self.get_type();

        self.advance();
        self.expect_token(TokenKind::Equal);
        self.advance();

        let mut value = vec![];

        while self.current_token().kind != TokenKind::Semicolon && !self.is_eof() {
            value.push(self.current_token());
            self.advance();
        }

        self.expect_token(TokenKind::Semicolon);

        AstNode::DeclareStatement {
            name,
            r#type,
            value: Box::new(Statement::new(value, 0).parse().0),
        }
    }

    fn parse_literal(&mut self) -> AstNode {
        if self.tokens.len() < 2 {
            let current = self.current_token();

            AstNode::LiteralStatement {
                kind: current.kind,
                value: current.value,
            }
        } else {
            self.parse_arithmetic()
        }
    }

    fn parse_return(&mut self) -> AstNode {
        self.advance();

        let mut value = vec![];

        while self.current_token().kind != TokenKind::Semicolon && !self.is_eof() {
            value.push(self.current_token());
            self.advance();
        }

        self.expect_token(TokenKind::Semicolon);

        AstNode::ReturnStatement {
            value: Box::new(Statement::new(value, 0).parse().0),
        }
    }

    fn parse_function(&mut self) -> AstNode {
        let name = self.get_identifier();

        self.advance();
        self.expect_token(TokenKind::LeftParenthesis);
        self.advance();

        let mut parameters = vec![];

        while self.current_token().kind != TokenKind::RightParenthesis
            && self.current_token().kind != TokenKind::Semicolon
            && !self.is_eof()
        {
            let mut tokens = vec![];

            tokens.push(self.current_token());
            self.advance();

            while self.current_token().kind != TokenKind::Comma
                && self.current_token().kind != TokenKind::RightParenthesis
                && !self.is_eof()
            {
                tokens.push(self.current_token());
                self.advance();
            }

            parameters.push(Statement::new(tokens, 0).parse().0);
            self.advance();
        }

        self.expect_token(TokenKind::RightParenthesis);

        AstNode::FunctionCall { name, parameters }
    }

    fn parse_arithmetic(&mut self) -> AstNode {
        let mut left = vec![];

        while self.current_token().kind != TokenKind::ArithmeticOperation && !self.is_eof() {
            left.push(self.current_token());
            self.advance();
        }

        self.expect_token(TokenKind::ArithmeticOperation);

        let operator = if let Token {
            value: ValueKind::Character(operator),
            ..
        } = self.current_token()
        {
            operator.clone()
        } else {
            panic!(
                "Expected {:?}, got {:?}",
                TokenKind::ArithmeticOperation,
                self.current_token()
            );
        };

        self.advance();

        let mut right = vec![];

        while self.current_token().kind != TokenKind::Semicolon && !self.is_eof() {
            right.push(self.current_token());
            self.advance();
        }

        if self.current_token().kind != TokenKind::Semicolon {
            right.push(self.current_token());
        }

        AstNode::ArithmeticOperation {
            left: Box::new(Statement::new(left, 0).parse().0),
            right: Box::new(Statement::new(right, 0).parse().0),
            operator,
        }
    }

    pub fn parse(&mut self) -> (AstNode, usize) {
        let node = match self.current_token().kind {
            TokenKind::Declare => self.parse_declare(),
            TokenKind::Return => self.parse_return(),
            TokenKind::Identifier => {
                if self.is_eof() {
                    self.parse_literal()
                } else {
                    let next = self.tokens[self.position + 1].clone();

                    match next.kind {
                        TokenKind::LeftParenthesis => self.parse_function(),
                        TokenKind::ArithmeticOperation => self.parse_arithmetic(),
                        _ => todo!(),
                    }
                }
            }
            TokenKind::IntegerLiteral
            | TokenKind::StringLiteral
            | TokenKind::CharLiteral
            | TokenKind::InterpolatedLiteral => self.parse_literal(),
            _ => panic!(
                "[{:?}] Expected expression, got {:?}",
                self.current_token().location.display(),
                self.current_token().kind
            ),
        };

        (node, self.position)
    }
}
