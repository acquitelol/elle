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

    fn next_token(&mut self) -> Option<Token> {
        match self.is_eof() {
            true => None,
            false => Some(self.tokens[self.position + 1].clone()),
        }
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
        if self.tokens.len() - self.position == 1 {
            let current = self.current_token();

            AstNode::LiteralStatement {
                kind: current.kind,
                value: current.value,
            }
        } else {
            match self.next_token() {
                Some(token) => match token.kind {
                    TokenKind::Semicolon => {
                        let current = self.current_token();

                        self.advance();

                        AstNode::LiteralStatement {
                            kind: current.kind,
                            value: current.value,
                        }
                    }
                    _ => self.parse_arithmetic(),
                },
                None => self.parse_arithmetic(),
            }
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

        while self.current_token().kind != TokenKind::RightParenthesis && !self.is_eof() {
            let mut tokens = vec![];

            loop {
                tokens.push(self.current_token());
                self.advance();

                if self.current_token().kind == TokenKind::Comma {
                    self.advance();
                    break;
                }

                if self.current_token().kind == TokenKind::RightParenthesis || self.is_eof() {
                    break;
                }
            }

            parameters.push(Statement::new(tokens, 0).parse().0);
        }

        self.expect_token(TokenKind::RightParenthesis);
        self.advance();

        AstNode::FunctionCall { name, parameters }
    }

    fn find_highest_precedence(&mut self) -> usize {
        let tokens = self.tokens.clone();
        let mut precedence = 0;
        let mut precedence_index = 0;
        let mut index = self.position.clone();

        loop {
            index += 1;

            if index >= tokens.len() - 1 {
                break;
            }

            let token = tokens[index].clone();

            if token.kind.precedence() > precedence {
                precedence_index = index;
                precedence = token.kind.precedence();
            }
        }

        precedence_index
    }

    fn parse_arithmetic(&mut self) -> AstNode {
        let position = self.find_highest_precedence();
        let operator = self.tokens[position].clone().kind;

        dbg!(position, &operator, &self.tokens);

        let tokens = self.tokens.clone();
        let left = tokens[self.position..=position - 1].to_vec();
        let mut raw_right = tokens[position..=tokens.len() - 1].to_vec();

        self.position += left.len();
        self.position += raw_right.len() - 1;

        raw_right.remove(0); // Get rid of the operator

        let right = if let Some(index) = raw_right
            .iter()
            .position(|token| token.kind == TokenKind::Semicolon)
        {
            raw_right[..index].to_vec()
        } else {
            raw_right
        };

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

                    if next.kind.is_arithmetic() {
                        self.parse_arithmetic()
                    } else {
                        match next.kind {
                            TokenKind::LeftParenthesis => self.parse_function(),
                            _ => todo!(),
                        }
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
