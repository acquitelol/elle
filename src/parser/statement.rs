use std::cell::RefCell;

use crate::lexer::enums::{Token, TokenKind, ValueKind};
use super::enums::AstNode;

pub struct Statement<'a> {
    tokens: Vec<Token>,
    position: usize,
    body: &'a RefCell<Vec<AstNode>>,
}

impl<'a> Statement<'a> {
    pub fn new(tokens: Vec<Token>, position: usize, body: &'a RefCell<Vec<AstNode>>) -> Self {
        Statement {
            tokens,
            position,
            body,
        }
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

    fn expect_token_with_message(&self, expected: TokenKind, message: Option<&str>) {
        if self.current_token().kind != expected {
            panic!(
                "[{}] Expected {:?}, found {:?}. {}",
                self.current_token().location.display(),
                expected,
                self.current_token().kind,
                message.unwrap_or("")
            );
        }
    }

    fn expect_token(&self, expected: TokenKind) {
        self.expect_token_with_message(expected, None);
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

    fn parse_declare(&mut self, ty: Option<String>) -> AstNode {
        let r#type = if ty.is_some() { ty.unwrap() } else {
            let tmp = self.get_type();
            self.advance();

            tmp
        };

        let name = self.get_identifier();

        self.advance();

        if self.current_token().kind == TokenKind::LeftBlockBrace {
            return self.parse_buffer(Some(name), Some(r#type));
        }

        self.expect_token(TokenKind::Equal);
        self.advance();

        let mut value = vec![];

        while self.current_token().kind != TokenKind::Semicolon && !self.is_eof() {
            value.push(self.current_token());
            self.advance();
        }

        if !self.is_eof() {
            self.expect_token(TokenKind::Semicolon);
        }

        let res = Statement::new(value, 0, &self.body).parse().0;

        let parsed_res = match res.clone() {
            AstNode::DeclareStatement {
                name,
                r#type: _,
                value: _,
            } => {
                self.body.borrow_mut().push(res);

                AstNode::LiteralStatement {
                    kind: TokenKind::Identifier,
                    value: ValueKind::String(name),
                }
            }
            _ => res,
        };

        AstNode::DeclareStatement {
            name,
            r#type,
            value: Box::new(parsed_res),
        }
    }

    fn parse_declarative_like(&mut self) -> AstNode {
        let name = self.get_identifier();

        self.advance();
        let operation = self.current_token();
        self.advance();

        let res = match operation.kind {
            TokenKind::AddOne => Some(TokenKind::Add),
            TokenKind::SubtractOne => Some(TokenKind::Subtract),
            _ => None,
        };

        if res.is_some() {
            if !self.is_eof() {
                self.expect_token(TokenKind::Semicolon);
            }

            let mapping = res.unwrap();

            self.body.borrow_mut().push(AstNode::LiteralStatement { 
                kind: TokenKind::ExactLiteral, 
                value: ValueKind::String("__<#insert#>__".to_owned()) 
            });

            self.body.borrow_mut().push(AstNode::DeclareStatement {
                name: name.clone(),
                r#type: "Nil".to_owned(),
                value: Box::new(AstNode::ArithmeticOperation {
                    left: Box::new(AstNode::LiteralStatement {
                        kind: TokenKind::Identifier,
                        value: ValueKind::String(name.clone()),
                    }),
                    right: Box::new(AstNode::LiteralStatement {
                        kind: TokenKind::IntegerLiteral,
                        value: ValueKind::Number(1),
                    }),
                    operator: mapping,
                }),
            });
    

            return AstNode::LiteralStatement {
                kind: TokenKind::Identifier,
                value: ValueKind::String(name.clone()),
            }
        }

        let mut value = vec![];

        if self.is_eof() {
            value.push(self.current_token());
        } else {
            while self.current_token().kind != TokenKind::Semicolon && !self.is_eof() {
                value.push(self.current_token());
                self.advance();
            }
        }

        let mapping = match operation.kind {
            TokenKind::AddEqual => TokenKind::Add,
            TokenKind::SubtractEqual => TokenKind::Subtract,
            TokenKind::MultiplyEqual => TokenKind::Multiply,
            TokenKind::DivideEqual => TokenKind::Divide,
            TokenKind::ModulusEqual => TokenKind::Modulus,
            other => panic!("Invalid identifier operation {:?}", other),
        };

        AstNode::DeclareStatement {
            name: name.clone(),
            r#type: "Nil".to_owned(),
            value: Box::new(AstNode::ArithmeticOperation {
                left: Box::new(AstNode::LiteralStatement {
                    kind: TokenKind::Identifier,
                    value: ValueKind::String(name),
                }),
                right: Box::new(Statement::new(value, 0, &self.body).parse().0),
                operator: mapping,
            }),
        }
    }

    fn parse_literal(&mut self) -> AstNode {
        if self.tokens.len() - self.position == 1 {
            let current = self.current_token();

            match current.kind {
                TokenKind::TrueLiteral => {
                    AstNode::LiteralStatement {
                        kind: TokenKind::IntegerLiteral,
                        value: ValueKind::Number(1),
                    }
                },
                TokenKind::FalseLiteral => {
                    AstNode::LiteralStatement {
                        kind: TokenKind::IntegerLiteral,
                        value: ValueKind::Number(0),
                    }
                }
                _ => {
                    AstNode::LiteralStatement {
                        kind: current.kind,
                        value: current.value,
                    }
                }
            }
        } else {
            match self.next_token() {
                Some(token) => match token.kind {
                    TokenKind::Semicolon => {
                        let current = self.current_token();

                        self.advance();

                        match current.kind {
                            TokenKind::TrueLiteral => {
                                AstNode::LiteralStatement {
                                    kind: TokenKind::IntegerLiteral,
                                    value: ValueKind::Number(1),
                                }
                            },
                            TokenKind::FalseLiteral => {
                                AstNode::LiteralStatement {
                                    kind: TokenKind::IntegerLiteral,
                                    value: ValueKind::Number(0),
                                }
                            }
                            _ => {
                                AstNode::LiteralStatement {
                                    kind: current.kind,
                                    value: current.value,
                                }
                            }
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

        if !self.is_eof() {
            self.expect_token(TokenKind::Semicolon);
        }

        let res = if value.len() > 0 {
            Statement::new(value, 0, &self.body).parse().0
        } else {
            AstNode::LiteralStatement { kind: TokenKind::IntegerLiteral, value: ValueKind::Number(0) }
        };

        let parsed_res = match res.clone() {
            AstNode::DeclareStatement {
                name,
                r#type: _,
                value: _,
            } => {
                self.body.borrow_mut().push(res);

                AstNode::LiteralStatement {
                    kind: TokenKind::Identifier,
                    value: ValueKind::String(name),
                }
            }
            _ => res,
        };

        AstNode::ReturnStatement {
            value: Box::new(parsed_res),
        }
    }

    fn parse_function(&mut self, variadic: bool) -> AstNode {
        let name = self.get_identifier();
        let mut variadic_index: usize = 1;

        self.advance();
        match self.current_token().kind {
            TokenKind::LeftParenthesis => {
                self.advance();
            }
            TokenKind::Not => {
                self.advance();

                if self.current_token().kind == TokenKind::IntegerLiteral {
                    match self.current_token().value {
                        ValueKind::Number(val) => {
                            variadic_index = val.to_string().parse::<usize>().unwrap();
                        },
                        _ => {}
                    }

                    self.advance();
                }

                self.expect_token(TokenKind::LeftParenthesis);
                self.advance();
            }
            other => panic!("Expected left parethesis or exclamation mark (for variadic functions) but got {:?}", other)
        }

        let mut parameters = vec![];

        while self.current_token().kind != TokenKind::RightParenthesis && !self.is_eof() {
            let mut tokens = vec![];
            let mut nesting = 0;

            loop {
                if self.current_token().kind == TokenKind::LeftParenthesis {
                    nesting += 1;
                }

                tokens.push(self.current_token());
                self.advance();

                if self.current_token().kind == TokenKind::Comma {
                    if nesting > 0 {
                        // Comma in an inner function should just be added to the
                        // token list to be parsed
                        tokens.push(self.current_token());
                        self.advance();
                        continue;
                    } else {
                        // Continue to the next parameter in the outer function
                        self.advance();
                        break;
                    }
                }

                if self.current_token().kind == TokenKind::RightParenthesis {
                    if nesting > 0 {
                        nesting -= 1;
                    } else {
                        break; // The function call has ended
                    }
                }

                if self.is_eof() {
                    break;
                }
            }

            parameters.push(Statement::new(tokens.clone(), 0, &self.body).parse().0);
        }

        self.expect_token_with_message(
            TokenKind::RightParenthesis,
            Some("Perhaps you forgot to close a nested expression?"),
        );

        self.advance();

        if variadic {
            parameters.insert(
                variadic_index,
                AstNode::LiteralStatement {
                    kind: TokenKind::ExactLiteral,
                    value: ValueKind::String("...".to_owned()),
                },
            )
        }

        AstNode::FunctionCall { name, parameters }
    }

    fn find_lowest_precedence(&mut self) -> usize {
        let tokens = self.tokens.clone();
        let mut precedence = TokenKind::highest_precedence();
        let mut precedence_index = 0;
        let mut nesting = 0;
        let mut index = self.position.clone();

        loop {
            index += 1;

            if index >= tokens.len() - 1 {
                break;
            }

            let token = tokens[index].clone();

            match token.kind {
                TokenKind::LeftParenthesis => {
                    nesting += 1;
                }
                TokenKind::RightParenthesis => {
                    nesting -= 1;
                }
                _ => {}
            }

            // Set the precedence to the last lowest precedence found.
            // If the expression is 1 + 2 * 3 + 4 * 5 for example,
            // it'll return the position of the second '+' token
            if token.kind.is_arithmetic() 
                && token.kind.precedence() <= precedence
                && nesting == 0 {
                precedence_index = index;
                precedence = token.kind.precedence();
            }
        }

        precedence_index
    }

    fn parse_arithmetic(&mut self) -> AstNode {
        let position = self.find_lowest_precedence();
        let operator = self.tokens[position].clone().kind;

        let tokens = self.tokens.clone();
        let left =
            tokens[self.position..=if position > 0 { position - 1 } else { position }].to_vec();
        let mut raw_right = tokens[position..=tokens.len() - 1].to_vec();

        raw_right.remove(0); // Get rid of the operator

        // Shift the position across the size of the expression
        self.position += left.len();
        self.position += raw_right.len();

        let right = if let Some(index) = raw_right
            .iter()
            .position(|token| token.kind == TokenKind::Semicolon)
        {
            raw_right[..index].to_vec()
        } else {
            raw_right
        };

        AstNode::ArithmeticOperation {
            left: Box::new(Statement::new(left, 0, &self.body).parse().0),
            right: Box::new(Statement::new(right, 0, &self.body).parse().0),
            operator,
        }
    }

    fn parse_expression(&mut self) -> AstNode {
        let mut node = self.parse_primary();

        while self.current_token().kind.is_arithmetic() {
            let operator = self.current_token().kind;

            self.advance();

            let right = self.parse_primary();

            node = AstNode::ArithmeticOperation {
                left: Box::new(node),
                right: Box::new(right),
                operator,
            };
        }

        node
    }

    fn parse_prefix_increment(&mut self) -> AstNode {
        let mapping = match self.current_token().kind {
            TokenKind::AddOne => TokenKind::Add,
            TokenKind::SubtractOne => TokenKind::Subtract,
            other => panic!("Expected increment token (++ or --) but got {:?}", other),
        };

        self.advance();
        self.expect_token_with_message(
            TokenKind::Identifier, 
            Some(
                format!(
                    "Expected identifier, got {:?}. You cannot increment anything that isn't an identifier.", 
                    self.current_token().kind
                ).as_str()
            )
        );

        let name = self.get_identifier();
        self.advance();

        AstNode::DeclareStatement {
            name: name.clone(),
            r#type: "Nil".to_owned(),
            value: Box::new(AstNode::ArithmeticOperation {
                left: Box::new(AstNode::LiteralStatement {
                    kind: TokenKind::Identifier,
                    value: ValueKind::String(name.clone()),
                }),
                right: Box::new(AstNode::LiteralStatement {
                    kind: TokenKind::IntegerLiteral,
                    value: ValueKind::Number(1),
                }),
                operator: mapping,
            }),
        }
    }

    fn parse_buffer(&mut self, name: Option<String>, ty: Option<String>) -> AstNode {
        let name = if name.is_some() {
            name.unwrap()
        } else {
            let tmp = self.get_identifier();
            self.advance();

            tmp
        };

        self.expect_token(TokenKind::LeftBlockBrace);
        self.advance();

        self.expect_token(TokenKind::IntegerLiteral);
        let size = self.current_token().value;

        // These should not be assigned to a variable as they are themselves a variable declaration
        // Therefore we don't need to check is_eof and can just enforce that there's a semicolon
        self.advance();
        self.expect_token(TokenKind::RightBlockBrace);
        self.advance();
        self.expect_token(TokenKind::Semicolon);

        AstNode::BufferStatement { name, r#type: ty.unwrap_or("Byte".to_string()), size }
    }

    fn parse_store(&mut self) -> AstNode {
        let name = self.get_identifier();
        self.advance();

        self.expect_token(TokenKind::LeftArrow);
        self.advance();

        let r#type = self.get_type();
        self.advance();

        let mut value = vec![];

        while self.current_token().kind != TokenKind::Semicolon && !self.is_eof() {
            value.push(self.current_token());
            self.advance();
        }

        if !self.is_eof() {
            self.expect_token(TokenKind::Semicolon);
        }

        AstNode::StoreStatement {
            name,
            r#type,
            value: Box::new(Statement::new(value, 0, &self.body).parse().0),
        }
    }

    fn parse_if_statement(&mut self) -> AstNode {
        self.advance();

        self.expect_token(TokenKind::LeftParenthesis);
        self.advance();

        let mut tokens = vec![];
        let mut nesting = 0;

        loop {
            if self.current_token().kind == TokenKind::LeftParenthesis {
                nesting += 1;
            }

            tokens.push(self.current_token());
            self.advance();

            if self.current_token().kind == TokenKind::RightParenthesis {
                if nesting > 0 {
                    nesting -= 1;
                } else {
                    break;
                }
            }

            if self.is_eof() {
                break;
            }
        }

        let expression = Statement::new(tokens, 0, &self.body).parse().0;

        self.expect_token(TokenKind::RightParenthesis);
        self.advance();

        self.expect_token(TokenKind::LeftCurlyBrace);
        self.advance();

        let body = self.yield_block();
        let mut else_body: Vec<AstNode> = vec![];

        if self.current_token().kind == TokenKind::Else {
            self.advance();
            self.expect_token(TokenKind::LeftCurlyBrace);
            self.advance();

            else_body = self.yield_block();
        }

        self.position -= 1;

        AstNode::IfStatement { condition: Box::new(expression), body, else_body }
    }

    fn parse_while_statement(&mut self) -> AstNode {
        self.advance();

        self.expect_token(TokenKind::LeftParenthesis);
        self.advance();

        let mut tokens = vec![];
        let mut nesting = 0;

        loop {
            if self.current_token().kind == TokenKind::LeftParenthesis {
                nesting += 1;
            }

            tokens.push(self.current_token());
            self.advance();

            if self.current_token().kind == TokenKind::RightParenthesis {
                if nesting > 0 {
                    nesting -= 1;
                } else {
                    break;
                }
            }

            if self.is_eof() {
                break;
            }
        }

        let expression = Statement::new(tokens, 0, &self.body).parse().0;

        self.expect_token(TokenKind::RightParenthesis);
        self.advance();

        self.expect_token(TokenKind::LeftCurlyBrace);
        self.advance();

        let body = self.yield_block();

        self.position -= 1;

        AstNode::WhileLoop { condition: Box::new(expression), body }
    }

    fn yield_block(&mut self) -> Vec<AstNode> { 
        let cell: RefCell<Vec<AstNode>> = RefCell::new(vec![]);

        loop {
            let current = self.current_token();

            match current.kind {
                TokenKind::RightCurlyBrace => {
                    self.advance();
                    break;
                }
                _ => {
                    let (node, position) = Statement::new(
                        self.tokens.clone(),
                        self.position.clone(),
                        &cell,
                    )
                    .parse();

                    let mut body_ref = cell.borrow_mut();
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
                                    _ => false
                                }
                            } else {
                                false
                            }
                        }
                        _ => false
                    });

                    if res.is_some() {
                        match node {
                            AstNode::DeclareStatement { name: _, r#type: _, value: _ } => {
                                body_ref[res.unwrap()] = node
                            }
                            _ => {
                                body_ref.remove(res.unwrap());
                                body_ref.push(node);
                            }
                        }
                    } else {
                        body_ref.push(node);
                    }

                    self.position = position;
                }
            };

            self.advance();
        }

        let body = cell.borrow_mut().to_owned().clone();

        body
    }

    fn parse_primary(&mut self) -> AstNode {
        match self.current_token().kind {
            token if token.is_literal() => self.parse_literal(),
            TokenKind::Identifier => {
                if self.is_eof() {
                    self.parse_literal()
                } else {
                    let next = self.tokens[self.position + 1].clone();

                    if next.kind == TokenKind::LeftParenthesis {
                        self.parse_function(false)
                    } else if next.kind == TokenKind::LeftBlockBrace {
                        self.parse_buffer(None, None)
                    } else if next.kind == TokenKind::Not {
                        if self.position + 2 > self.tokens.len() - 1 {
                            panic!("EOF but specified a variadic identifier")
                        }

                        self.parse_function(true)
                    } else if next.kind == TokenKind::Equal {
                        self.parse_declare(Some("Nil".to_owned()))
                    } else if next.kind == TokenKind::LeftArrow {
                        self.parse_store()
                    } else if next.kind.is_declarative() {
                        self.parse_declarative_like()
                    } else if next.kind.is_arithmetic() {
                        self.parse_arithmetic()
                    } else {
                        panic!(
                            "[{:?}] Expected left parenthesis or arithmetic, got {:?}",
                            self.current_token().location.display(),
                            self.current_token().kind
                        );
                    }
                }
            }
            other if other.is_one_operator() => self.parse_prefix_increment(),
            _ => panic!(
                "[{:?}] Expected expression, got {:?}",
                self.current_token().location.display(),
                self.current_token().kind,
            ),
        }
    }

    pub fn parse(&mut self) -> (AstNode, usize) {
        let node = match self.current_token().kind {
            TokenKind::Type => self.parse_declare(None),
            TokenKind::Return => self.parse_return(),
            TokenKind::If => self.parse_if_statement(),
            TokenKind::While => self.parse_while_statement(),
            _ => self.parse_expression(),
        };

        (node, self.position)
    }
}
