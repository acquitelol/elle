use std::cell::RefCell;
use std::iter::FromIterator;

use super::enums::AstNode;
use crate::{
    compiler::enums::Type,
    lexer::enums::{Location, Token, TokenKind, ValueKind},
};

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

    pub fn advance_opt(&mut self) -> Option<()> {
        if self.is_eof() {
            None
        } else {
            self.position += 1;
            Some(())
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
            let upper = format!(
                "{}[{}]{}",
                "-".repeat(20),
                self.current_token().location.display(),
                "-".repeat(20)
            );

            panic!(
                "\n\n{}\nExpected {:?}, found {:?}. {}\n{}\n\n",
                upper,
                expected,
                self.current_token().kind,
                message.unwrap_or(""),
                "-".repeat(upper.len())
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

    pub fn get_type(&mut self) -> Type {
        let mut ty = ValueKind::String(self.get(TokenKind::Type))
            .to_type_string()
            .unwrap();

        loop {
            let tmp = self.next_token();

            if tmp.is_some() {
                match tmp.unwrap().kind {
                    TokenKind::Multiply => {
                        ty = Type::Pointer(Box::new(ty));
                        self.advance();
                    }
                    _ => break,
                }
            } else {
                break;
            }
        }

        ty
    }

    fn parse_declare(&mut self, ty: Option<Option<Type>>) -> AstNode {
        let r#type = if ty.is_some() {
            ty.unwrap()
        } else {
            let tmp = self.get_type();
            self.advance();

            Some(tmp)
        };

        let name = self.get_identifier();

        self.advance();

        if self.current_token().kind == TokenKind::LeftBlockBrace {
            return self.parse_buffer(Some(name), r#type);
        }

        if self.is_eof() || self.current_token().kind == TokenKind::Semicolon {
            return AstNode::DeclareStatement {
                name,
                r#type,
                value: Box::new(AstNode::LiteralStatement {
                    kind: TokenKind::IntegerLiteral,
                    value: ValueKind::Number(0),
                }),
            };
        }

        self.expect_token(TokenKind::Equal);
        self.advance();

        let tokens = self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon]);

        if !self.is_eof() {
            self.expect_token(TokenKind::Semicolon);
        }

        let res = Statement::new(tokens, 0, &self.body).parse().0;

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
                value: ValueKind::String("__<#insert#>__".to_owned()),
            });

            self.body.borrow_mut().push(AstNode::DeclareStatement {
                name: name.clone(),
                r#type: None,
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
            };
        }

        let tokens = self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon]);
        let mapping = match operation.kind {
            TokenKind::AddEqual => TokenKind::Add,
            TokenKind::SubtractEqual => TokenKind::Subtract,
            TokenKind::MultiplyEqual => TokenKind::Multiply,
            TokenKind::DivideEqual => TokenKind::Divide,
            TokenKind::ModulusEqual => TokenKind::Modulus,
            TokenKind::XorEqual => TokenKind::Xor,
            other => panic!("Invalid identifier operation {:?}", other),
        };

        AstNode::DeclareStatement {
            name: name.clone(),
            r#type: None,
            value: Box::new(AstNode::ArithmeticOperation {
                left: Box::new(AstNode::LiteralStatement {
                    kind: TokenKind::Identifier,
                    value: ValueKind::String(name),
                }),
                right: Box::new(Statement::new(tokens, 0, &self.body).parse().0),
                operator: mapping,
            }),
        }
    }

    fn parse_float(&mut self, token: Token) -> AstNode {
        let value = match token.value {
            ValueKind::String(val) => val,
            _ => todo!(),
        };

        if !value.contains(".") {
            panic!("Invalid float literal provided");
        }

        let nodes: Vec<&str> = value.split('.').collect();
        let left = nodes[0];
        let right = nodes[1];

        let exponent = right.len();
        let original = String::from_iter([left, right]).parse::<i64>().unwrap();

        AstNode::ArithmeticOperation {
            left: Box::new(AstNode::LiteralStatement {
                kind: TokenKind::FloatLiteral,
                value: ValueKind::Number(original),
            }),
            right: Box::new(AstNode::LiteralStatement {
                kind: TokenKind::FloatLiteral,
                value: ValueKind::Number(10_i64.pow(exponent as u32)),
            }),
            operator: TokenKind::Divide,
        }
    }

    fn parse_literal(&mut self) -> AstNode {
        if self.tokens.len() - self.position == 1 {
            let current = self.current_token();

            match current.kind {
                TokenKind::TrueLiteral => AstNode::LiteralStatement {
                    kind: TokenKind::IntegerLiteral,
                    value: ValueKind::Number(1),
                },
                TokenKind::FalseLiteral => AstNode::LiteralStatement {
                    kind: TokenKind::IntegerLiteral,
                    value: ValueKind::Number(0),
                },
                TokenKind::FloatingPoint => self.parse_float(current),
                _ => AstNode::LiteralStatement {
                    kind: current.kind,
                    value: current.value,
                },
            }
        } else {
            match self.next_token() {
                Some(token) => match token.kind {
                    TokenKind::Semicolon => {
                        let current = self.current_token();

                        self.advance();

                        match current.kind {
                            TokenKind::TrueLiteral => AstNode::LiteralStatement {
                                kind: TokenKind::IntegerLiteral,
                                value: ValueKind::Number(1),
                            },
                            TokenKind::FalseLiteral => AstNode::LiteralStatement {
                                kind: TokenKind::IntegerLiteral,
                                value: ValueKind::Number(0),
                            },
                            TokenKind::FloatingPoint => self.parse_float(current),
                            _ => AstNode::LiteralStatement {
                                kind: current.kind,
                                value: current.value,
                            },
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

        let tokens = self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon]);

        if !self.is_eof() {
            self.expect_token(TokenKind::Semicolon);
        }

        let res = if tokens.len() > 0 {
            Statement::new(tokens, 0, &self.body).parse().0
        } else {
            AstNode::LiteralStatement {
                kind: TokenKind::IntegerLiteral,
                value: ValueKind::Number(0),
            }
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

    fn parse_function(&mut self) -> AstNode {
        let name = self.get_identifier();
        let mut calculate_variadic_size = false;

        self.advance();
        match self.current_token().kind {
            TokenKind::LeftParenthesis => {
                self.advance();
            }
            TokenKind::Dot => {
                self.advance();
                calculate_variadic_size = true;

                self.expect_token(TokenKind::LeftParenthesis);
                self.advance();
            }
            other => panic!("Expected left parethesis or exclamation mark (for variadic functions) but got {:?}", other)
        }

        let mut parameters = vec![];

        while self.current_token().kind != TokenKind::RightParenthesis && !self.is_eof() {
            let mut tokens = vec![];
            let mut paren_nesting = 0;
            let mut block_nesting = 0;

            loop {
                if self.current_token().kind == TokenKind::LeftParenthesis {
                    paren_nesting += 1;
                }

                if self.current_token().kind == TokenKind::LeftBlockBrace {
                    block_nesting += 1;
                }

                tokens.push(self.current_token());
                self.advance();

                if self.current_token().kind == TokenKind::Comma {
                    if paren_nesting > 0 || block_nesting > 0 {
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
                    if paren_nesting > 0 {
                        paren_nesting -= 1;
                    } else {
                        break; // The function call has ended
                    }
                }

                if self.current_token().kind == TokenKind::RightBlockBrace {
                    if block_nesting > 0 {
                        block_nesting -= 1;
                    } else {
                        panic!(
                            "[{}] Invalid balance of block braces",
                            self.current_token().location.display()
                        )
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

        if calculate_variadic_size {
            parameters.insert(
                0,
                AstNode::LiteralStatement {
                    kind: TokenKind::IntegerLiteral,
                    value: ValueKind::Number(parameters.len() as i64),
                },
            );
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
            if token.kind.is_arithmetic() && token.kind.precedence() <= precedence && nesting == 0 {
                precedence_index = index;
                precedence = token.kind.precedence();
            }

            // This MUST be here, not at the start of the loop
            // If it was at the start then it would fail when parsing brackets
            // at the start of expressions:
            // (1 + 2) * 4 - 3 would *fail* because it will never parse the 0th bracket
            // so the nesting will never reach 0.
            index += 1;
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
            Some("\nYou cannot increment or decrement anything that isn't an identifier."),
        );

        let name = self.get_identifier();
        self.advance();

        AstNode::DeclareStatement {
            name: name.clone(),
            r#type: None,
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

    fn parse_buffer(&mut self, name: Option<String>, ty: Option<Type>) -> AstNode {
        let name = if name.is_some() {
            name.unwrap()
        } else {
            let tmp = self.get_identifier();
            self.advance();

            tmp
        };

        self.expect_token(TokenKind::LeftBlockBrace);
        self.advance();

        let mut size = None;

        if self.current_token().kind != TokenKind::RightBlockBrace {
            let tokens = self.yield_tokens_with_delimiters(vec![TokenKind::RightBlockBrace]);
            size = Some(Statement::new(tokens, 0, &self.body).parse().0);
        }

        self.expect_token(TokenKind::RightBlockBrace);
        self.advance();
        self.expect_token(TokenKind::Semicolon);

        AstNode::BufferStatement {
            name,
            r#type: Some(ty.unwrap_or(Type::Byte)),
            size: Box::new(size.unwrap()),
        }
    }

    fn parse_array(&mut self) -> AstNode {
        self.expect_token(TokenKind::LeftBlockBrace);
        self.advance();

        let mut values = vec![];

        while self.current_token().kind != TokenKind::RightBlockBrace && !self.is_eof() {
            let tmp_tokens = self
                .yield_tokens_with_delimiters(vec![TokenKind::Comma, TokenKind::RightBlockBrace]);

            if self.current_token().kind == TokenKind::Comma {
                self.advance();
            }

            values.push(Statement::new(tmp_tokens.clone(), 0, &self.body).parse().0);
        }

        self.advance();

        let array = AstNode::ArrayStatement {
            size: Box::new(AstNode::LiteralStatement {
                kind: TokenKind::LongLiteral,
                value: ValueKind::Number(values.len() as i64),
            }),
            values,
        };

        if self.current_token().kind == TokenKind::LeftBlockBrace {
            return self.parse_offset_store(Some(array));
        }

        array
    }

    fn parse_if_statement(&mut self) -> AstNode {
        self.advance();

        let tokens = self.yield_tokens_with_delimiters(vec![TokenKind::LeftCurlyBrace]);
        let expression = Statement::new(tokens, 0, &self.body).parse().0;

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

        AstNode::IfStatement {
            condition: Box::new(expression),
            body,
            else_body,
        }
    }

    fn parse_while_statement(&mut self) -> AstNode {
        self.advance();

        let tokens = self.yield_tokens_with_delimiters(vec![TokenKind::LeftCurlyBrace]);
        let expression = Statement::new(tokens, 0, &self.body).parse().0;

        self.expect_token(TokenKind::LeftCurlyBrace);
        self.advance();

        let body = self.yield_block();

        self.position -= 1;

        AstNode::WhileLoop {
            condition: Box::new(expression),
            body,
        }
    }

    fn parse_for_statement(&mut self) -> AstNode {
        self.advance();

        let mut wrapped = false;
        if self.current_token().kind == TokenKind::LeftParenthesis {
            wrapped = true;
            self.advance();
        }

        let declare_tokens = if self.current_token().kind != TokenKind::Semicolon {
            self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon])
        } else {
            vec![]
        };

        let declare = if declare_tokens.len() > 0 {
            Statement::new(declare_tokens.clone(), 0, &self.body)
                .parse()
                .0
        } else {
            AstNode::LiteralStatement {
                kind: TokenKind::Not,
                value: ValueKind::Nil,
            }
        };

        self.expect_token(TokenKind::Semicolon);
        self.advance();

        let condition_tokens = if self.current_token().kind != TokenKind::Semicolon {
            self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon])
        } else {
            vec![]
        };

        let condition = if condition_tokens.len() > 0 {
            Statement::new(condition_tokens, 0, &self.body).parse().0
        } else {
            AstNode::LiteralStatement {
                kind: TokenKind::IntegerLiteral,
                value: ValueKind::Number(1),
            }
        };

        self.expect_token(TokenKind::Semicolon);
        self.advance();

        let mut step_tokens = vec![];
        let mut nesting = 0;

        if self.current_token().kind != TokenKind::RightParenthesis {
            loop {
                if wrapped && self.current_token().kind == TokenKind::LeftParenthesis {
                    nesting += 1;
                }

                step_tokens.push(self.current_token());
                let res = self.advance_opt();

                if self.current_token().kind == TokenKind::LeftCurlyBrace {
                    break;
                }

                if wrapped && self.current_token().kind == TokenKind::RightParenthesis {
                    if nesting > 0 {
                        nesting -= 1;
                    } else {
                        break;
                    }
                }

                if self.is_eof() {
                    if res.is_some() {
                        step_tokens.push(self.current_token());
                    }

                    break;
                }
            }
        }

        if wrapped {
            self.expect_token(TokenKind::RightParenthesis);
            self.advance();
        }

        self.expect_token(TokenKind::LeftCurlyBrace);
        self.advance();

        if step_tokens.len() > 0 {
            let mut index = self.position.clone();
            let mut nesting = 0;

            loop {
                index += 1;

                if self.tokens.get(index).unwrap().kind == TokenKind::LeftCurlyBrace {
                    nesting += 1;
                }

                if self.tokens.get(index).unwrap().kind == TokenKind::RightCurlyBrace {
                    if nesting > 0 {
                        nesting -= 1;
                    } else {
                        break;
                    }
                }

                if index + 1 >= self.tokens.len() {
                    break;
                }
            }

            step_tokens.push(Token {
                kind: TokenKind::Semicolon,
                value: ValueKind::Nil,
                location: step_tokens.last().unwrap().clone().location,
            });

            for token in step_tokens.iter().cloned().rev() {
                self.tokens.insert(index, token);
            }
        }

        let body = self.yield_block();

        self.position -= 1;

        if declare_tokens.len() > 0 {
            self.body.borrow_mut().push(declare);
        }

        AstNode::WhileLoop {
            condition: Box::new(condition),
            body,
        }
    }

    fn parse_wrapped_statement(&mut self) -> AstNode {
        let mut nesting = 0;
        let mut index = 0;

        loop {
            let token = self.tokens[index].clone();

            match token.kind {
                TokenKind::LeftParenthesis => {
                    nesting += 1;
                }
                TokenKind::RightParenthesis => {
                    if nesting > 0 {
                        nesting -= 1;
                    } else {
                        break;
                    }
                }
                _ => {}
            }

            if nesting == 0 {
                break;
            }

            index += 1;
        }

        let next_token = self.tokens.get(index + 1);

        if next_token.is_some() && next_token.unwrap().kind.is_arithmetic() {
            return self.parse_arithmetic();
        }

        let mut tokens = vec![];

        loop {
            if nesting == 0 && self.current_token().kind == TokenKind::RightParenthesis {
                break;
            }

            if self.current_token().kind == TokenKind::LeftParenthesis {
                if nesting == 0 {
                    nesting += 1;
                    self.advance();
                    continue;
                }

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

        expression
    }

    fn parse_offset_store(&mut self, maybe_left: Option<AstNode>) -> AstNode {
        let original_position = self.position.clone();

        let left_tokens = if maybe_left.is_some() {
            vec![]
        } else {
            self.yield_tokens_with_delimiters(vec![TokenKind::LeftBlockBrace])
        };

        let left = Box::new(if maybe_left.is_some() {
            maybe_left.unwrap()
        } else {
            Statement::new(left_tokens, 0, &self.body).parse().0
        });

        self.expect_token(TokenKind::LeftBlockBrace);
        self.advance();

        let right_tokens = self.yield_tokens_with_delimiters(vec![TokenKind::RightBlockBrace]);
        let right = Box::new(Statement::new(right_tokens, 0, &self.body).parse().0);

        self.expect_token(TokenKind::RightBlockBrace);
        self.advance();

        if self.current_token().kind.is_arithmetic() {
            self.position = original_position.clone();
            return self.parse_arithmetic();
        }

        if self.current_token().kind == TokenKind::Semicolon || self.is_eof() {
            return AstNode::LoadStatement { left, right };
        }

        self.expect_token(TokenKind::Equal);
        self.advance();

        let tokens = self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon]);
        let value = Box::new(Statement::new(tokens, 0, &self.body).parse().0);
        AstNode::StoreStatement { left, right, value }
    }

    fn parse_variadic(&mut self) -> AstNode {
        self.advance();
        let name = self.get_identifier();

        self.advance();
        self.expect_token(TokenKind::LeftBlockBrace);
        self.advance();

        let tokens = self.yield_tokens_with_delimiters(vec![TokenKind::RightBlockBrace]);
        let size = Box::new(if tokens.len() > 0 {
            Statement::new(tokens, 0, &self.body).parse().0
        } else {
            panic!("Invalid size for buffer {}", name);
        });

        self.advance();

        if !self.is_eof() {
            self.expect_token(TokenKind::Semicolon);
        }

        AstNode::VariadicStatement { name, size }
    }

    fn parse_yield_variadic(&mut self) -> AstNode {
        let name = self.get_identifier();

        self.advance();
        self.expect_token(TokenKind::Yield);
        self.advance();

        let r#type = self.get_type();
        self.advance();

        if !self.is_eof() {
            self.expect_token(TokenKind::Semicolon);
        }

        AstNode::NextStatement {
            name,
            r#type: Some(r#type),
        }
    }

    fn parse_defer(&mut self) -> AstNode {
        self.advance();

        let tokens = self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon]);
        let value = Box::new(Statement::new(tokens, 0, &self.body).parse().0);
        AstNode::DeferStatement { value }
    }

    fn parse_type_conversion(&mut self) -> AstNode {
        self.advance();

        let r#type = self.get_type();

        self.advance();
        self.expect_token(TokenKind::RightParenthesis);
        self.advance();

        let mut tokens = vec![];
        let mut nesting = 0;

        if self.current_token().kind == TokenKind::Semicolon {
            panic!(
                "[{}] expected type conversion but got empty passthrough",
                self.current_token().location.display()
            );
        }

        loop {
            if self.current_token().kind == TokenKind::LeftParenthesis {
                nesting += 1;
            }

            tokens.push(self.current_token());
            let res = self.advance_opt();

            if self.current_token().kind == TokenKind::Semicolon
                || (self.current_token().kind.is_arithmetic() && nesting == 0)
            {
                break;
            }

            if self.current_token().kind == TokenKind::RightParenthesis {
                if nesting > 0 {
                    nesting -= 1;
                } else {
                    break;
                }
            }

            if self.is_eof() {
                if res.is_some() {
                    tokens.push(self.current_token());
                }

                break;
            }
        }

        let value = Box::new(Statement::new(tokens, 0, &self.body).parse().0);
        AstNode::ConversionStatement {
            r#type: Some(r#type),
            value,
        }
    }

    fn parse_block(&mut self) -> AstNode {
        self.expect_token(TokenKind::LeftCurlyBrace);
        self.advance();

        let body = self.yield_block();
        self.position -= 1;
        AstNode::BlockStatement { body }
    }

    fn parse_size(&mut self) -> AstNode {
        self.expect_token(TokenKind::Size);
        self.advance();

        self.expect_token(TokenKind::LeftParenthesis);
        self.advance();

        let value = if self.current_token().kind == TokenKind::Type {
            Ok(self.get_type())
        } else {
            let mut tokens = vec![];
            let mut nesting = 0;

            if self.current_token().kind == TokenKind::Semicolon {
                panic!(
                    "[{}] expected size directive but got empty passthrough",
                    self.current_token().location.display()
                );
            }

            loop {
                if self.current_token().kind == TokenKind::LeftParenthesis {
                    nesting += 1;
                }

                tokens.push(self.current_token());
                let res = self.advance_opt();

                if self.current_token().kind == TokenKind::Semicolon
                    || (self.current_token().kind.is_arithmetic() && nesting == 0)
                {
                    break;
                }

                if self.current_token().kind == TokenKind::RightParenthesis {
                    if nesting > 0 {
                        nesting -= 1;
                    } else {
                        break;
                    }
                }

                if self.is_eof() {
                    if res.is_some() {
                        tokens.push(self.current_token());
                    }

                    break;
                }
            }

            let value = Box::new(Statement::new(tokens, 0, &self.body).parse().0);
            Err(value)
        };

        self.advance();

        self.expect_token(TokenKind::RightParenthesis);
        self.advance();

        AstNode::SizeStatement {
            value,
            standalone: true,
        }
    }

    fn parse_array_length(&mut self) -> AstNode {
        self.expect_token(TokenKind::ArrayLength);
        self.advance();

        self.expect_token(TokenKind::LeftParenthesis);
        self.advance();

        let mut tokens = vec![];
        let mut nesting = 0;

        if self.current_token().kind == TokenKind::Semicolon {
            panic!(
                "[{}] expected type conversion but got empty passthrough",
                self.current_token().location.display()
            );
        }

        loop {
            if self.current_token().kind == TokenKind::LeftParenthesis {
                nesting += 1;
            }

            tokens.push(self.current_token());
            let res = self.advance_opt();

            if self.current_token().kind == TokenKind::Semicolon
                || (self.current_token().kind.is_arithmetic() && nesting == 0)
            {
                break;
            }

            if self.current_token().kind == TokenKind::RightParenthesis {
                if nesting > 0 {
                    nesting -= 1;
                } else {
                    break;
                }
            }

            if self.is_eof() {
                if res.is_some() {
                    tokens.push(self.current_token());
                }

                break;
            }
        }

        let value = Box::new(Statement::new(tokens, 0, &self.body).parse().0);
        self.advance();

        self.expect_token(TokenKind::RightParenthesis);
        self.advance();

        AstNode::SizeStatement {
            value: Err(value),
            standalone: false,
        }
    }

    fn parse_unary(&mut self) -> AstNode {
        let token = self.current_token();
        self.advance();

        let tokens = self.yield_tokens_with_condition(|token| {
            token.kind.is_arithmetic() || token.kind == TokenKind::Semicolon
        });

        let parsed = Box::new(Statement::new(tokens, 0, &self.body).parse().0);

        AstNode::ArithmeticOperation {
            left: parsed,
            right: Box::new(AstNode::token_to_literal(token)),
            operator: TokenKind::Multiply,
        }
    }

    fn parse_not(&mut self) -> AstNode {
        self.advance();

        let tokens = self.yield_tokens_with_condition(|token| {
            token.kind.is_arithmetic() || token.kind == TokenKind::Semicolon
        });

        let value = Box::new(Statement::new(tokens, 0, &self.body).parse().0);
        AstNode::NotStatement { value }
    }

    fn parse_address(&mut self) -> AstNode {
        self.advance();

        let name = self.get_identifier();
        self.advance();

        AstNode::AddressStatement { name }
    }

    fn parse_deref(&mut self) -> AstNode {
        self.advance();

        let addr_tokens = self.yield_tokens_with_condition(|token| {
            token.kind.is_arithmetic()
                || token.kind == TokenKind::Semicolon
                || token.kind == TokenKind::Equal
        });

        let left = Box::new(Statement::new(addr_tokens, 0, &self.body).parse().0);
        let right = Box::new(AstNode::LiteralStatement {
            kind: TokenKind::LongLiteral,
            value: ValueKind::Number(0),
        });

        if self.current_token().kind == TokenKind::Equal {
            self.advance();

            let value_tokens = self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon]);
            let value = Box::new(Statement::new(value_tokens, 0, &self.body).parse().0);

            AstNode::StoreStatement { left, right, value }
        } else {
            AstNode::LoadStatement { left, right }
        }
    }

    fn yield_tokens_with_delimiters(&mut self, delimiters: Vec<TokenKind>) -> Vec<Token> {
        if delimiters.contains(&self.current_token().kind) {
            panic!(
                "[{}] expected expression but got {:?}",
                self.current_token().location.display(),
                self.current_token().kind
            );
        }

        return self.yield_tokens_with_condition(|token| delimiters.contains(&token.kind));
    }

    fn yield_tokens_with_condition<F>(&mut self, condition: F) -> Vec<Token>
    where
        F: Fn(Token) -> bool,
    {
        let mut tokens = vec![];

        loop {
            tokens.push(self.current_token());
            let res = self.advance_opt();

            if condition(self.current_token().clone()) {
                break;
            }

            if self.is_eof() {
                if res.is_some() {
                    tokens.push(self.current_token());
                }

                break;
            }
        }

        tokens
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
                    let (node, position, tokens) =
                        Statement::new(self.tokens.clone(), self.position.clone(), &cell).parse();

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
                            _ => {
                                body_ref.remove(res.unwrap());
                                body_ref.push(node);
                            }
                        }
                    } else {
                        body_ref.push(node);
                    }

                    self.position = position;
                    self.tokens = tokens;
                }
            };

            self.advance();
        }

        let mut res = cell.borrow_mut().to_owned().clone();
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
        res
    }

    fn parse_primary(&mut self) -> AstNode {
        match self.current_token().kind {
            token if token.is_literal() => self.parse_literal(),
            TokenKind::Unary => self.parse_unary(),
            TokenKind::Not => self.parse_not(),
            TokenKind::Deref => self.parse_deref(),
            TokenKind::Address => self.parse_address(),
            TokenKind::Size => self.parse_size(),
            TokenKind::ArrayLength => self.parse_array_length(),
            TokenKind::LeftParenthesis => {
                let next = self.next_token();

                if next.is_some() && next.unwrap().kind == TokenKind::Type {
                    self.parse_type_conversion()
                } else {
                    self.parse_wrapped_statement()
                }
            }
            TokenKind::LeftCurlyBrace => self.parse_block(),
            TokenKind::LeftBlockBrace => self.parse_array(),
            TokenKind::Identifier => {
                if self.is_eof() {
                    self.parse_literal()
                } else {
                    let next = self.tokens[self.position + 1].clone();

                    if next.kind == TokenKind::LeftParenthesis {
                        self.parse_function()
                    } else if next.kind == TokenKind::LeftBlockBrace {
                        self.parse_offset_store(None)
                    } else if next.kind == TokenKind::Dot {
                        if self.position + 2 > self.tokens.len() - 1 {
                            panic!("EOF but specified a variadic identifier")
                        }

                        self.parse_function()
                    } else if next.kind == TokenKind::Equal {
                        self.parse_declare(Some(None))
                    } else if next.kind.is_declarative() {
                        self.parse_declarative_like()
                    } else if next.kind == TokenKind::Yield {
                        self.parse_yield_variadic()
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

    pub fn parse(&mut self) -> (AstNode, usize, Vec<Token>) {
        let node = match self.current_token().kind {
            TokenKind::Type => self.parse_declare(None),
            TokenKind::Variadic => self.parse_variadic(),
            TokenKind::Return => self.parse_return(),
            TokenKind::If => self.parse_if_statement(),
            TokenKind::While => self.parse_while_statement(),
            TokenKind::For => self.parse_for_statement(),
            TokenKind::Defer => self.parse_defer(),
            _ => self.parse_expression(),
        };

        (node, self.position, self.tokens.clone())
    }
}
