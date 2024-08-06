use std::cell::RefCell;
use std::iter::FromIterator;

use super::enums::AstNode;
use crate::{
    compiler::enums::Type,
    lexer::enums::{Token, TokenKind, ValueKind},
    token_to_node,
};

pub struct Statement<'a> {
    tokens: Vec<Token>,
    position: usize,
    body: &'a RefCell<Vec<AstNode>>,
    struct_pool: Vec<String>,
    standalone: bool,
}

impl<'a> Statement<'a> {
    pub fn new(
        tokens: Vec<Token>,
        position: usize,
        body: &'a RefCell<Vec<AstNode>>,
        struct_pool: Vec<String>,
        standalone: bool,
    ) -> Self {
        Statement {
            tokens,
            position,
            body,
            struct_pool,
            standalone,
        }
    }

    pub fn advance(&mut self) {
        if self.is_eof() {
            #[cfg(debug_assertions)]
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

    fn next_token_seek(&mut self, seek: usize) -> Option<Token> {
        match self.is_eof() {
            true => None,
            false => Some(self.tokens[self.position + seek].clone()),
        }
    }

    fn is_eof(&mut self) -> bool {
        self.position + 1 >= self.tokens.len()
    }

    fn expect_token_with_message(&self, expected: Vec<TokenKind>, message: Option<&str>) {
        if !expected.contains(&self.current_token().kind) {
            panic!(
                "{}",
                self.current_token().location.error(format!(
                    "Expected one of [{}], got {:?}. {}",
                    expected
                        .iter()
                        .map(|kind| kind.to_string())
                        .collect::<Vec<String>>()
                        .join(", "),
                    self.current_token(),
                    message.unwrap_or("")
                )),
            )
        }
    }

    fn expect_token(&self, expected: Vec<TokenKind>) {
        self.expect_token_with_message(expected, None);
    }

    pub fn get(&mut self, expected: Vec<TokenKind>) -> String {
        let mut found = false;

        for kind in expected.clone().iter().cloned() {
            if self.current_token().kind == kind {
                found = true;
                break;
            }
        }

        let token = self.current_token();

        if !found {
            panic!(
                "{}",
                token.location.error(format!(
                    "Expected one of {:?} but got {:?}",
                    expected, token.kind
                ))
            )
        }

        let identifier = if let Token {
            value: ValueKind::String(identifier),
            ..
        } = self.current_token()
        {
            identifier.clone()
        } else {
            token.location.error(format!(
                "Expected one of {:?} but got {:?}",
                expected, token.kind
            ))
        };

        identifier
    }

    pub fn get_identifier(&mut self) -> String {
        self.get(vec![TokenKind::Identifier])
    }

    pub fn get_type(&mut self) -> Type {
        let name = self.get(vec![TokenKind::Identifier]);

        let is_valid =
            self.struct_pool.contains(&name) || ValueKind::String(name.clone()).is_base_type();

        if !is_valid {
            panic!(
                "{}",
                self.current_token().location.error(format!(
                    "Type named {} could not be found. Are you sure you spelt it correctly?",
                    name
                ))
            )
        }

        let mut ty = ValueKind::String(name).to_type_string().unwrap();

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

        // Essentially makes the compiler forget what type the pointer holds
        if matches!(ty.clone(), Type::Pointer(inner) if matches!(*inner, Type::Void)) {
            Type::Long
        } else {
            ty
        }
    }

    fn parse_declare(&mut self, ty: Option<Option<Type>>) -> AstNode {
        let r#type = if ty.is_some() {
            ty.clone().unwrap()
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
                    location: self.current_token().location,
                }),
                location: self.current_token().location,
            };
        }

        self.expect_token(vec![TokenKind::Equal]);
        self.advance();

        let tokens = self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon]);

        let res = Statement::new(tokens, 0, &self.body, self.struct_pool.clone(), false)
            .parse()
            .0;

        let parsed_res = match res.clone() {
            AstNode::DeclareStatement { name, .. } => {
                self.body.borrow_mut().push(res);

                AstNode::LiteralStatement {
                    kind: TokenKind::Identifier,
                    value: ValueKind::String(name),
                    location: self.current_token().location,
                }
            }
            _ => res,
        };

        AstNode::DeclareStatement {
            name,
            r#type,
            value: Box::new(parsed_res),
            location: self.current_token().location,
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
            let mapping = res.unwrap();

            if self.standalone {
                return AstNode::DeclareStatement {
                    name: name.clone(),
                    r#type: None,
                    value: Box::new(AstNode::ArithmeticOperation {
                        left: Box::new(AstNode::LiteralStatement {
                            kind: TokenKind::Identifier,
                            value: ValueKind::String(name.clone()),
                            location: self.current_token().location,
                        }),
                        right: Box::new(AstNode::LiteralStatement {
                            kind: TokenKind::IntegerLiteral,
                            value: ValueKind::Number(1),
                            location: self.current_token().location,
                        }),
                        operator: mapping,
                        location: self.current_token().location,
                    }),
                    location: self.current_token().location,
                };
            }

            self.body.borrow_mut().push(AstNode::LiteralStatement {
                kind: TokenKind::ExactLiteral,
                value: ValueKind::String("__<#insert#>__".to_owned()),
                location: self.current_token().location,
            });

            self.body.borrow_mut().push(AstNode::DeclareStatement {
                name: name.clone(),
                r#type: None,
                value: Box::new(AstNode::ArithmeticOperation {
                    left: Box::new(AstNode::LiteralStatement {
                        kind: TokenKind::Identifier,
                        value: ValueKind::String(name.clone()),
                        location: self.current_token().location,
                    }),
                    right: Box::new(AstNode::LiteralStatement {
                        kind: TokenKind::IntegerLiteral,
                        value: ValueKind::Number(1),
                        location: self.current_token().location,
                    }),
                    operator: mapping,
                    location: self.current_token().location,
                }),
                location: self.current_token().location,
            });

            return AstNode::LiteralStatement {
                kind: TokenKind::Identifier,
                value: ValueKind::String(name.clone()),
                location: self.current_token().location,
            };
        }

        let tokens = self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon]);
        let mapping = operation.kind.to_non_declarative();

        AstNode::DeclareStatement {
            name: name.clone(),
            r#type: None,
            value: Box::new(AstNode::ArithmeticOperation {
                left: Box::new(AstNode::LiteralStatement {
                    kind: TokenKind::Identifier,
                    value: ValueKind::String(name),
                    location: self.current_token().location,
                }),
                right: Box::new(
                    Statement::new(tokens, 0, &self.body, self.struct_pool.clone(), false)
                        .parse()
                        .0,
                ),
                operator: mapping,
                location: self.current_token().location,
            }),
            location: self.current_token().location,
        }
    }

    fn parse_float(&self, token: Token) -> AstNode {
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
        let original = String::from_iter([left, right]).parse::<i128>().unwrap();

        AstNode::ArithmeticOperation {
            left: Box::new(AstNode::LiteralStatement {
                kind: TokenKind::FloatLiteral,
                value: ValueKind::Number(original),
                location: self.current_token().location,
            }),
            right: Box::new(AstNode::LiteralStatement {
                kind: TokenKind::FloatLiteral,
                value: ValueKind::Number(10_i128.pow(exponent as u32)),
                location: self.current_token().location,
            }),
            operator: TokenKind::Divide,
            location: self.current_token().location,
        }
    }

    fn parse_literal(&mut self) -> AstNode {
        let position = self.position.clone();

        if self.is_eof() {
            let current = self.current_token();
            token_to_node!(current, self)
        } else {
            match self.next_token() {
                Some(token) => match token.kind {
                    TokenKind::Semicolon => {
                        let current = self.current_token();
                        self.advance();
                        token_to_node!(current, self)
                    }
                    TokenKind::LeftBlockBrace => {
                        let current = self.current_token();
                        self.advance();
                        self.parse_offset_store(Some((position, token_to_node!(current, self))))
                    }
                    _ => self.parse_arithmetic(),
                },
                None => unreachable!(),
            }
        }
    }

    fn parse_return(&mut self) -> AstNode {
        self.advance();

        let tokens = self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon]);

        if !self.is_eof() {
            self.expect_token(vec![TokenKind::Semicolon]);
        }

        let res = if tokens.len() > 0 {
            Statement::new(tokens, 0, &self.body, self.struct_pool.clone(), false)
                .parse()
                .0
        } else {
            AstNode::LiteralStatement {
                kind: TokenKind::IntegerLiteral,
                value: ValueKind::Number(0),
                location: self.current_token().location,
            }
        };

        let parsed_res = match res.clone() {
            AstNode::DeclareStatement { name, .. } => {
                self.body.borrow_mut().push(res);

                AstNode::LiteralStatement {
                    kind: TokenKind::Identifier,
                    value: ValueKind::String(name),
                    location: self.current_token().location,
                }
            }
            _ => res,
        };

        AstNode::ReturnStatement {
            value: Box::new(parsed_res),
            location: self.current_token().location,
        }
    }

    fn parse_function(&mut self) -> AstNode {
        let name = self.get_identifier();
        let location = self.current_token().location.clone();
        let mut calculate_variadic_size = false;

        self.advance();
        match self.current_token().kind {
            TokenKind::LeftParenthesis => {
                self.advance();
            }
            TokenKind::Dot => {
                self.advance();
                calculate_variadic_size = true;

                self.expect_token(vec![TokenKind::LeftParenthesis]);
                self.advance();
            }
            other => panic!("Expected left parethesis or exclamation mark (for variadic functions) but got {:?}", other)
        }

        let mut parameters = vec![];

        while self.current_token().kind != TokenKind::RightParenthesis && !self.is_eof() {
            let location = self.current_token().location.clone();

            let mut tokens = vec![];
            let mut paren_nesting = 0;
            let mut block_nesting = 0;
            let mut curly_nesting = 0;

            loop {
                // Wrapped statement, deref, nested function call
                if self.current_token().kind == TokenKind::LeftParenthesis {
                    paren_nesting += 1;
                }

                // Inline array
                if self.current_token().kind == TokenKind::LeftBlockBrace {
                    block_nesting += 1;
                }

                // Struct init
                if self.current_token().kind == TokenKind::LeftCurlyBrace {
                    curly_nesting += 1;
                }

                tokens.push(self.current_token());
                self.advance();

                if self.current_token().kind == TokenKind::Comma {
                    if paren_nesting > 0 || block_nesting > 0 || curly_nesting > 0 {
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
                            "{}",
                            self.current_token()
                                .location
                                .error("Invalid balance of block braces".to_string())
                        )
                    }
                }

                if self.current_token().kind == TokenKind::RightCurlyBrace {
                    if curly_nesting > 0 {
                        curly_nesting -= 1;
                    } else {
                        panic!(
                            "{}",
                            self.current_token()
                                .location
                                .error("Invalid balance of curly braces".to_string())
                        )
                    }
                }

                if self.is_eof() {
                    break;
                }
            }

            parameters.push((
                location,
                Statement::new(
                    tokens.clone(),
                    0,
                    &self.body,
                    self.struct_pool.clone(),
                    false,
                )
                .parse()
                .0,
            ));
        }

        self.expect_token_with_message(
            vec![TokenKind::RightParenthesis],
            Some("Perhaps you forgot to close a nested expression?"),
        );

        self.advance();

        if calculate_variadic_size {
            parameters.insert(
                0,
                (
                    location.clone(),
                    AstNode::LiteralStatement {
                        kind: TokenKind::IntegerLiteral,
                        value: ValueKind::Number(parameters.len() as i128),
                        location: location.clone(),
                    },
                ),
            );
        }

        AstNode::FunctionCall {
            name,
            parameters,
            location,
        }
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
                TokenKind::Semicolon => {
                    break;
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

        let right_end_index = if let Some(index) = raw_right
            .iter()
            .position(|token| token.kind == TokenKind::Semicolon)
        {
            index + 1
        } else {
            raw_right.len()
        };

        // Separate the right-hand side expression up to a semicolon
        let right = raw_right[..right_end_index].to_vec();

        // Shift the position across the size of the expression
        self.position += left.len() + right_end_index;

        AstNode::ArithmeticOperation {
            left: Box::new(
                Statement::new(left, 0, &self.body, self.struct_pool.clone(), false)
                    .parse()
                    .0,
            ),
            right: Box::new(
                Statement::new(right, 0, &self.body, self.struct_pool.clone(), false)
                    .parse()
                    .0,
            ),
            operator,
            location: self.current_token().location,
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
                location: self.current_token().location,
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
            vec![TokenKind::Identifier],
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
                    location: self.current_token().location,
                }),
                right: Box::new(AstNode::LiteralStatement {
                    kind: TokenKind::IntegerLiteral,
                    value: ValueKind::Number(1),
                    location: self.current_token().location,
                }),
                operator: mapping,
                location: self.current_token().location,
            }),
            location: self.current_token().location,
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

        self.expect_token(vec![TokenKind::LeftBlockBrace]);
        self.advance();

        let mut size = None;

        if self.current_token().kind != TokenKind::RightBlockBrace {
            let tokens = self.yield_tokens_with_delimiters(vec![TokenKind::RightBlockBrace]);
            size = Some(
                Statement::new(tokens, 0, &self.body, self.struct_pool.clone(), false)
                    .parse()
                    .0,
            );
        }

        self.expect_token(vec![TokenKind::RightBlockBrace]);
        self.advance();
        self.expect_token(vec![TokenKind::Semicolon]);

        AstNode::BufferStatement {
            name,
            r#type: Some(ty.unwrap_or(Type::Byte)),
            size: Box::new(size.unwrap()),
            location: self.current_token().location,
        }
    }

    fn parse_array(&mut self) -> AstNode {
        let position = self.position.clone();
        self.expect_token(vec![TokenKind::LeftBlockBrace]);
        self.advance();

        let mut values = vec![];

        while self.current_token().kind != TokenKind::RightBlockBrace && !self.is_eof() {
            let tmp_tokens = self
                .yield_tokens_with_delimiters(vec![TokenKind::Comma, TokenKind::RightBlockBrace]);

            if self.current_token().kind == TokenKind::Comma {
                self.advance();
            }

            values.push(
                Statement::new(
                    tmp_tokens.clone(),
                    0,
                    &self.body,
                    self.struct_pool.clone(),
                    false,
                )
                .parse()
                .0,
            );
        }

        self.advance();

        let array = AstNode::ArrayStatement {
            size: Box::new(AstNode::LiteralStatement {
                kind: TokenKind::LongLiteral,
                value: ValueKind::Number(values.len() as i128),
                location: self.current_token().location,
            }),
            values,
            location: self.current_token().location,
        };

        if self.current_token().kind == TokenKind::LeftBlockBrace {
            return self.parse_offset_store(Some((position, array)));
        }

        array
    }

    fn parse_if_statement(&mut self) -> AstNode {
        self.advance();

        let tokens = self.yield_tokens_with_delimiters(vec![TokenKind::LeftCurlyBrace]);
        let expression = Statement::new(tokens, 0, &self.body, self.struct_pool.clone(), false)
            .parse()
            .0;

        self.expect_token(vec![TokenKind::LeftCurlyBrace]);
        self.advance();

        let body = self.yield_block();
        let mut else_body: Vec<AstNode> = vec![];

        if self.current_token().kind == TokenKind::Else {
            self.advance();
            self.expect_token(vec![TokenKind::LeftCurlyBrace]);
            self.advance();

            else_body = self.yield_block();
        }

        self.position -= 1;

        AstNode::IfStatement {
            condition: Box::new(expression),
            body,
            else_body,
            location: self.current_token().location,
        }
    }

    fn parse_while_statement(&mut self) -> AstNode {
        self.advance();

        let tokens = self.yield_tokens_with_delimiters(vec![TokenKind::LeftCurlyBrace]);
        let expression = Statement::new(tokens, 0, &self.body, self.struct_pool.clone(), false)
            .parse()
            .0;

        self.expect_token(vec![TokenKind::LeftCurlyBrace]);
        self.advance();

        let body = self.yield_block();

        self.position -= 1;

        AstNode::WhileLoop {
            condition: Box::new(expression),
            step: None,
            body,
            location: self.current_token().location,
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
            Statement::new(
                declare_tokens.clone(),
                0,
                &self.body,
                self.struct_pool.clone(),
                true,
            )
            .parse()
            .0
        } else {
            AstNode::LiteralStatement {
                kind: TokenKind::Not,
                value: ValueKind::Nil,
                location: self.current_token().location,
            }
        };

        self.expect_token(vec![TokenKind::Semicolon]);
        self.advance();

        let condition_tokens = if self.current_token().kind != TokenKind::Semicolon {
            self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon])
        } else {
            vec![]
        };

        let condition = if condition_tokens.len() > 0 {
            Statement::new(
                condition_tokens,
                0,
                &self.body,
                self.struct_pool.clone(),
                true,
            )
            .parse()
            .0
        } else {
            AstNode::LiteralStatement {
                kind: TokenKind::IntegerLiteral,
                value: ValueKind::Number(1),
                location: self.current_token().location,
            }
        };

        self.expect_token(vec![TokenKind::Semicolon]);
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
            self.expect_token(vec![TokenKind::RightParenthesis]);
            self.advance();
        }

        self.expect_token(vec![TokenKind::LeftCurlyBrace]);
        self.advance();

        let step = if step_tokens.len() > 0 {
            Statement::new(step_tokens, 0, &self.body, self.struct_pool.clone(), true)
                .parse()
                .0
        } else {
            AstNode::LiteralStatement {
                kind: TokenKind::IntegerLiteral,
                value: ValueKind::Number(1),
                location: self.current_token().location,
            }
        };

        let body = self.yield_block();

        self.position -= 1;

        if declare_tokens.len() > 0 {
            self.body.borrow_mut().push(declare);
        }

        AstNode::WhileLoop {
            condition: Box::new(condition),
            step: Some(Box::new(step)),
            body,
            location: self.current_token().location,
        }
    }

    fn parse_wrapped_statement(&mut self) -> AstNode {
        let mut nesting = 0;
        let mut index = 0;
        let position = self.position.clone();

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

        let mut expression = Statement::new(tokens, 0, &self.body, self.struct_pool.clone(), false)
            .parse()
            .0;

        self.expect_token(vec![TokenKind::RightParenthesis]);
        self.advance();

        match self.current_token().kind {
            TokenKind::Dot => expression = self.parse_field_access(Some((position, expression))),
            TokenKind::LeftBlockBrace => {
                expression = self.parse_offset_store(Some((position, expression)))
            }
            _ => {}
        }

        expression
    }

    fn parse_offset_store(&mut self, lhs: Option<(usize, AstNode)>) -> AstNode {
        let position = if lhs.is_some() {
            lhs.clone().unwrap().0
        } else {
            self.position.clone()
        };

        let location = if lhs.is_some() {
            self.tokens[lhs.clone().unwrap().0].location.clone()
        } else {
            self.current_token().location.clone()
        };

        let mut value = None;

        let left_tokens = if lhs.is_some() {
            vec![]
        } else {
            self.yield_tokens_with_delimiters(vec![TokenKind::LeftBlockBrace])
        };

        let left = Box::new(if lhs.is_some() {
            lhs.unwrap().1
        } else {
            Statement::new(left_tokens, 0, &self.body, self.struct_pool.clone(), false)
                .parse()
                .0
        });

        self.expect_token(vec![TokenKind::LeftBlockBrace]);
        self.advance();

        let right_tokens = self.yield_tokens_with_delimiters(vec![TokenKind::RightBlockBrace]);
        let right = Box::new(
            Statement::new(right_tokens, 0, &self.body, self.struct_pool.clone(), false)
                .parse()
                .0,
        );

        self.expect_token(vec![TokenKind::RightBlockBrace]);
        self.advance();

        match self.current_token().kind {
            TokenKind::Equal => {
                self.advance();
                let value_tokens = self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon]);

                value = Some(Box::new(
                    Statement::new(value_tokens, 0, &self.body, self.struct_pool.clone(), false)
                        .parse()
                        .0,
                ));
            }
            other if other.is_declarative() => {
                value = Some(Box::new(self.parse_declarative_node(
                    AstNode::MemoryStatement {
                        left: left.clone(),
                        right: right.clone(),
                        value,
                        location: self.current_token().location,
                    },
                )));
            }
            other if other.is_arithmetic() => {
                self.position = position;
                return self.parse_arithmetic();
            }
            _ => {}
        }

        AstNode::MemoryStatement {
            left,
            right,
            value,
            location,
        }
    }

    fn parse_variadic(&mut self) -> AstNode {
        self.advance();
        let name = self.get_identifier();

        self.advance();
        self.expect_token(vec![TokenKind::LeftBlockBrace]);
        self.advance();

        let tokens = self.yield_tokens_with_delimiters(vec![TokenKind::RightBlockBrace]);
        let size = Box::new(if tokens.len() > 0 {
            Statement::new(tokens, 0, &self.body, self.struct_pool.clone(), false)
                .parse()
                .0
        } else {
            panic!("Invalid size for buffer {}", name);
        });

        self.advance();

        if !self.is_eof() {
            self.expect_token(vec![TokenKind::Semicolon]);
        }

        AstNode::VariadicStatement {
            name,
            size,
            location: self.current_token().location,
        }
    }

    fn parse_yield_variadic(&mut self) -> AstNode {
        let name = self.get_identifier();

        self.advance();
        self.expect_token(vec![TokenKind::Yield]);
        self.advance();

        let r#type = self.get_type();
        self.advance();

        if !self.is_eof() {
            self.expect_token(vec![TokenKind::Semicolon]);
        }

        AstNode::NextStatement {
            name,
            r#type: Some(r#type),
            location: self.current_token().location,
        }
    }

    fn parse_defer(&mut self) -> AstNode {
        self.advance();

        let tokens = self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon]);
        let value = Box::new(
            Statement::new(tokens, 0, &self.body, self.struct_pool.clone(), false)
                .parse()
                .0,
        );
        AstNode::DeferStatement {
            value,
            location: self.current_token().location,
        }
    }

    fn parse_type_conversion(&mut self) -> AstNode {
        self.advance();

        let r#type = self.get_type();

        self.advance();
        self.expect_token(vec![TokenKind::RightParenthesis]);
        self.advance();

        let mut tokens = vec![];
        let mut nesting = 0;

        if self.current_token().kind == TokenKind::Semicolon {
            panic!(
                "{}",
                self.current_token()
                    .location
                    .error("Expected type conversion but got empty passthrough".to_string())
            )
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

        let value = Box::new(
            Statement::new(tokens, 0, &self.body, self.struct_pool.clone(), false)
                .parse()
                .0,
        );
        AstNode::ConversionStatement {
            r#type: Some(r#type),
            value,
            location: self.current_token().location,
        }
    }

    fn parse_block(&mut self) -> AstNode {
        self.expect_token(vec![TokenKind::LeftCurlyBrace]);
        self.advance();

        let body = self.yield_block();
        self.position -= 1;
        AstNode::BlockStatement {
            body,
            location: self.current_token().location,
        }
    }

    fn parse_size(&mut self) -> AstNode {
        self.expect_token(vec![TokenKind::Size]);
        self.advance();

        self.expect_token(vec![TokenKind::LeftParenthesis]);
        self.advance();

        let value = if self.current_token().kind == TokenKind::Identifier
            && (self
                .struct_pool
                .contains(&self.current_token().value.get_string_inner().unwrap())
                || self.current_token().value.is_base_type())
        {
            Ok(self.get_type())
        } else {
            let mut tokens = vec![];
            let mut nesting = 0;

            if self.current_token().kind == TokenKind::Semicolon {
                panic!(
                    "{}",
                    self.current_token()
                        .location
                        .error("Expected size directive but got empty passthrough".to_string())
                )
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

            let value = Box::new(
                Statement::new(tokens, 0, &self.body, self.struct_pool.clone(), false)
                    .parse()
                    .0,
            );
            Err(value)
        };

        self.advance();

        self.expect_token(vec![TokenKind::RightParenthesis]);
        self.advance();

        AstNode::SizeStatement {
            value,
            standalone: true,
            location: self.current_token().location,
        }
    }

    fn parse_array_length(&mut self) -> AstNode {
        self.expect_token(vec![TokenKind::ArrayLength]);
        self.advance();

        self.expect_token(vec![TokenKind::LeftParenthesis]);
        self.advance();

        let mut tokens = vec![];
        let mut nesting = 0;

        if self.current_token().kind == TokenKind::Semicolon {
            panic!(
                "{}",
                self.current_token()
                    .location
                    .error("Expected type conversion but got empty passthrough".to_string())
            )
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

        let value = Box::new(
            Statement::new(tokens, 0, &self.body, self.struct_pool.clone(), false)
                .parse()
                .0,
        );
        self.advance();

        self.expect_token(vec![TokenKind::RightParenthesis]);
        self.advance();

        AstNode::SizeStatement {
            value: Err(value),
            standalone: false,
            location: self.current_token().location,
        }
    }

    fn parse_unary(&mut self) -> AstNode {
        let token = self.current_token();
        self.advance();

        let tokens = self.yield_tokens_with_condition(|token, prev_token| {
            (token.kind.is_arithmetic() && prev_token.kind != TokenKind::Type)
                || token.kind == TokenKind::Semicolon
        });

        let parsed = Box::new(
            Statement::new(tokens, 0, &self.body, self.struct_pool.clone(), false)
                .parse()
                .0,
        );

        AstNode::ArithmeticOperation {
            left: parsed,
            right: Box::new(AstNode::token_to_literal(token)),
            operator: TokenKind::Multiply,
            location: self.current_token().location,
        }
    }

    fn parse_not(&mut self) -> AstNode {
        self.advance();

        let tokens = self.yield_tokens_with_condition(|token, prev_token| {
            (token.kind.is_arithmetic() && prev_token.kind != TokenKind::Type)
                || token.kind == TokenKind::Semicolon
        });

        let value = Box::new(
            Statement::new(tokens, 0, &self.body, self.struct_pool.clone(), false)
                .parse()
                .0,
        );
        AstNode::NotStatement {
            value,
            location: self.current_token().location,
        }
    }

    fn parse_address(&mut self) -> AstNode {
        self.advance();

        let name = self.get_identifier();
        self.advance();

        AstNode::AddressStatement {
            name,
            location: self.current_token().location,
        }
    }

    fn parse_deref(&mut self) -> AstNode {
        self.advance();

        let location = self.current_token().location.clone();

        let mut value = None;
        let addr_tokens = self.yield_tokens_with_condition(|token, prev_token| {
            (token.kind.is_arithmetic() && prev_token.kind != TokenKind::Type)
                || token.kind.is_declarative()
                || token.kind == TokenKind::Semicolon
                || token.kind == TokenKind::Equal
        });

        let left = Box::new(
            Statement::new(addr_tokens, 0, &self.body, self.struct_pool.clone(), false)
                .parse()
                .0,
        );
        let right = Box::new(AstNode::LiteralStatement {
            kind: TokenKind::LongLiteral,
            value: ValueKind::Number(0),
            location: self.current_token().location,
        });

        match self.current_token().kind {
            TokenKind::Equal => {
                self.advance();
                let value_tokens = self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon]);

                value = Some(Box::new(
                    Statement::new(value_tokens, 0, &self.body, self.struct_pool.clone(), false)
                        .parse()
                        .0,
                ));
            }
            other if other.is_declarative() => {
                value = Some(Box::new(self.parse_declarative_node(
                    AstNode::MemoryStatement {
                        left: left.clone(),
                        right: right.clone(),
                        value,
                        location: location.clone(),
                    },
                )));
            }
            _ => {}
        }

        AstNode::MemoryStatement {
            left,
            right,
            value,
            location,
        }
    }

    fn parse_struct_init(&mut self) -> AstNode {
        let name = self.get_identifier();
        let location = self.current_token().location.clone();

        if !self.struct_pool.contains(&name) {
            panic!(
                "{}",
                self.current_token().location.error(format!(
                    "Struct named '{}' could not be found. Are you sure you typed it correctly?",
                    name
                ))
            )
        }

        self.advance();
        self.expect_token(vec![TokenKind::LeftCurlyBrace]);
        self.advance();

        let mut values = vec![];

        loop {
            if self.current_token().kind == TokenKind::RightCurlyBrace {
                self.advance();
                break;
            }

            if self.current_token().kind == TokenKind::Comma {
                self.advance();
                continue;
            }

            let name = self.get_identifier();

            self.advance();
            self.expect_token(vec![TokenKind::Equal]);
            self.advance();

            let mut tokens = vec![];
            let mut paren_nesting = 0;
            let mut block_nesting = 0;
            let mut curly_nesting = 0;

            loop {
                // Wrapped statement, deref, nested function call
                if self.current_token().kind == TokenKind::LeftParenthesis {
                    paren_nesting += 1;
                }

                // Inline array
                if self.current_token().kind == TokenKind::LeftBlockBrace {
                    block_nesting += 1;
                }

                // Struct init
                if self.current_token().kind == TokenKind::LeftCurlyBrace {
                    curly_nesting += 1;
                }

                tokens.push(self.current_token());
                self.advance();

                if self.current_token().kind == TokenKind::Comma {
                    if paren_nesting > 0 || block_nesting > 0 || curly_nesting > 0 {
                        tokens.push(self.current_token());
                        self.advance();
                        continue;
                    } else {
                        self.advance();
                        break;
                    }
                }

                if self.current_token().kind == TokenKind::RightParenthesis {
                    if paren_nesting > 0 {
                        paren_nesting -= 1;
                    }
                }

                if self.current_token().kind == TokenKind::RightBlockBrace {
                    if block_nesting > 0 {
                        block_nesting -= 1;
                    }
                }

                if self.current_token().kind == TokenKind::RightCurlyBrace {
                    if curly_nesting > 0 {
                        curly_nesting -= 1;
                    }
                }

                if self.is_eof() {
                    break;
                }
            }

            let value = Box::new(
                Statement::new(tokens, 0, &self.body, self.struct_pool.clone(), false)
                    .parse()
                    .0,
            );

            values.push((name, value));
        }

        AstNode::StructStatement {
            name,
            values,
            location,
        }
    }

    fn parse_field_access(&mut self, lhs: Option<(usize, AstNode)>) -> AstNode {
        let location = self.current_token().location.clone();
        let valid_tokens = vec![TokenKind::Dot];
        let mut value = None;

        let position = if lhs.is_some() {
            lhs.clone().unwrap().0
        } else {
            self.position.clone()
        };

        // Parse the initial left-hand side of the field access
        let left = if lhs.is_some() {
            Box::new(lhs.unwrap().1)
        } else {
            let left_tokens = self.yield_tokens_with_delimiters(valid_tokens.clone());

            Box::new(
                Statement::new(left_tokens, 0, &self.body, self.struct_pool.clone(), false)
                    .parse()
                    .0,
            )
        };

        self.expect_token(valid_tokens.clone());
        self.advance();

        self.expect_token(vec![TokenKind::Identifier]);
        let mut right = Box::new(AstNode::token_to_literal(self.current_token()));
        self.advance();

        // Parse the rest of the field accesses
        while valid_tokens.contains(&self.current_token().kind) {
            self.advance(); // Ignore the TokenKind::Dot

            self.expect_token(vec![TokenKind::Identifier]);
            let location = self.current_token().location.clone();
            let inner = Box::new(AstNode::token_to_literal(self.current_token()));
            self.advance();

            if let AstNode::FieldStatement {
                left,
                right: inner_right,
                location,
                ..
            } = *right
            {
                right = Box::new(AstNode::FieldStatement {
                    left,
                    right: Box::new(AstNode::FieldStatement {
                        left: inner_right,
                        right: inner,
                        value: None,
                        location: location.clone(),
                    }),
                    value: None,
                    location,
                })
            } else {
                right = Box::new(AstNode::FieldStatement {
                    left: right,
                    right: inner,
                    value: None, // Only the root may have a value
                    location,
                });
            }
        }

        match self.current_token().kind {
            TokenKind::Equal => {
                self.advance();
                let value_tokens = self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon]);

                value = Some(Box::new(
                    Statement::new(value_tokens, 0, &self.body, self.struct_pool.clone(), false)
                        .parse()
                        .0,
                ));
            }
            TokenKind::LeftBlockBrace => {
                return self.parse_offset_store(Some((
                    position,
                    AstNode::FieldStatement {
                        left: left.clone(),
                        right: right.clone(),
                        value,
                        location: location.clone(),
                    },
                )))
            }
            other if other.is_declarative() => {
                value = Some(Box::new(self.parse_declarative_node(
                    AstNode::FieldStatement {
                        left: left.clone(),
                        right: right.clone(),
                        value: None,
                        location: location.clone(),
                    },
                )));
            }
            other if other.is_arithmetic() => {
                self.position = position;
                return self.parse_arithmetic();
            }
            _ => {}
        }

        AstNode::FieldStatement {
            left,
            right,
            value,
            location,
        }
    }

    fn parse_declarative_node(&mut self, node: AstNode) -> AstNode {
        let operation = self.current_token();
        self.advance();

        let res = match operation.kind {
            TokenKind::AddOne => Some(TokenKind::Add),
            TokenKind::SubtractOne => Some(TokenKind::Subtract),
            _ => None,
        };

        if res.is_some() {
            let mapping = res.unwrap();

            if self.standalone {
                return AstNode::ArithmeticOperation {
                    left: Box::new(node),
                    right: Box::new(AstNode::LiteralStatement {
                        kind: TokenKind::IntegerLiteral,
                        value: ValueKind::Number(1),
                        location: self.current_token().location,
                    }),
                    operator: mapping,
                    location: self.current_token().location,
                };
            }

            self.body.borrow_mut().push(AstNode::LiteralStatement {
                kind: TokenKind::ExactLiteral,
                value: ValueKind::String("__<#insert#>__".to_owned()),
                location: self.current_token().location,
            });

            self.body.borrow_mut().push(AstNode::ArithmeticOperation {
                left: Box::new(node.clone()),
                right: Box::new(AstNode::LiteralStatement {
                    kind: TokenKind::IntegerLiteral,
                    value: ValueKind::Number(1),
                    location: self.current_token().location,
                }),
                operator: mapping,
                location: self.current_token().location,
            });

            return node;
        }

        let tokens = self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon]);
        let mapping = operation.kind.to_non_declarative();

        AstNode::ArithmeticOperation {
            left: Box::new(node.clone()),
            right: Box::new(
                Statement::new(tokens, 0, &self.body, self.struct_pool.clone(), false)
                    .parse()
                    .0,
            ),
            operator: mapping,
            location: self.current_token().location,
        }
    }

    fn yield_tokens_with_delimiters(&mut self, delimiters: Vec<TokenKind>) -> Vec<Token> {
        if delimiters.contains(&self.current_token().kind) {
            panic!(
                "{}",
                self.current_token().location.error(format!(
                    "Expected expression but got {:?}",
                    self.current_token().kind
                ))
            );
        }

        return self.yield_tokens_with_condition(|token, _| delimiters.contains(&token.kind));
    }

    fn yield_tokens_with_condition<F>(&mut self, mut condition: F) -> Vec<Token>
    where
        F: FnMut(Token, Token) -> bool,
    {
        let mut tokens = vec![];
        let mut previous;

        loop {
            tokens.push(self.current_token());
            previous = self.current_token().clone();

            let res = self.advance_opt();

            if condition(self.current_token().clone(), previous) {
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
                    let (node, position, tokens) = Statement::new(
                        self.tokens.clone(),
                        self.position.clone(),
                        &cell,
                        self.struct_pool.clone(),
                        false,
                    )
                    .parse();

                    let mut body_ref = cell.borrow_mut();
                    let res = body_ref.iter().position(|item| match item.clone() {
                        AstNode::LiteralStatement { kind, value, .. } => {
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
                            AstNode::DeclareStatement { .. } => body_ref[res.unwrap()] = node,
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
            AstNode::DeferStatement { value, .. } => {
                deferred.push(*value.clone());
                false
            }
            AstNode::LiteralStatement { kind, value, .. } => {
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

                if let Some(token) = next {
                    if token.kind == TokenKind::Identifier
                        && (self
                            .struct_pool
                            .contains(&token.value.get_string_inner().unwrap())
                            || token.value.is_base_type())
                    {
                        self.parse_type_conversion()
                    } else {
                        self.parse_wrapped_statement()
                    }
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
                    let next = self.next_token().expect(
                        &self
                            .current_token()
                            .location
                            .error("Unexpected EOF when parsing an identifier".to_string()),
                    );

                    if next.kind == TokenKind::LeftParenthesis {
                        self.parse_function()
                    } else if next.kind == TokenKind::LeftBlockBrace {
                        self.parse_offset_store(None)
                    } else if next.kind == TokenKind::LeftCurlyBrace {
                        self.parse_struct_init()
                    } else if next.kind == TokenKind::Dot {
                        let current = self.current_token();
                        let name = current.value.get_string_inner().unwrap();
                        let unexpected_error = |msg: String| {
                            current.location.error(format!("Expected a field access ({}.foo) or a function call ({}.(1)) but got {}", name, name, msg))
                        };

                        let tie = self
                            .next_token_seek(2)
                            .expect(&unexpected_error("EOF".to_string()));

                        match tie.kind {
                            TokenKind::Identifier => self.parse_field_access(None),
                            TokenKind::LeftParenthesis => self.parse_function(),
                            other => panic!("{}", unexpected_error(format!("{}", other))),
                        }
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
                            "{}",
                            self.current_token().location.error(format!(
                                "Expected left parenthesis or arithmetic, got {:?}",
                                self.current_token().kind
                            )),
                        )
                    }
                }
            }
            other if other.is_one_operator() => self.parse_prefix_increment(),
            _ => panic!(
                "{}",
                self.current_token().location.error(format!(
                    "Expected expression, got {:?}",
                    self.current_token().kind
                )),
            ),
        }
    }

    pub fn parse(&mut self) -> (AstNode, usize, Vec<Token>) {
        let node = match self.current_token().kind {
            TokenKind::Variadic => self.parse_variadic(),
            TokenKind::Return => self.parse_return(),
            TokenKind::If => self.parse_if_statement(),
            TokenKind::While => self.parse_while_statement(),
            TokenKind::For => self.parse_for_statement(),
            TokenKind::Defer => self.parse_defer(),
            other
                if other == TokenKind::Identifier
                    && (self
                        .struct_pool
                        .contains(&self.current_token().value.get_string_inner().unwrap())
                        || self.current_token().value.is_base_type()) =>
            {
                if let Some(token) = self.next_token() {
                    if token.kind == TokenKind::LeftCurlyBrace {
                        self.parse_struct_init()
                    } else {
                        self.parse_declare(None)
                    }
                } else {
                    self.parse_declare(None)
                }
            }
            _ => self.parse_expression(),
        };

        (node, self.position, self.tokens.clone())
    }
}
