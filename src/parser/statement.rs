use std::cell::RefCell;
use std::iter::FromIterator;

use super::enums::{AstNode, Primitive};
use super::parser::{create_generic_struct, StructPool};
use crate::{
    compiler::enums::Type,
    ensure_fn_pointer,
    lexer::enums::{Location, Token, TokenKind, ValueKind},
    not_valid_struct_or_type, token_to_node, GENERIC_END, GENERIC_IDENTIFIER,
};

pub struct Shared<'a> {
    pub struct_pool: &'a RefCell<StructPool>,
    pub extra_structs: &'a RefCell<Vec<Primitive>>,
    pub tree: &'a RefCell<Vec<Primitive>>,
    pub generics: &'a Vec<String>,
}

pub struct Statement<'a> {
    tokens: Vec<Token>,
    position: usize,
    body: &'a RefCell<Vec<AstNode>>,
    shared: &'a Shared<'a>,
}

impl<'a> Statement<'a> {
    pub fn new(
        tokens: Vec<Token>,
        position: usize,
        body: &'a RefCell<Vec<AstNode>>,
        shared: &'a Shared<'a>,
    ) -> Self {
        Statement {
            tokens,
            position,
            body,
            shared,
        }
    }

    pub fn advance(&mut self) {
        if !self.is_eof() {
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
        match self.position + seek > self.tokens.len() - 1 {
            true => None,
            false => Some(self.tokens[self.position + seek].clone()),
        }
    }

    fn is_eof(&mut self) -> bool {
        self.position + 1 >= self.tokens.len()
    }

    fn expect_tokens_with_message(&self, expected: Vec<TokenKind>, message: Option<&str>) {
        if !expected.contains(&self.current_token().kind) {
            panic!(
                "{}",
                self.current_token().location.error(format!(
                    "Expected one of [{}], got {:?}. {}",
                    expected
                        .iter()
                        .map(|kind| format!("{:?}", kind))
                        .collect::<Vec<String>>()
                        .join(", "),
                    self.current_token().kind,
                    message.unwrap_or("")
                )),
            )
        }
    }

    fn expect_tokens(&self, expected: Vec<TokenKind>) {
        self.expect_tokens_with_message(expected, None);
    }

    pub fn get(&mut self, expected: Vec<TokenKind>) -> String {
        let mut found = false;

        for kind in expected.clone().iter() {
            if &self.current_token().kind == kind {
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

    pub fn get_type(&mut self, generics: Option<&Vec<String>>) -> Type {
        let is_fn_pointer = self.current_token().kind == TokenKind::Function;
        let name = if is_fn_pointer {
            self.current_token().value.get_string_inner().unwrap()
        } else {
            self.get(vec![TokenKind::Identifier])
        };

        let is_struct = self.shared.struct_pool.borrow().contains_key(&name);

        let is_valid = is_fn_pointer
            || is_struct
            || generics.unwrap_or(&vec![]).contains(&name)
            || ValueKind::String(name.clone()).is_base_type();

        if !is_valid {
            panic!(
                "{}",
                self.current_token().location.error(format!(
                    "Type or struct named '{}' could not be found. Are you sure you spelt it correctly?",
                    name
                ))
            )
        }

        let mut ty = ValueKind::String(name.clone())
            .to_type_string(is_struct)
            .unwrap();
        let mut found_ptr = false;

        loop {
            let tmp = self.next_token();

            if tmp.is_some() {
                match tmp.unwrap().kind {
                    TokenKind::Multiply | TokenKind::Deref => {
                        found_ptr = true;
                        ty = Type::Pointer(Box::new(ty));
                        self.advance();
                    }
                    TokenKind::LessThan if is_struct => {
                        self.advance();
                        self.advance();

                        let mut known_generics = vec![];

                        while self.current_token().kind != TokenKind::GreaterThan {
                            known_generics.push(self.get_type(generics));
                            self.advance();

                            if self.current_token().kind == TokenKind::Comma {
                                self.advance();
                            }
                        }

                        let location = self.current_token().location;

                        let generic_name = format!(
                            "{name}.{GENERIC_IDENTIFIER}.{}.{GENERIC_END}",
                            known_generics
                                .iter()
                                .map(|known| known.to_internal_id().to_string())
                                .collect::<Vec<String>>()
                                .join(".")
                        );

                        if !self.shared.struct_pool.borrow().contains_key(&generic_name) {
                            create_generic_struct(
                                name.clone(),
                                generic_name.clone(),
                                location,
                                known_generics,
                                &self.shared.struct_pool,
                                &self.shared.extra_structs,
                            )
                        }

                        ty = Type::Struct(generic_name);
                        self.expect_tokens(vec![TokenKind::GreaterThan]);
                    }
                    // Crashes if it hasn't got at least 1 nested pointer for
                    // function pointers, ie `fn main(fn a)` is invalid
                    // you must have `fn main(fn *a)` instead.
                    _ => ensure_fn_pointer!(self, is_fn_pointer, found_ptr),
                }
            } else {
                ensure_fn_pointer!(self, is_fn_pointer, found_ptr)
            }
        }

        ty
    }

    fn parse_declare(&mut self, ty: Option<Option<Type>>) -> AstNode {
        let r#type = if ty.is_some() {
            ty.clone().unwrap()
        } else {
            let tmp = self.get_type(Some(self.shared.generics));
            self.advance();

            Some(tmp)
        };

        let name = self.get_identifier();
        let location = self.current_token().location.clone();

        self.advance();

        if self.current_token().kind == TokenKind::LeftBlockBrace {
            return self.parse_buffer(Some(name), r#type);
        }

        if self.is_eof() || self.current_token().kind == TokenKind::Semicolon {
            return AstNode::DeclareStatement {
                name,
                r#type: r#type.clone(),
                // If the type is a struct create 'Struct {}' otherwise 0
                value: Box::new(if r#type.clone().is_some_and(|ty| ty.is_struct()) {
                    AstNode::StructStatement {
                        name: r#type.unwrap().get_struct_inner().unwrap(),
                        values: vec![],
                        location: self.current_token().location,
                    }
                } else {
                    AstNode::LiteralStatement {
                        kind: TokenKind::IntegerLiteral,
                        value: ValueKind::Number(0),
                        location: self.current_token().location,
                    }
                }),
                location,
            };
        }

        self.expect_tokens(vec![TokenKind::Equal]);
        self.advance();

        let tokens = self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon]);

        let res = Statement::new(tokens, 0, &self.body, self.shared).parse().0;

        let parsed_res = match res.clone() {
            AstNode::DeclareStatement { name, .. } => {
                self.body.borrow_mut().push(res);

                AstNode::LiteralStatement {
                    kind: TokenKind::Identifier,
                    value: ValueKind::String(name),
                    location: location.clone(),
                }
            }
            _ => res,
        };

        AstNode::DeclareStatement {
            name,
            r#type,
            value: Box::new(parsed_res),
            location,
        }
    }

    fn parse_declarative_like(&mut self) -> AstNode {
        let name = self.get_identifier();
        let location = self.current_token().location.clone();

        self.advance();
        let operation = self.current_token();
        self.advance();

        let tokens = self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon]);
        let mapping = operation.kind.to_non_declarative();

        AstNode::DeclareStatement {
            name: name.clone(),
            r#type: None,
            value: Box::new(AstNode::ArithmeticOperation {
                left: Box::new(AstNode::LiteralStatement {
                    kind: TokenKind::Identifier,
                    value: ValueKind::String(name),
                    location: location.clone(),
                }),
                right: Box::new(Statement::new(tokens, 0, &self.body, self.shared).parse().0),
                operator: mapping,
                treat_as_string: true,
                location: location.clone(),
            }),
            location,
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
            treat_as_string: false,
            location: self.current_token().location,
        }
    }

    fn parse_literal(&mut self) -> AstNode {
        let position = self.position.clone();
        let location = self.current_token().location.clone();

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
                        self.parse_offset_store(Some((
                            position,
                            token_to_node!(current, self),
                            location,
                        )))
                    }
                    TokenKind::Dot => {
                        let current = self.current_token();
                        self.advance();
                        self.parse_field_access(Some((
                            position,
                            token_to_node!(current, self),
                            location,
                        )))
                    }
                    _ => self.parse_arithmetic(),
                },
                None => unreachable!(),
            }
        }
    }

    fn parse_return(&mut self) -> AstNode {
        self.advance();

        if self.current_token().kind == TokenKind::Semicolon {
            return AstNode::ReturnStatement {
                value: Box::new(AstNode::LiteralStatement {
                    kind: TokenKind::IntegerLiteral,
                    value: ValueKind::Number(0),
                    location: self.current_token().location.clone(),
                }),
                location: self.current_token().location,
            };
        }

        let location = self.current_token().location.clone();
        let tokens = self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon]);

        let res = if tokens.len() > 0 {
            Statement::new(tokens, 0, &self.body, self.shared).parse().0
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
            location,
        }
    }

    fn parse_function(
        &mut self,
        maybe_name: Option<(Location, String)>,
        maybe_params: Option<Vec<(Location, AstNode)>>,
        maybe_position: Option<usize>,
        type_method: bool,
    ) -> AstNode {
        let position = maybe_position.unwrap_or(self.position.clone());
        let (location, name) = if let Some((location, name)) = maybe_name {
            (location, name)
        } else {
            let tmp = self.get_identifier();
            let location = self.current_token().location.clone();
            self.advance();

            (location, tmp)
        };

        let mut generics = vec![];

        if self.current_token().kind == TokenKind::LessThan {
            self.advance();

            while self.current_token().kind != TokenKind::GreaterThan {
                generics.push(self.get_type(Some(self.shared.generics)));
                self.advance();

                if self.current_token().kind == TokenKind::Comma {
                    self.advance();
                }
            }

            self.expect_tokens(vec![TokenKind::GreaterThan]);
            self.advance();
        }

        self.expect_tokens(vec![TokenKind::LeftParenthesis]);
        self.advance();

        let mut parameters = maybe_params.unwrap_or(vec![]);

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
                                .error("Invalid balance of block braces")
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
                                .error("Invalid balance of curly braces")
                        )
                    }
                }

                if self.is_eof() {
                    break;
                }
            }

            parameters.push((
                location,
                Statement::new(tokens.clone(), 0, &self.body, self.shared)
                    .parse()
                    .0,
            ));
        }

        self.expect_tokens_with_message(
            vec![TokenKind::RightParenthesis],
            Some("Perhaps you forgot to close a nested expression?"),
        );

        self.advance();

        let mut expression = AstNode::FunctionCall {
            name: name.clone(),
            generics,
            parameters,
            type_method,
            ignore_no_def: false,
            location: location.clone(),
        };

        match self.current_token().kind {
            TokenKind::Dot => {
                expression = self.parse_field_access(Some((position, expression, location)))
            }
            TokenKind::LeftBlockBrace => {
                expression = self.parse_offset_store(Some((position, expression, location)))
            }
            other if other.is_arithmetic() => {
                self.position = position;
                return self.parse_arithmetic();
            }
            _ => {}
        }

        expression
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
            left: Box::new(Statement::new(left, 0, &self.body, self.shared).parse().0),
            right: Box::new(Statement::new(right, 0, &self.body, self.shared).parse().0),
            operator,
            treat_as_string: true,
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
                treat_as_string: true,
                location: self.current_token().location,
            };
        }

        node
    }

    fn parse_buffer(&mut self, name: Option<String>, ty: Option<Type>) -> AstNode {
        let name = if name.is_some() {
            name.unwrap()
        } else {
            let tmp = self.get_identifier();
            self.advance();

            tmp
        };

        self.expect_tokens(vec![TokenKind::LeftBlockBrace]);
        self.advance();

        let mut size = None;
        let mut location = self.current_token().location.clone();

        if self.current_token().kind != TokenKind::RightBlockBrace {
            let tokens = self.yield_tokens_with_condition(|token, _, _| {
                if token.kind == TokenKind::RightBlockBrace {
                    return true;
                }

                location.column += token.location.length;
                location.length += token.location.length;

                return false;
            });

            size = Some(Statement::new(tokens, 0, &self.body, self.shared).parse().0);
        }

        self.expect_tokens(vec![TokenKind::RightBlockBrace]);
        self.advance();
        self.expect_tokens(vec![TokenKind::Semicolon]);

        AstNode::BufferStatement {
            name,
            r#type: Some(ty.unwrap_or(Type::Byte)),
            size: Box::new(size.unwrap()),
            location,
        }
    }

    fn parse_array(&mut self) -> AstNode {
        let position = self.position.clone();
        let location = self.current_token().location.clone();
        self.expect_tokens(vec![TokenKind::LeftBlockBrace]);
        self.advance();

        let mut values = vec![];

        while self.current_token().kind != TokenKind::RightBlockBrace && !self.is_eof() {
            let location = self.current_token().location.clone();
            let tmp_tokens = self
                .yield_tokens_with_delimiters(vec![TokenKind::Comma, TokenKind::RightBlockBrace]);

            if self.current_token().kind == TokenKind::Comma {
                self.advance();
            }

            values.push((
                location,
                Statement::new(tmp_tokens.clone(), 0, &self.body, self.shared)
                    .parse()
                    .0,
            ));
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
            return self.parse_offset_store(Some((position, array, location)));
        }

        array
    }

    fn parse_if_statement(&mut self) -> AstNode {
        self.advance();

        let tokens = self.yield_tokens_with_delimiters(vec![TokenKind::LeftCurlyBrace]);
        let expression = Statement::new(tokens, 0, &self.body, self.shared).parse().0;

        self.expect_tokens(vec![TokenKind::LeftCurlyBrace]);
        self.advance();

        let body = self.yield_block();
        let mut else_body: Vec<AstNode> = vec![];

        if self.current_token().kind == TokenKind::Else {
            self.advance();
            self.expect_tokens(vec![TokenKind::LeftCurlyBrace]);
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
        let expression = Statement::new(tokens, 0, &self.body, self.shared).parse().0;

        self.expect_tokens(vec![TokenKind::LeftCurlyBrace]);
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
            Statement::new(declare_tokens.clone(), 0, &self.body, self.shared)
                .parse()
                .0
        } else {
            AstNode::LiteralStatement {
                kind: TokenKind::Not,
                value: ValueKind::Nil,
                location: self.current_token().location,
            }
        };

        self.expect_tokens(vec![TokenKind::Semicolon]);
        self.advance();

        let condition_tokens = if self.current_token().kind != TokenKind::Semicolon {
            self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon])
        } else {
            vec![]
        };

        let condition = if condition_tokens.len() > 0 {
            Statement::new(condition_tokens, 0, &self.body, self.shared)
                .parse()
                .0
        } else {
            AstNode::LiteralStatement {
                kind: TokenKind::IntegerLiteral,
                value: ValueKind::Number(1),
                location: self.current_token().location,
            }
        };

        self.expect_tokens(vec![TokenKind::Semicolon]);
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
            self.expect_tokens(vec![TokenKind::RightParenthesis]);
            self.advance();
        }

        self.expect_tokens(vec![TokenKind::LeftCurlyBrace]);
        self.advance();

        let step = if step_tokens.len() > 0 {
            Statement::new(step_tokens, 0, &self.body, self.shared)
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
        let location = self.current_token().location.clone();

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

        let mut expression = Statement::new(tokens, 0, &self.body, self.shared).parse().0;

        self.expect_tokens(vec![TokenKind::RightParenthesis]);
        self.advance();

        match self.current_token().kind {
            TokenKind::Dot => {
                expression = self.parse_field_access(Some((position, expression, location)))
            }
            TokenKind::LeftBlockBrace => {
                expression = self.parse_offset_store(Some((position, expression, location)))
            }
            _ => {}
        }

        expression
    }

    fn parse_offset_store(&mut self, lhs: Option<(usize, AstNode, Location)>) -> AstNode {
        let position = if lhs.is_some() {
            lhs.clone().unwrap().0
        } else {
            self.position.clone()
        };

        let location = if lhs.is_some() {
            lhs.clone().unwrap().2
        } else {
            self.current_token().location.clone()
        };

        let left_location = if lhs.is_some() {
            self.tokens[lhs.clone().unwrap().0].location.clone()
        } else {
            self.current_token().location.clone()
        };

        let value;

        let left_tokens = if lhs.is_some() {
            vec![]
        } else {
            self.yield_tokens_with_delimiters(vec![TokenKind::LeftBlockBrace])
        };

        let left = Box::new(if lhs.is_some() {
            lhs.unwrap().1
        } else {
            Statement::new(left_tokens, 0, &self.body, self.shared)
                .parse()
                .0
        });

        self.expect_tokens(vec![TokenKind::LeftBlockBrace]);
        self.advance();

        let mut right_location = self.current_token().location.clone();
        let right_tokens = self.yield_tokens_with_condition(|token, _, _| {
            if token.kind == TokenKind::RightBlockBrace {
                return true;
            }

            right_location.column += token.location.length;
            right_location.length += token.location.length;

            return false;
        });

        let right = Box::new(
            Statement::new(right_tokens, 0, &self.body, self.shared)
                .parse()
                .0,
        );

        self.expect_tokens(vec![TokenKind::RightBlockBrace]);
        self.advance();

        let mut value_location = self.current_token().location.clone();

        let mut expression = AstNode::MemoryStatement {
            left: left.clone(),
            right: right.clone(),
            value: None,
            left_location: left_location.clone(),
            right_location: right_location.clone(),
            value_location: value_location.clone(),
        };

        match self.current_token().kind {
            TokenKind::Equal => {
                self.advance();
                value_location = self.current_token().location.clone();
                let value_tokens = self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon]);

                value = Some(Box::new(
                    Statement::new(value_tokens, 0, &self.body, self.shared)
                        .parse()
                        .0,
                ));

                expression = AstNode::MemoryStatement {
                    left: left.clone(),
                    right: right.clone(),
                    value,
                    left_location: left_location.clone(),
                    right_location: right_location.clone(),
                    value_location,
                };
            }
            other if other.is_declarative() => {
                value = Some(Box::new(self.parse_declarative_node(expression.clone())));

                expression = AstNode::MemoryStatement {
                    left: left.clone(),
                    right: right.clone(),
                    value,
                    left_location: left_location.clone(),
                    right_location: right_location.clone(),
                    value_location,
                };
            }
            TokenKind::Dot => {
                expression = self.parse_field_access(Some((position, expression, location)));
            }
            TokenKind::LeftBlockBrace => {
                expression = self.parse_offset_store(Some((position, expression, location)));
            }
            other if other.is_arithmetic() => {
                self.position = position;
                return self.parse_arithmetic();
            }
            _ => {}
        }

        expression
    }

    fn parse_variadic(&mut self) -> AstNode {
        self.advance();
        let name = self.get_identifier();

        self.advance();
        self.expect_tokens(vec![TokenKind::LeftBlockBrace]);
        self.advance();
        let mut location = self.current_token().location.clone();

        let tokens = self.yield_tokens_with_condition(|token, _, _| {
            if token.kind == TokenKind::RightBlockBrace {
                return true;
            }

            location.column += token.location.length;
            location.length += token.location.length;

            return false;
        });

        let size = Box::new(if tokens.len() > 0 {
            Statement::new(tokens, 0, &self.body, self.shared).parse().0
        } else {
            panic!("Invalid size for buffer {}", name);
        });

        self.advance();

        AstNode::VariadicStatement {
            name,
            size,
            location,
        }
    }

    fn parse_yield_variadic(&mut self) -> AstNode {
        let position = self.position.clone();
        let name = self.get_identifier();

        self.advance();
        self.expect_tokens(vec![TokenKind::Dot]);
        self.advance();
        self.expect_tokens(vec![TokenKind::Yield]);
        self.advance();
        self.expect_tokens(vec![TokenKind::LeftParenthesis]);
        self.advance();

        let location = self.current_token().location;
        let r#type = self.get_type(Some(&self.shared.generics));
        self.advance();

        self.expect_tokens(vec![TokenKind::RightParenthesis]);
        self.advance();

        let mut expression = AstNode::NextStatement {
            name,
            r#type: Some(r#type),
            location: location.clone(),
        };

        match self.current_token().kind {
            TokenKind::Dot => {
                expression = self.parse_field_access(Some((position, expression, location)))
            }
            TokenKind::LeftBlockBrace => {
                expression = self.parse_offset_store(Some((position, expression, location)))
            }
            _ => {}
        }

        expression
    }

    fn parse_defer(&mut self) -> AstNode {
        self.advance();

        let tokens = self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon]);
        let value = Box::new(Statement::new(tokens, 0, &self.body, self.shared).parse().0);
        AstNode::DeferStatement {
            value,
            location: self.current_token().location,
        }
    }

    fn parse_type_conversion(&mut self) -> AstNode {
        self.advance();

        let location = self.current_token().location.clone();
        let r#type = self.get_type(Some(self.shared.generics));

        self.advance();
        self.expect_tokens(vec![TokenKind::RightParenthesis]);
        self.advance();

        let mut tokens = vec![];
        let mut nesting = 0;

        if self.current_token().kind == TokenKind::Semicolon {
            panic!(
                "{}",
                self.current_token()
                    .location
                    .error("Expected type conversion but got empty passthrough")
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

        let value = Box::new(Statement::new(tokens, 0, &self.body, self.shared).parse().0);

        AstNode::ConversionStatement {
            r#type: Some(r#type),
            value,
            location,
        }
    }

    fn parse_block(&mut self) -> AstNode {
        self.expect_tokens(vec![TokenKind::LeftCurlyBrace]);
        self.advance();

        let body = self.yield_block();
        self.position -= 1;
        AstNode::BlockStatement {
            body,
            location: self.current_token().location,
        }
    }

    fn parse_size(&mut self) -> AstNode {
        self.expect_tokens(vec![TokenKind::Size]);
        self.advance();

        self.expect_tokens(vec![TokenKind::LeftParenthesis]);
        self.advance();

        let mut location = self.current_token().location.clone();
        let ty_name = self
            .current_token()
            .value
            .get_string_inner()
            .unwrap_or("".into());

        let value = if self.current_token().kind == TokenKind::Identifier
            && (self.shared.struct_pool.borrow().contains_key(&ty_name)
                || self.shared.generics.contains(&ty_name)
                || self.current_token().value.is_base_type())
        {
            Ok(self.get_type(Some(&self.shared.generics)))
        } else {
            let mut tokens = vec![];
            let mut nesting = 0;

            if self.current_token().kind == TokenKind::Semicolon {
                panic!(
                    "{}",
                    self.current_token()
                        .location
                        .error("Expected size directive but got empty passthrough")
                )
            }

            loop {
                if self.current_token().kind == TokenKind::LeftParenthesis {
                    nesting += 1;
                }

                tokens.push(self.current_token());

                location.column += self.current_token().location.length;
                location.length += self.current_token().location.length;

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

                        location.column += self.current_token().location.length;
                        location.length += self.current_token().location.length;
                    }

                    break;
                }
            }

            let value = Box::new(Statement::new(tokens, 0, &self.body, self.shared).parse().0);
            Err(value)
        };

        self.advance();

        self.expect_tokens(vec![TokenKind::RightParenthesis]);
        self.advance();

        AstNode::SizeStatement {
            value,
            standalone: true,
            location,
        }
    }

    fn parse_array_length(&mut self) -> AstNode {
        self.expect_tokens(vec![TokenKind::ArrayLength]);
        self.advance();

        self.expect_tokens(vec![TokenKind::LeftParenthesis]);
        self.advance();

        let mut location = self.current_token().location.clone();

        let mut tokens = vec![];
        let mut nesting = 0;

        if self.current_token().kind == TokenKind::Semicolon {
            panic!(
                "{}",
                self.current_token()
                    .location
                    .error("Expected array length directive but got empty passthrough")
            )
        }

        loop {
            if self.current_token().kind == TokenKind::LeftParenthesis {
                nesting += 1;
            }

            tokens.push(self.current_token());

            location.column += self.current_token().location.length;
            location.length += self.current_token().location.length;

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

                    location.column += self.current_token().location.length;
                    location.length += self.current_token().location.length;
                }

                break;
            }
        }

        let value = Box::new(Statement::new(tokens, 0, &self.body, self.shared).parse().0);
        self.advance();

        self.expect_tokens(vec![TokenKind::RightParenthesis]);
        self.advance();

        AstNode::SizeStatement {
            value: Err(value),
            standalone: false,
            location,
        }
    }

    fn parse_unary(&mut self) -> AstNode {
        let token = self.current_token();
        let location = self.current_token().location.clone();
        self.advance();

        let tokens = self.yield_tokens_for_unary();
        let parsed = Box::new(Statement::new(tokens, 0, &self.body, self.shared).parse().0);

        AstNode::ArithmeticOperation {
            left: parsed,
            right: Box::new(AstNode::token_to_literal(token)),
            operator: TokenKind::Multiply,
            treat_as_string: false,
            location,
        }
    }

    fn parse_not(&mut self) -> AstNode {
        self.advance();
        let location = self.current_token().location.clone();

        let tokens = self.yield_tokens_for_unary();
        let value = Box::new(Statement::new(tokens, 0, &self.body, self.shared).parse().0);

        AstNode::NotStatement { value, location }
    }

    fn parse_bitwise_not(&mut self) -> AstNode {
        self.advance();
        let location = self.current_token().location.clone();

        let tokens = self.yield_tokens_for_unary();
        let value = Box::new(Statement::new(tokens, 0, &self.body, self.shared).parse().0);

        AstNode::BitwiseNotStatement { value, location }
    }

    fn parse_address(&mut self) -> AstNode {
        self.advance();
        let location = self.current_token().location.clone();

        let tokens = self.yield_tokens_for_unary();
        let value = Box::new(Statement::new(tokens, 0, &self.body, self.shared).parse().0);

        AstNode::AddressStatement { value, location }
    }

    fn parse_deref(&mut self) -> AstNode {
        self.advance();

        let left_location = self.current_token().location.clone();
        let mut value = None;

        let tokens = self.yield_tokens_for_unary();
        let left = Box::new(Statement::new(tokens, 0, &self.body, self.shared).parse().0);

        let right_location = self.current_token().location.clone();
        let right = Box::new(AstNode::LiteralStatement {
            kind: TokenKind::LongLiteral,
            value: ValueKind::Number(0),
            location: self.current_token().location,
        });

        let mut value_location = self.current_token().location.clone();

        match self.current_token().kind {
            TokenKind::Equal => {
                self.advance();
                value_location = self.current_token().location.clone();
                let value_tokens = self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon]);

                value = Some(Box::new(
                    Statement::new(value_tokens, 0, &self.body, self.shared)
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
                        left_location: left_location.clone(),
                        right_location: right_location.clone(),
                        value_location: value_location.clone(),
                    },
                )));
            }
            _ => {}
        }

        AstNode::MemoryStatement {
            left,
            right,
            value,
            left_location,
            right_location,
            value_location,
        }
    }

    fn parse_struct_init(&mut self) -> AstNode {
        let name = self.get_identifier();
        let location = self.current_token().location.clone();

        if !(self.shared.struct_pool.borrow().contains_key(&name)) {
            panic!(
                "{}",
                self.current_token().location.error(format!(
                    "Struct named '{}' could not be found. Are you sure you typed it correctly?",
                    name
                ))
            )
        }

        self.advance();
        self.expect_tokens(vec![TokenKind::LeftCurlyBrace]);
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
            self.expect_tokens(vec![TokenKind::Equal]);
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

            let value = Box::new(Statement::new(tokens, 0, &self.body, self.shared).parse().0);

            values.push((name, value));
        }

        AstNode::StructStatement {
            name,
            values,
            location,
        }
    }

    fn parse_field_access(&mut self, lhs: Option<(usize, AstNode, Location)>) -> AstNode {
        let location = if lhs.is_some() {
            lhs.clone().unwrap().2
        } else {
            self.current_token().location.clone()
        };

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
                Statement::new(left_tokens, 0, &self.body, self.shared)
                    .parse()
                    .0,
            )
        };

        self.expect_tokens(valid_tokens.clone());
        self.advance();

        self.expect_tokens(vec![TokenKind::Identifier]);

        let name = self.get_identifier();
        let mut right = Box::new(AstNode::token_to_literal(self.current_token()));

        self.advance();

        if self.current_token().kind == TokenKind::LeftParenthesis {
            return self.parse_function(
                Some((location.clone(), name)),
                Some(vec![(location.clone(), *left)]),
                Some(position),
                true,
            );
        }

        // Parse the rest of the field accesses
        while valid_tokens.contains(&self.current_token().kind) {
            self.advance(); // Ignore the TokenKind::Dot

            self.expect_tokens(vec![TokenKind::Identifier]);
            let inner_location = self.current_token().location.clone();

            let name = self.get_identifier();
            let inner = Box::new(AstNode::token_to_literal(self.current_token()));

            self.advance();

            if self.current_token().kind == TokenKind::LeftParenthesis {
                return self.parse_function(
                    Some((inner_location, name)),
                    Some(vec![(
                        location.clone(),
                        AstNode::FieldStatement {
                            left,
                            right,
                            value,
                            location,
                        },
                    )]),
                    Some(position),
                    true,
                );
            }

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
                    location: location.clone(),
                });
            }
        }

        match self.current_token().kind {
            TokenKind::Equal => {
                self.advance();
                let value_tokens = self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon]);

                value = Some(Box::new(
                    Statement::new(value_tokens, 0, &self.body, self.shared)
                        .parse()
                        .0,
                ));
            }
            // foo.a.meow() = meow(foo.a)
            TokenKind::LeftParenthesis => {
                return self.parse_function(
                    Some((location.clone(), name)),
                    Some(vec![(location.clone(), *left)]),
                    Some(position),
                    true,
                )
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
                    location,
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
        let location = self.current_token().location.clone();
        self.advance();

        let tokens = self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon]);
        let mapping = operation.kind.to_non_declarative();

        AstNode::ArithmeticOperation {
            left: Box::new(node.clone()),
            right: Box::new(Statement::new(tokens, 0, &self.body, self.shared).parse().0),
            operator: mapping,
            treat_as_string: true,
            location,
        }
    }

    fn yield_tokens_for_unary(&mut self) -> Vec<Token> {
        self.yield_tokens_with_condition(|token, prev_token, next_token| {
            let ty_name = prev_token.value.get_string_inner().unwrap_or("".into());

            if token.kind.is_arithmetic() {
                if token.kind == TokenKind::LessThan && next_token.is_some() {
                    let next = next_token.unwrap();
                    let next_name = next.value.get_string_inner().unwrap_or("".into());

                    !(self.shared.struct_pool.borrow().contains_key(&next_name)
                        || self.shared.generics.contains(&next_name)
                        || next.value.is_base_type())
                } else if token.kind == TokenKind::GreaterThan {
                    !(self.shared.struct_pool.borrow().contains_key(&ty_name)
                        || self.shared.generics.contains(&ty_name)
                        || prev_token.value.is_base_type())
                } else {
                    true
                }
            } else {
                token.kind.is_declarative()
                    || token.kind == TokenKind::Semicolon
                    || token.kind == TokenKind::Equal
            }
        })
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

        return self.yield_tokens_with_condition(|token, _, _| delimiters.contains(&token.kind));
    }

    fn yield_tokens_with_condition<F>(&mut self, mut condition: F) -> Vec<Token>
    where
        F: FnMut(Token, Token, Option<Token>) -> bool,
    {
        let mut tokens = vec![];
        let mut previous;

        loop {
            tokens.push(self.current_token());
            previous = self.current_token().clone();

            let res = self.advance_opt();

            if condition(self.current_token().clone(), previous, self.next_token()) {
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
                        self.shared,
                    )
                    .parse();

                    cell.borrow_mut().push(node);
                    self.position = position;
                    self.tokens = tokens;
                }
            };

            self.advance();
        }

        let mut res = cell.borrow_mut().to_owned().clone();
        let mut deferred: Vec<AstNode> = vec![];

        res.retain(|node| match node.clone() {
            AstNode::DeferStatement { value, .. } => {
                deferred.push(*value.clone());
                false
            }
            _ => true,
        });

        fn insert_deferred_statements(
            nodes: &mut Vec<AstNode>,
            deferred: &Vec<AstNode>,
            root: bool,
        ) {
            let mut new_nodes = vec![];
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
            TokenKind::BitwiseNot => self.parse_bitwise_not(),
            TokenKind::Deref => self.parse_deref(),
            TokenKind::Address => self.parse_address(),
            TokenKind::Size => self.parse_size(),
            TokenKind::ArrayLength => self.parse_array_length(),
            TokenKind::LeftParenthesis => {
                let next = self.next_token();

                if let Some(token) = next {
                    let ty_name = token.value.get_string_inner().unwrap_or("".into());

                    if token.kind == TokenKind::Identifier
                        && (self.shared.struct_pool.borrow().contains_key(&ty_name)
                            || self.shared.generics.contains(&ty_name)
                            || token.value.is_base_type())
                    {
                        let next = self.next_token_seek(2);

                        if let Some(next) = next {
                            if next.kind == TokenKind::LeftCurlyBrace
                                || next.kind == TokenKind::DoubleColon
                            {
                                self.parse_wrapped_statement()
                            } else {
                                self.parse_type_conversion()
                            }
                        } else {
                            self.parse_type_conversion()
                        }
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
                            .error("Unexpected EOF when parsing an identifier"),
                    );

                    if next.kind == TokenKind::LeftParenthesis {
                        self.parse_function(None, None, None, false)
                    } else if next.kind == TokenKind::LeftBlockBrace {
                        self.parse_offset_store(None)
                    } else if next.kind == TokenKind::LeftCurlyBrace {
                        self.parse_struct_init()
                    } else if next.kind == TokenKind::Dot {
                        let current = self.current_token();
                        let name = current.value.get_string_inner().unwrap();
                        let unexpected_error = |token: Token, msg: String| {
                            token.location.error(format!(
                                "Expected a field access ({}.foo) but got {}",
                                name, msg
                            ))
                        };

                        let tie = self
                            .next_token_seek(2)
                            .expect(&unexpected_error(next, "EOF".into()));

                        match tie.clone().kind {
                            TokenKind::Yield => self.parse_yield_variadic(),
                            TokenKind::Identifier => self.parse_field_access(None),
                            other => panic!("{}", unexpected_error(tie, format!("{:?}", other))),
                        }
                    } else if next.kind == TokenKind::Equal {
                        self.parse_declare(Some(None))
                    } else if next.kind.is_declarative() {
                        self.parse_declarative_like()
                    } else if next.kind == TokenKind::LessThan {
                        if let Some(token) = self.next_token_seek(2) {
                            let ty_name = token.value.get_string_inner().unwrap_or("".into());

                            if token.value.is_base_type()
                                || self.shared.struct_pool.borrow().contains_key(&ty_name)
                                || self.shared.generics.contains(&ty_name)
                            {
                                self.parse_function(None, None, None, false)
                            } else {
                                self.parse_arithmetic()
                            }
                        } else {
                            self.parse_arithmetic()
                        }
                    } else if next.kind.is_arithmetic() {
                        self.parse_arithmetic()
                    } else if next.kind == TokenKind::Identifier {
                        not_valid_struct_or_type!(self)
                    } else if next.kind == TokenKind::DoubleColon {
                        not_valid_struct_or_type!(self)
                    } else {
                        panic!(
                            "{}",
                            next.location.error(format!(
                                "Expected left parenthesis or arithmetic, got {:?}",
                                next.kind
                            )),
                        )
                    }
                }
            }
            _ => panic!(
                "{}",
                self.current_token().location.error(format!(
                    "Expected expression, got {:?}\nMaybe you forgot a semicolon nearby?",
                    self.current_token().kind
                )),
            ),
        }
    }

    pub fn parse(&mut self) -> (AstNode, usize, Vec<Token>) {
        if self.position >= 2 && self.tokens.len() > 1 {
            let prev = self.tokens[self.position - 1].clone();
            let kind = prev.kind.clone();

            if !(kind.is_arithmetic()
                || kind.is_declarative()
                || kind.is_brace()
                || kind == TokenKind::Semicolon)
            {
                let mut location = self
                    .tokens
                    .get(self.position - 2)
                    .unwrap_or(&prev)
                    .location
                    .clone();

                location.ctx.push(' ');
                location.column += 1;

                panic!(
                    "{}",
                    location.error(format!(
                        "Expected semicolon here, but got {:?}",
                        self.current_token().kind
                    ))
                )
            }
        }

        let ty_name = self
            .current_token()
            .value
            .get_string_inner()
            .unwrap_or("".into());

        let position = self.position.clone();

        let node = match self.current_token().kind {
            TokenKind::Variadic => self.parse_variadic(),
            TokenKind::Return => self.parse_return(),
            TokenKind::If => self.parse_if_statement(),
            TokenKind::While => self.parse_while_statement(),
            TokenKind::For => self.parse_for_statement(),
            TokenKind::Defer => self.parse_defer(),
            other
                if other == TokenKind::Identifier
                    && (self.shared.struct_pool.borrow().contains_key(&ty_name)
                        || self.shared.generics.contains(&ty_name)
                        || self.current_token().value.is_base_type()) =>
            {
                if let Some(token) = self.next_token() {
                    if token.kind == TokenKind::LeftCurlyBrace {
                        self.parse_struct_init()
                    } else if token.kind == TokenKind::Dot {
                        panic!(
                            "{}",
                            token.location.error(format!(
                                "Cannot access methods on a struct or type '{}' using '.'\nPlease use '::' for non-instance method access.",
                                self.current_token().value.get_string_inner().unwrap()
                            ))
                        )
                    } else if token.kind == TokenKind::DoubleColon {
                        let ty = self.current_token().clone();
                        let method =
                            self.next_token_seek(2)
                                .expect(&self.current_token().location.error(format!(
                                    "Expected method name after '{}::'",
                                    ty.value.get_string_inner().unwrap()
                                )));

                        if method.kind != TokenKind::Identifier {
                            panic!(
                                "{}",
                                method.location.error(format!(
                                    "Expected method name in '{}::{}', but got '{:?}' instead.",
                                    ty.value.get_string_inner().unwrap(),
                                    method
                                        .value
                                        .get_string_inner()
                                        .unwrap_or(format!("{}", method.value)),
                                    method.kind
                                ))
                            );
                        }

                        self.advance(); // Skip type
                        self.advance(); // Skip dot
                        self.advance(); // Skip func name

                        self.parse_function(
                            Some((
                                self.current_token().location,
                                format!(
                                    "{}.{}",
                                    ty.value.get_string_inner().unwrap(),
                                    method.value.get_string_inner().unwrap()
                                ),
                            )),
                            None,
                            Some(position),
                            false,
                        )
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
