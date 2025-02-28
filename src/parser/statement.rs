use std::cell::RefCell;
use std::iter::FromIterator;
use std::rc::Rc;

use super::enums::{Argument, AstNode, Primitive};
use super::parser::{create_generic_struct, StructPool};
use crate::lexer::enums::Attribute;
use crate::{
    compiler::enums::Type,
    ensure_fn_pointer,
    lexer::enums::{Location, Token, TokenKind, ValueKind},
    misc::colors::*,
    not_valid_struct_or_type, token_to_node, GENERIC_END, GENERIC_IDENTIFIER,
};
use crate::{elle_error, get_type, INTERNAL_IDX_FORMAT, INTERNAL_ITERATOR_FORMAT, LEN_CONSTANT};

#[derive(Clone, Copy)]
pub struct Shared<'a> {
    pub struct_pool: &'a RefCell<StructPool>,
    #[allow(unused)]
    pub tree: &'a RefCell<Vec<Primitive>>,
    pub generics: &'a Vec<String>,
    pub known_generics: &'a Vec<Type>,
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

    pub fn reverse(&mut self) {
        if !self.is_eof() {
            self.position -= 1;
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
            elle_error!(self.current_token().location.error(format!(
                "Expected one of [{}], got {:?}. {}",
                expected
                    .iter()
                    .map(|kind| format!("{:?}", kind))
                    .collect::<Vec<String>>()
                    .join(", "),
                self.current_token().kind,
                message.unwrap_or("")
            )))
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
            elle_error!(token.location.error(format!(
                "Expected one of {:?} but got {:?}",
                expected, token.kind
            )))
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
        self.get(vec![TokenKind::Identifier, TokenKind::ExactLiteral])
    }

    pub fn get_type(&mut self, generics: Option<&Vec<String>>) -> Type {
        get_type!(self, generics, self.shared.struct_pool, self.shared.tree)
    }

    fn parse_declare(&mut self, ty: Option<Option<Type>>) -> AstNode {
        let r#type = if let Some(ty) = ty {
            ty.clone()
        } else {
            let tmp = self.get_type(Some(self.shared.generics));
            self.advance();

            Some(tmp)
        };

        let name = self.get_identifier();
        let location = self.current_token().location.clone();

        self.advance();

        if self.current_token().kind == TokenKind::LeftBlockBrace {
            if r#type.as_ref().is_some_and(|x| x == &Type::Infer) {
                elle_error!(location.error("Cannot declare a buffer with an inferred inner type."));
            }

            return self.parse_buffer(Some(name), r#type);
        }

        if self.is_eof() || self.current_token().kind == TokenKind::Semicolon {
            return AstNode::Declare {
                name,
                r#type: r#type.clone(),
                // If the type is a struct create 'Struct {}' otherwise 0
                value: None,
                location: location.clone(),
                value_location: location,
            };
        }

        if self.current_token().kind == TokenKind::Colon {
            if r#type.clone().is_none_or(|ty| !ty.is_infer()) {
                elle_error!(
                    self.current_token()
                        .location
                        .with_extra_info(format!("Remove this colon to declare \"{name}\" explicitly"))
                        .error(format!(
                            "Cannot use \"{GREEN}:={RESET}\" to declare a variable with a non-inferred type.\nYou can remove the \"{GREEN}:{RESET}\" to declare a variable explicitly.",
                            GREEN = get_GREEN!(),
                            RESET = get_RESET!()
                        ))
                );
            }

            self.advance();
        }

        self.expect_tokens(vec![TokenKind::Equal]);
        self.advance();

        let value_location = self.current_token().location.clone();
        // Parser skips the first token
        let mut curly_nesting = (self.current_token().kind == TokenKind::LeftCurlyBrace) as i32;
        let mut block_nesting = (self.current_token().kind == TokenKind::LeftBlockBrace) as i32;

        let tokens = self.yield_tokens_with_condition(|token, _, _| {
            if token.kind == TokenKind::LeftCurlyBrace {
                curly_nesting += 1;
            }

            if token.kind == TokenKind::RightCurlyBrace {
                curly_nesting -= 1;
            }

            if token.kind == TokenKind::LeftBlockBrace {
                block_nesting += 1;
            }

            if token.kind == TokenKind::RightBlockBrace {
                block_nesting -= 1;
            }

            token.kind == TokenKind::Semicolon && curly_nesting == 0 && block_nesting == 0
        });

        let res = Statement::new(tokens, 0, &self.body, self.shared).parse().0;

        let parsed_res = match res.clone() {
            AstNode::Declare { name, .. } => {
                self.body.borrow_mut().push(res);

                AstNode::Literal {
                    kind: TokenKind::Identifier,
                    value: ValueKind::String(name),
                    location: location.clone(),
                }
            }
            _ => res,
        };

        AstNode::Declare {
            name,
            r#type,
            value: Some(Box::new(parsed_res)),
            location,
            value_location,
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

        AstNode::Declare {
            name: name.clone(),
            r#type: None,
            value: Some(Box::new(AstNode::BinaryOperation {
                left: Box::new(AstNode::Literal {
                    kind: TokenKind::Identifier,
                    value: ValueKind::String(name),
                    location: location.clone(),
                }),
                right: Box::new(Statement::new(tokens, 0, &self.body, self.shared).parse().0),
                operator: mapping,
                treat_as_string: true,
                dunder_methods: true,
                location: location.clone(),
            })),
            location: location.clone(),
            value_location: location,
        }
    }

    fn parse_float(&self, token: Token) -> AstNode {
        let value = match token.value {
            ValueKind::String(val) => val,
            _ => todo!(),
        };

        if !value.contains(".") {
            elle_error!(token.location.error("Invalid float literal provided"));
        }

        let nodes: Vec<&str> = value.split('.').collect();
        let left = nodes[0];
        let right = nodes[1];

        let exponent = right.len();
        let original = String::from_iter([left, right]).parse::<i128>().unwrap();

        AstNode::BinaryOperation {
            left: Box::new(AstNode::Literal {
                kind: TokenKind::FloatLiteral,
                value: ValueKind::Number(original),
                location: self.current_token().location,
            }),
            right: Box::new(AstNode::Literal {
                kind: TokenKind::FloatLiteral,
                value: ValueKind::Number(10_i128.pow(exponent as u32)),
                location: self.current_token().location,
            }),
            operator: TokenKind::Divide,
            treat_as_string: false,
            dunder_methods: true,
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
                    TokenKind::Question => {
                        let current = self.current_token();
                        self.advance();
                        self.parse_ternary_node(token_to_node!(current, self))
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
            return AstNode::Return {
                value: Box::new(AstNode::Literal {
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
            AstNode::Literal {
                kind: TokenKind::IntegerLiteral,
                value: ValueKind::Number(0),
                location: self.current_token().location,
            }
        };

        let parsed_res = match res.clone() {
            AstNode::Declare { name, .. } => {
                self.body.borrow_mut().push(res);

                AstNode::Literal {
                    kind: TokenKind::Identifier,
                    value: ValueKind::String(name),
                    location: self.current_token().location,
                }
            }
            _ => res,
        };

        AstNode::Return {
            value: Box::new(parsed_res),
            location,
        }
    }

    fn parse_function(
        &mut self,
        maybe_name: Option<(Rc<Location>, String)>,
        maybe_params: Option<Vec<(Rc<Location>, AstNode)>>,
        maybe_generics: Option<Vec<Type>>,
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

        let generics = if let Some(generics) = maybe_generics {
            generics
        } else {
            let mut tmp = vec![];

            if self.current_token().kind == TokenKind::LessThan {
                self.advance();

                while self.current_token().kind != TokenKind::GreaterThan {
                    tmp.push(self.get_type(Some(self.shared.generics)));
                    self.advance();

                    if self.current_token().kind == TokenKind::Comma {
                        self.advance();
                    }
                }

                self.expect_tokens(vec![TokenKind::GreaterThan]);
                self.advance();
            } else {
                tmp = self.shared.known_generics.clone();
            }

            tmp
        };

        if self.current_token().kind == TokenKind::Semicolon || self.is_eof() {
            return AstNode::Literal {
                kind: TokenKind::Identifier,
                value: ValueKind::String(name),
                location,
            };
        } else {
            self.expect_tokens(vec![TokenKind::LeftParenthesis]);
        }

        self.advance();

        let mut parameters = maybe_params.unwrap_or(vec![]);

        while self.current_token().kind != TokenKind::RightParenthesis && !self.is_eof() {
            let location = self.current_token().location.clone();

            let mut tokens = vec![];
            let mut paren_nesting = 0;
            let mut block_nesting = 0;
            let mut curly_nesting = 0;
            let mut generic_nesting = 0;

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

                // Generic of a funcall
                if self.current_token().kind == TokenKind::LessThan
                    && self.next_token().is_some_and(|token| {
                        let ty_name = token.value.get_string_inner().unwrap_or("".into());

                        token.value.is_base_type()
                            || self.shared.struct_pool.borrow().contains_key(&ty_name)
                            || self.shared.generics.contains(&ty_name)
                            || token.kind == TokenKind::LeftParenthesis
                    })
                {
                    generic_nesting += 1;
                }

                tokens.push(self.current_token());
                self.advance();

                if self.current_token().kind == TokenKind::Comma {
                    if paren_nesting > 0
                        || block_nesting > 0
                        || curly_nesting > 0
                        || generic_nesting > 0
                    {
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
                        elle_error!(self
                            .current_token()
                            .location
                            .error("Invalid balance of block braces"))
                    }
                }

                if self.current_token().kind == TokenKind::RightCurlyBrace {
                    if curly_nesting > 0 {
                        curly_nesting -= 1;
                    } else {
                        elle_error!(self
                            .current_token()
                            .location
                            .error("Invalid balance of curly braces"))
                    }
                }

                if self.current_token().kind == TokenKind::GreaterThan {
                    if generic_nesting > 0 {
                        generic_nesting -= 1;
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
            other if other.is_ternary_start() => return self.parse_ternary_node(expression),
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
        let mut ternary_nesting = 0;
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
                _ if token.kind.is_ternary_start() => {
                    ternary_nesting += 1;
                }
                _ if token.kind.is_ternary_end() => {
                    ternary_nesting -= 1;
                }
                TokenKind::Semicolon => {
                    break;
                }
                _ => {}
            }

            // Set the precedence to the last lowest precedence found.
            // If the expression is 1 + 2 * 3 + 4 * 5 for example,
            // it'll return the position of the second '+' token
            if token.kind.is_arithmetic()
                && token.kind.precedence() <= precedence
                && nesting == 0
                && ternary_nesting == 0
            {
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
            .position(|token| token.kind == TokenKind::Semicolon || token.kind.is_ternary_start())
        {
            if raw_right[index].kind.is_ternary_start() {
                index
            } else {
                index + 1
            }
        } else {
            raw_right.len()
        };

        // Separate the right-hand side expression up to a semicolon
        let right = raw_right[..right_end_index].to_vec();

        // Shift the position across the size of the expression
        self.position += left.len() + right_end_index;

        let node = AstNode::BinaryOperation {
            left: Box::new(Statement::new(left, 0, &self.body, self.shared).parse().0),
            right: Box::new(Statement::new(right, 0, &self.body, self.shared).parse().0),
            operator,
            treat_as_string: true,
            dunder_methods: true,
            location: self.current_token().location,
        };

        if self
            .next_token()
            .is_some_and(|token| token.kind.is_ternary_start())
        {
            self.advance();
            self.parse_ternary_node(node)
        } else {
            node
        }
    }

    fn parse_expression(&mut self) -> AstNode {
        let mut node = self.parse_primary();

        while self.current_token().kind.is_arithmetic() {
            let operator = self.current_token().kind;

            self.advance();

            let right = self.parse_primary();

            node = AstNode::BinaryOperation {
                left: Box::new(node),
                right: Box::new(right),
                operator,
                treat_as_string: true,
                dunder_methods: true,
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
        let mut location = (*self.current_token().location).clone();

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

        AstNode::Buffer {
            name,
            r#type: Some(ty.unwrap_or(Type::Byte)),
            size: Box::new(size.unwrap()),
            location: Rc::new(location),
        }
    }

    fn parse_array(&mut self, dynamic: bool) -> AstNode {
        let position = self.position.clone();
        let mut location = (*self.current_token().location).clone();
        self.expect_tokens(vec![TokenKind::LeftBlockBrace]);
        self.advance();

        let mut values = vec![];
        let mut inner_ty = None;

        while self.current_token().kind != TokenKind::RightBlockBrace && !self.is_eof() {
            let ty_name = self
                .current_token()
                .value
                .get_string_inner()
                .unwrap_or("".into());

            if dynamic
                && (self.current_token().kind == TokenKind::Identifier
                    && (self.shared.struct_pool.borrow().contains_key(&ty_name)
                        || self.shared.generics.contains(&ty_name)
                        || self.current_token().value.is_base_type())
                    && !self
                        .next_token()
                        .is_some_and(|token| token.kind == TokenKind::Colon)
                    || self.current_token().kind == TokenKind::LeftParenthesis)
            {
                inner_ty = Some(self.get_type(Some(self.shared.generics)));
                self.advance();

                if self.current_token().kind == TokenKind::Semicolon {
                    self.advance();

                    // if the token *is* now right block brace after getting a ty
                    // then break out early because there are no values
                    if self.current_token().kind == TokenKind::RightBlockBrace {
                        break;
                    }
                }
            }

            let location = self.current_token().location.clone();
            let mut tmp_tokens = vec![];
            let mut paren_nesting = 0;
            let mut block_nesting = 0;
            let mut curly_nesting = 0;
            let mut generic_nesting = 0;

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

                if self.current_token().kind == TokenKind::LessThan
                    && self.next_token().is_some_and(|token| {
                        let ty_name = token.value.get_string_inner().unwrap_or("".into());

                        token.value.is_base_type()
                            || self.shared.struct_pool.borrow().contains_key(&ty_name)
                            || self.shared.generics.contains(&ty_name)
                            || token.kind == TokenKind::LeftParenthesis
                    })
                {
                    generic_nesting += 1;
                }

                tmp_tokens.push(self.current_token());
                self.advance();

                if self.current_token().kind == TokenKind::Comma {
                    if paren_nesting > 0
                        || block_nesting > 0
                        || curly_nesting > 0
                        || generic_nesting > 0
                    {
                        // Comma in an inner function should just be added to the
                        // token list to be parsed
                        tmp_tokens.push(self.current_token());
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
                        elle_error!(self
                            .current_token()
                            .location
                            .error("Invalid balance of parenthesis"))
                    }
                }

                if self.current_token().kind == TokenKind::RightBlockBrace {
                    if block_nesting > 0 {
                        block_nesting -= 1;
                    } else {
                        break;
                    }
                }

                if self.current_token().kind == TokenKind::RightCurlyBrace {
                    if curly_nesting > 0 {
                        curly_nesting -= 1;
                    } else {
                        elle_error!(self
                            .current_token()
                            .location
                            .error("Invalid balance of curly braces"))
                    }
                }

                if self.current_token().kind == TokenKind::GreaterThan {
                    if generic_nesting > 0 {
                        generic_nesting -= 1;
                    }
                }

                if self.is_eof() {
                    break;
                }
            }

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
        let end_loc = self.current_token().location;

        if end_loc.ctx == location.ctx {
            location.length += end_loc.column - location.column;
            location.column += end_loc.column - location.column;
        }

        let array = AstNode::ArrayLiteral {
            values,
            explicit_inner: inner_ty.or(self.shared.known_generics.get(0).cloned()),
            known_generics: self.shared.known_generics.clone(),
            location: Rc::new(location.clone()),
            dynamic,
        };

        match self.current_token().kind {
            TokenKind::Dot => {
                return self.parse_field_access(Some((position, array, Rc::new(location.clone()))));
            }
            TokenKind::LeftBlockBrace => {
                return self.parse_offset_store(Some((position, array, Rc::new(location))));
            }
            other if other.is_ternary_start() => return self.parse_ternary_node(array),
            other if other.is_arithmetic() => {
                self.position = position;
                return self.parse_arithmetic();
            }
            _ => {}
        }

        array
    }

    fn parse_if_statement(&mut self) -> AstNode {
        self.advance();

        let tokens = self.yield_tokens_with_delimiters(vec![TokenKind::LeftCurlyBrace]);
        let expression = Statement::new(tokens, 0, &self.body, self.shared).parse().0;

        self.expect_tokens(vec![TokenKind::LeftCurlyBrace]);
        self.advance();

        let body = self.yield_block(false); // If statements are.. well.. statements
        let mut else_body: Vec<AstNode> = vec![];

        if self.current_token().kind == TokenKind::Else {
            self.advance();
            self.expect_tokens(vec![TokenKind::LeftCurlyBrace]);
            self.advance();

            else_body = self.yield_block(false); // Else is also a statement
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

        let body = self.yield_block(false); // While loops are statements

        self.position -= 1;

        AstNode::WhileLoopStatement {
            condition: Box::new(expression),
            step: None,
            body,
            location: self.current_token().location,
        }
    }

    fn parse_for_statement(&mut self) -> AstNode {
        self.advance();

        let mut wrapped = false;
        let position = self.position.clone();

        if self.current_token().kind == TokenKind::LeftParenthesis {
            let mut i = self.position;

            wrapped = 'x: {
                while !self.is_eof() && self.tokens[i].kind != TokenKind::LeftCurlyBrace {
                    if self.tokens[i].kind == TokenKind::In {
                        break 'x false;
                    }

                    i += 1;
                }

                true
            };

            self.advance();
        }

        let location = self.current_token().location.clone();

        let declare_tokens = if self.current_token().kind != TokenKind::Semicolon {
            self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon, TokenKind::In])
        } else {
            vec![]
        };

        if self.current_token().kind == TokenKind::In {
            self.position = position;

            if self
                .next_token()
                .is_some_and(|token| token.kind == TokenKind::In)
            {
                let name = self.get_identifier();
                self.advance();

                return self.parse_foreach_statement(Type::Infer, name, location);
            }

            let ty = self.get_type(Some(&self.shared.generics));
            self.advance();

            let name = self.get_identifier();
            self.advance();

            return self.parse_foreach_statement(ty, name, location);
        }

        let declare = if declare_tokens.len() > 0 {
            Statement::new(declare_tokens.clone(), 0, &self.body, self.shared)
                .parse()
                .0
        } else {
            AstNode::Literal {
                kind: TokenKind::IntegerLiteral,
                value: ValueKind::Number(0),
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
            AstNode::Literal {
                kind: TokenKind::IntegerLiteral,
                value: ValueKind::Number(1),
                location: self.current_token().location,
            }
        };

        self.expect_tokens(vec![TokenKind::Semicolon]);
        self.advance();

        let mut step_tokens = vec![];
        let mut nesting = 0;

        if self.current_token().kind
            != if wrapped {
                TokenKind::RightParenthesis
            } else {
                TokenKind::LeftCurlyBrace
            }
        {
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
            AstNode::Literal {
                kind: TokenKind::IntegerLiteral,
                value: ValueKind::Number(1),
                location: self.current_token().location,
            }
        };

        let body = self.yield_block(false); // For loops are statements
        let mut statements = vec![];

        self.position -= 1;

        if declare_tokens.len() > 0 {
            statements.push(declare);
        }

        statements.push(AstNode::WhileLoopStatement {
            condition: Box::new(condition),
            step: Some(Box::new(step)),
            body,
            location: self.current_token().location,
        });

        AstNode::BlockStatement {
            body: statements,
            location: self.current_token().location,
        }
    }

    /// for i32 x in Array::new<i32>(1, 2, 3) {}
    fn parse_foreach_statement(
        &mut self,
        ty: Type,
        name: String,
        location: Rc<Location>,
    ) -> AstNode {
        self.advance(); // in

        let mut nesting = 0;
        let tokens = self.yield_tokens_with_condition(|token, _, _| {
            if token.kind == TokenKind::LeftCurlyBrace {
                if nesting == 0 {
                    return true;
                } else {
                    nesting += 1;
                }
            }

            if token.kind == TokenKind::RightCurlyBrace {
                nesting -= 1;
            }

            return false;
        });

        let mut new_shared = (*self.shared).clone();
        let known_generics = if ty != Type::Infer {
            vec![ty.clone()]
        } else {
            vec![]
        };
        new_shared.known_generics = &known_generics;

        let iterator = Statement::new(tokens, 0, &self.body, &new_shared).parse().0;
        let index = format!(INTERNAL_IDX_FORMAT!(), name);
        let iter = format!(INTERNAL_ITERATOR_FORMAT!(), name);

        self.expect_tokens(vec![TokenKind::LeftCurlyBrace]);
        self.advance();

        let mut body = self.yield_block(false); // Foreach is a statement

        self.position -= 1;

        let index_node = AstNode::Literal {
            kind: TokenKind::Identifier,
            value: ValueKind::String(index.clone()),
            location: location.clone(),
        };

        let iterator_node = AstNode::Literal {
            kind: TokenKind::Identifier,
            value: ValueKind::String(iter.clone()),
            location: location.clone(),
        };

        let element_access = AstNode::MemoryOperation {
            left: Box::new(iterator_node.clone()),
            right: Box::new(index_node.clone()),
            value: None,
            left_location: location.clone(),
            right_location: location.clone(),
            value_location: location.clone(),
            is_deref: false,
        };

        let element_node = AstNode::Declare {
            name,
            r#type: Some(ty),
            value: Some(Box::new(element_access)),
            location: location.clone(),
            value_location: location.clone(),
        };

        let condition_node = AstNode::BinaryOperation {
            left: Box::new(index_node.clone()),
            right: Box::new(AstNode::FunctionCall {
                name: LEN_CONSTANT.into(),
                generics: vec![],
                parameters: vec![(location.clone(), iterator_node)],
                type_method: true,
                ignore_no_def: false,
                location: location.clone(),
            }),
            operator: TokenKind::LessThan,
            treat_as_string: false,
            dunder_methods: true,
            location: location.clone(),
        };

        let step_node = AstNode::Declare {
            name: index.clone(),
            r#type: None,
            value: Some(Box::new(AstNode::BinaryOperation {
                left: Box::new(index_node),
                right: Box::new(AstNode::Literal {
                    kind: TokenKind::IntegerLiteral,
                    value: ValueKind::Number(1),
                    location: location.clone(),
                }),
                operator: TokenKind::Add,
                treat_as_string: false,
                dunder_methods: true,
                location: location.clone(),
            })),
            location: location.clone(),
            value_location: location.clone(),
        };

        let mut statements = vec![];

        statements.push(AstNode::Declare {
            name: index,
            r#type: Some(Type::Word),
            value: Some(Box::new(AstNode::Literal {
                kind: TokenKind::IntegerLiteral,
                value: ValueKind::Number(0),
                location: location.clone(),
            })),
            location: location.clone(),
            value_location: location.clone(),
        });

        statements.push(AstNode::Declare {
            name: iter,
            r#type: Some(Type::Infer),
            value: Some(Box::new(iterator)),
            location: location.clone(),
            value_location: location.clone(),
        });

        body.insert(0, element_node);

        statements.push(AstNode::WhileLoopStatement {
            condition: Box::new(condition_node),
            step: Some(Box::new(step_node)),
            body,
            location: self.current_token().location,
        });

        AstNode::BlockStatement {
            body: statements,
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
            TokenKind::Question => expression = self.parse_ternary_node(expression),
            _ => {}
        }

        expression
    }

    fn parse_offset_store(&mut self, lhs: Option<(usize, AstNode, Rc<Location>)>) -> AstNode {
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

        let mut right_location = (*self.current_token().location).clone();
        let mut nesting = 0;
        let right_tokens = self.yield_tokens_with_condition(|token, _, _| {
            if token.kind == TokenKind::LeftBlockBrace {
                nesting += 1;
            }

            if token.kind == TokenKind::RightBlockBrace {
                if nesting > 0 {
                    nesting -= 1;
                } else {
                    return true;
                }
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

        let mut expression = AstNode::MemoryOperation {
            left: left.clone(),
            right: right.clone(),
            value: None,
            left_location: left_location.clone(),
            right_location: Rc::new(right_location.clone()),
            value_location: value_location.clone(),
            is_deref: false,
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

                expression = AstNode::MemoryOperation {
                    left: left.clone(),
                    right: right.clone(),
                    value,
                    left_location: left_location.clone(),
                    right_location: Rc::new(right_location.clone()),
                    value_location,
                    is_deref: false,
                };
            }
            other if other.is_declarative() => {
                value = Some(Box::new(self.parse_declarative_node(expression.clone())));

                expression = AstNode::MemoryOperation {
                    left: left.clone(),
                    right: right.clone(),
                    value,
                    left_location: left_location.clone(),
                    right_location: Rc::new(right_location.clone()),
                    value_location,
                    is_deref: false,
                };
            }
            TokenKind::Dot => {
                expression = self.parse_field_access(Some((position, expression, location)));
            }
            TokenKind::LeftBlockBrace => {
                expression = self.parse_offset_store(Some((position, expression, location)));
            }
            other if other.is_ternary_start() => return self.parse_ternary_node(expression),
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
        let location = self.current_token().location.clone();

        self.advance();
        AstNode::VariadicStart { name, location }
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

        let mut expression = AstNode::VariadicArgument {
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
            TokenKind::Question => {
                expression = self.parse_ternary_node(expression);
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

    fn parse_lambda(&mut self) -> AstNode {
        let location = self.current_token().location.clone();

        self.expect_tokens(vec![TokenKind::Function]);
        self.advance();

        self.expect_tokens(vec![TokenKind::LeftParenthesis]);
        self.advance();

        let mut arguments = vec![];

        while self.current_token().kind != TokenKind::RightParenthesis {
            if self.current_token().kind == TokenKind::Ellipsis {
                elle_error!(self
                    .current_token()
                    .location
                    .error("Cannot create a variadic lambda function..."))
            }

            let mut no_fmt = false;

            if self.current_token().kind == TokenKind::Attribute {
                self.advance();

                match self.current_token().parse_attribute() {
                    Attribute::NoFormat => {
                        no_fmt = true;
                        self.advance();
                    }
                    _ => {}
                };
            }

            let ty = self.get_type(Some(self.shared.generics));
            self.advance();

            let name = self.get_identifier();
            self.advance();

            if self.current_token().kind == TokenKind::Comma {
                self.advance();
            }

            arguments.push(Argument {
                r#type: ty,
                name,
                manual: false,
                no_fmt,
            });
        }

        self.expect_tokens(vec![TokenKind::RightParenthesis]);
        self.advance();

        if self.current_token().kind == TokenKind::LeftCurlyBrace {
            self.expect_tokens(vec![TokenKind::LeftCurlyBrace]);
            self.advance();

            let body = self.yield_block(true); // Lambdas are expressions
            self.position -= 1;

            AstNode::Lambda {
                arguments,
                value: body,
                location,
            }
        } else {
            let mut nesting = 0;
            let tokens = self.yield_tokens_with_condition(|_, token, next_token| {
                if token.kind == TokenKind::LeftParenthesis {
                    nesting += 1;
                }

                if token.kind == TokenKind::RightParenthesis {
                    if nesting > 0 {
                        nesting -= 1;
                    } else {
                        return true;
                    }
                }

                token.kind == TokenKind::Semicolon
                    || (nesting == 0
                        && (token.kind == TokenKind::Comma
                            || next_token
                                .is_some_and(|next| next.kind == TokenKind::RightCurlyBrace)))
            });

            let value = Statement::new(tokens, 0, &self.body, self.shared).parse().0;

            AstNode::Lambda {
                arguments,
                value: vec![AstNode::Return {
                    value: Box::new(value),
                    location: location.clone(),
                }],
                location,
            }
        }
    }

    fn parse_type_conversion(&mut self) -> AstNode {
        let position = self.position;
        self.advance();

        let location = self.current_token().location.clone();
        let r#type = self.get_type(Some(self.shared.generics));

        self.advance();

        if self.current_token().kind == TokenKind::Comma {
            self.position = position;
            return self.parse_declare(None);
        }

        self.expect_tokens(vec![TokenKind::RightParenthesis]);
        self.advance();

        let mut tokens = vec![];
        let mut nesting = 0;

        if self.current_token().kind == TokenKind::Semicolon {
            elle_error!(self
                .current_token()
                .location
                .error("Expected type conversion but got empty passthrough"))
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

        AstNode::Conversion {
            r#type: Some(r#type),
            value,
            location,
        }
    }

    fn parse_block(&mut self) -> AstNode {
        self.expect_tokens(vec![TokenKind::LeftCurlyBrace]);
        self.advance();

        let body = self.yield_block(false); // Blocks are statements
        self.position -= 1;
        AstNode::BlockStatement {
            body,
            location: self.current_token().location,
        }
    }

    fn parse_size(&mut self) -> AstNode {
        let position = self.position;
        self.expect_tokens(vec![TokenKind::Size]);
        self.advance();

        self.expect_tokens(vec![TokenKind::LeftParenthesis]);
        self.advance();

        let mut location = (*self.current_token().location).clone();
        let ty_name = self
            .current_token()
            .value
            .get_string_inner()
            .unwrap_or("".into());

        let value = if self.current_token().kind == TokenKind::Identifier
            && (self.shared.struct_pool.borrow().contains_key(&ty_name)
                || self.shared.generics.contains(&ty_name)
                || self.current_token().value.is_base_type()
                || self.current_token().kind == TokenKind::LeftParenthesis)
        {
            Ok(self.get_type(Some(&self.shared.generics)))
        } else {
            let mut tokens = vec![];
            let mut nesting = 0;

            if self.current_token().kind == TokenKind::Semicolon {
                elle_error!(self
                    .current_token()
                    .location
                    .error("Expected size directive but got empty passthrough"))
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

        let mut expression = AstNode::Size {
            value,
            location: Rc::new(location),
        };

        match self.current_token().kind {
            other if other.is_ternary_start() => {
                expression = self.parse_ternary_node(expression);
            }
            other if other.is_arithmetic() => {
                self.position = position;
                expression = self.parse_arithmetic();
            }
            _ => {}
        }

        expression
    }

    fn parse_array_length(&mut self) -> AstNode {
        let position = self.position;
        self.expect_tokens(vec![TokenKind::ArrayLength]);
        self.advance();

        self.expect_tokens(vec![TokenKind::LeftParenthesis]);
        self.advance();

        let mut location = (*self.current_token().location).clone();

        let mut tokens = vec![];
        let mut nesting = 0;

        if self.current_token().kind == TokenKind::Semicolon {
            elle_error!(self
                .current_token()
                .location
                .error("Expected array length directive but got empty passthrough"))
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

        let mut expression = AstNode::ArrayLength {
            value,
            location: Rc::new(location),
        };

        match self.current_token().kind {
            other if other.is_ternary_start() => {
                expression = self.parse_ternary_node(expression);
            }
            other if other.is_arithmetic() => {
                self.position = position;
                expression = self.parse_arithmetic();
            }
            _ => {}
        }

        expression
    }

    fn parse_unary(&mut self) -> AstNode {
        let token = self.current_token();
        let location = self.current_token().location.clone();
        self.advance();

        let tokens = self.yield_tokens_for_unary();
        let parsed = Box::new(Statement::new(tokens, 0, &self.body, self.shared).parse().0);
        let node = AstNode::BinaryOperation {
            left: parsed,
            right: Box::new(AstNode::token_to_literal(token)),
            operator: TokenKind::Multiply,
            treat_as_string: false,
            dunder_methods: true,
            location,
        };

        if self.current_token().kind.is_ternary_start() {
            self.parse_ternary_node(node)
        } else {
            node
        }
    }

    fn parse_not(&mut self) -> AstNode {
        self.advance();
        let location = self.current_token().location.clone();

        let tokens = self.yield_tokens_for_unary();
        let value = Box::new(Statement::new(tokens, 0, &self.body, self.shared).parse().0);
        let node = AstNode::LogicalNot { value, location };

        if self.current_token().kind.is_ternary_start() {
            self.parse_ternary_node(node)
        } else {
            node
        }
    }

    fn parse_bitwise_not(&mut self) -> AstNode {
        self.advance();
        let location = self.current_token().location.clone();

        let tokens = self.yield_tokens_for_unary();
        let value = Box::new(Statement::new(tokens, 0, &self.body, self.shared).parse().0);
        let node = AstNode::BitWiseNot { value, location };

        if self.current_token().kind.is_ternary_start() {
            self.parse_ternary_node(node)
        } else {
            node
        }
    }

    fn parse_address(&mut self) -> AstNode {
        self.advance();
        let location = self.current_token().location.clone();

        let tokens = self.yield_tokens_for_unary();
        let value = Box::new(Statement::new(tokens, 0, &self.body, self.shared).parse().0);
        let node = AstNode::Address { value, location };

        if self.current_token().kind.is_ternary_start() {
            self.parse_ternary_node(node)
        } else {
            node
        }
    }

    fn parse_deref(&mut self) -> AstNode {
        self.advance();

        let left_location = self.current_token().location.clone();
        let mut value = None;

        let tokens = self.yield_tokens_for_unary();
        let left = Box::new(Statement::new(tokens, 0, &self.body, self.shared).parse().0);

        let right_location = self.current_token().location.clone();
        let right = Box::new(AstNode::Literal {
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
                    AstNode::MemoryOperation {
                        left: left.clone(),
                        right: right.clone(),
                        value,
                        left_location: left_location.clone(),
                        right_location: right_location.clone(),
                        value_location: value_location.clone(),
                        is_deref: true,
                    },
                )));
            }
            _ => {}
        }

        AstNode::MemoryOperation {
            left,
            right,
            value,
            left_location,
            right_location,
            value_location,
            is_deref: true,
        }
    }

    fn parse_struct_init(&mut self) -> AstNode {
        let name = self.get_identifier();
        let location = self.current_token().location.clone();

        if !(self.shared.struct_pool.borrow().contains_key(&name)) {
            elle_error!(self.current_token().location.error(format!(
                "Struct named '{}' could not be found. Are you sure you typed it correctly?",
                name
            )))
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

        AstNode::StructLiteral {
            name,
            values,
            location,
        }
    }

    fn parse_field_access(&mut self, lhs: Option<(usize, AstNode, Rc<Location>)>) -> AstNode {
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

        let mut tmp = vec![];

        if self.current_token().kind == TokenKind::LessThan
            && self.next_token().is_some_and(|token| {
                if ![TokenKind::Identifier, TokenKind::LeftParenthesis].contains(&token.kind) {
                    return false;
                }

                let ty_name = token.value.get_string_inner().unwrap();

                self.shared.struct_pool.borrow().contains_key(&ty_name)
                    || self.shared.generics.contains(&ty_name)
                    || token.value.is_base_type()
                    || token.kind == TokenKind::LeftParenthesis
            })
        {
            self.advance();

            while self.current_token().kind != TokenKind::GreaterThan {
                tmp.push(self.get_type(Some(self.shared.generics)));
                self.advance();

                if self.current_token().kind == TokenKind::Comma {
                    self.advance();
                }
            }

            self.expect_tokens(vec![TokenKind::GreaterThan]);
            self.advance();
        } else {
            tmp = self.shared.known_generics.clone();
        }

        if self.current_token().kind == TokenKind::LeftParenthesis {
            return self.parse_function(
                Some((location.clone(), name)),
                Some(vec![(location.clone(), *left)]),
                if !tmp.is_empty() { Some(tmp) } else { None },
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

            if self.current_token().kind == TokenKind::LessThan
                && self.next_token().is_some_and(|token| {
                    if ![TokenKind::Identifier, TokenKind::LeftParenthesis].contains(&token.kind) {
                        return false;
                    }

                    let ty_name = token.value.get_string_inner().unwrap();

                    self.shared.struct_pool.borrow().contains_key(&ty_name)
                        || self.shared.generics.contains(&ty_name)
                        || token.value.is_base_type()
                        || token.kind == TokenKind::LeftParenthesis
                })
            {
                self.advance();

                while self.current_token().kind != TokenKind::GreaterThan {
                    tmp.push(self.get_type(Some(self.shared.generics)));
                    self.advance();

                    if self.current_token().kind == TokenKind::Comma {
                        self.advance();
                    }
                }

                self.expect_tokens(vec![TokenKind::GreaterThan]);
                self.advance();
            }

            if self.current_token().kind == TokenKind::LeftParenthesis {
                return self.parse_function(
                    Some((inner_location, name)),
                    Some(vec![(
                        location.clone(),
                        AstNode::FieldAccess {
                            left,
                            right,
                            value,
                            location,
                        },
                    )]),
                    if !tmp.is_empty() { Some(tmp) } else { None },
                    Some(position),
                    true,
                );
            }

            if let AstNode::FieldAccess {
                left,
                right: inner_right,
                location,
                ..
            } = *right
            {
                right = Box::new(AstNode::FieldAccess {
                    left,
                    right: Box::new(AstNode::FieldAccess {
                        left: inner_right,
                        right: inner,
                        value: None,
                        location: location.clone(),
                    }),
                    value: None,
                    location,
                })
            } else {
                right = Box::new(AstNode::FieldAccess {
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
                    if !tmp.is_empty() { Some(tmp) } else { None },
                    Some(position),
                    true,
                )
            }
            TokenKind::LeftBlockBrace => {
                return self.parse_offset_store(Some((
                    position,
                    AstNode::FieldAccess {
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
                    AstNode::FieldAccess {
                        left: left.clone(),
                        right: right.clone(),
                        value: None,
                        location: location.clone(),
                    },
                )));
            }
            other if other.is_ternary_start() => {
                return self.parse_ternary_node(AstNode::FieldAccess {
                    left,
                    right,
                    value,
                    location,
                })
            }
            other if other.is_arithmetic() => {
                self.position = position;
                return self.parse_arithmetic();
            }
            _ => {}
        }

        AstNode::FieldAccess {
            left,
            right,
            value,
            location,
        }
    }

    fn parse_ternary_node(&mut self, condition: AstNode) -> AstNode {
        self.expect_tokens(vec![TokenKind::Question]);
        self.advance();

        let location = self.current_token().location.clone();

        let if_true = Box::new(if self.current_token().kind == TokenKind::Colon {
            self.advance();
            condition.clone()
        } else {
            let mut nesting = 0;

            let tokens = self.yield_tokens_with_condition(|current, _, _| {
                if current.kind.is_ternary_start() {
                    nesting += 1;
                }

                if current.kind.is_ternary_end() {
                    if nesting > 0 {
                        nesting -= 1;
                    } else {
                        return true;
                    }
                }

                return false;
            });

            self.advance();
            Statement::new(tokens, 0, &self.body, self.shared).parse().0
        });

        let if_false = Box::new({
            let tokens = self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon]);
            Statement::new(tokens, 0, &self.body, self.shared).parse().0
        });

        AstNode::Ternary {
            condition: Box::new(condition),
            if_true,
            if_false,
            location,
        }
    }

    fn parse_indexof(&mut self) -> AstNode {
        let position = self.position;
        self.advance();
        self.expect_tokens(vec![TokenKind::LeftParenthesis]);
        self.advance();

        let name = self.get_identifier();

        self.advance();
        self.expect_tokens(vec![TokenKind::RightParenthesis]);
        self.advance();

        let location = self.current_token().location.clone();

        let mut expression = AstNode::Literal {
            kind: TokenKind::Identifier,
            value: ValueKind::String(format!(INTERNAL_IDX_FORMAT!(), name)),
            location: location.clone(),
        };

        match self.current_token().kind {
            TokenKind::Equal => {
                self.advance();
                let value_tokens = self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon]);

                expression = AstNode::Declare {
                    name: format!(INTERNAL_IDX_FORMAT!(), name),
                    r#type: None,
                    value: Some(Box::new(
                        Statement::new(value_tokens, 0, &self.body, self.shared)
                            .parse()
                            .0,
                    )),
                    location: location.clone(),
                    value_location: location.clone(),
                }
            }
            other if other.is_declarative() => {
                expression = AstNode::Declare {
                    name: format!(INTERNAL_IDX_FORMAT!(), name),
                    r#type: None,
                    value: Some(Box::new(self.parse_declarative_node(expression))),
                    location: location.clone(),
                    value_location: location.clone(),
                }
            }
            other if other.is_ternary_start() => {
                expression = self.parse_ternary_node(expression);
            }
            other if other.is_arithmetic() => {
                self.position = position;
                expression = self.parse_arithmetic();
            }
            _ => {}
        }

        expression
    }

    fn parse_env(&mut self) -> AstNode {
        let position = self.position;
        let location = self.current_token().location.clone();
        self.advance();

        let mut expression = AstNode::Environment {
            value: None,
            location: location.clone(),
        };

        match self.current_token().kind {
            TokenKind::Equal => {
                self.advance();
                let value_tokens = self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon]);

                expression = AstNode::Environment {
                    value: Some(Box::new(
                        Statement::new(value_tokens, 0, &self.body, self.shared)
                            .parse()
                            .0,
                    )),
                    location: location.clone(),
                }
            }

            other if other.is_declarative() => {
                expression = AstNode::Environment {
                    value: Some(Box::new(self.parse_declarative_node(expression))),
                    location: location.clone(),
                }
            }

            TokenKind::Dot => {
                expression = self.parse_field_access(Some((position, expression, location.clone())))
            }

            TokenKind::LeftBlockBrace => {
                expression = self.parse_offset_store(Some((position, expression, location.clone())))
            }

            TokenKind::Question => expression = self.parse_ternary_node(expression),
            _ => {}
        }

        expression
    }

    fn parse_alloc(&mut self) -> AstNode {
        let position = self.position;
        let location = self.current_token().location.clone();
        self.advance();

        self.expect_tokens(vec![TokenKind::LeftParenthesis]);
        self.advance();

        let ty = self.get_type(Some(self.shared.generics));
        self.advance();

        let count = if self.current_token().kind == TokenKind::Comma {
            self.advance();
            let mut nesting = (self.current_token().kind == TokenKind::LeftParenthesis) as i32;

            let tokens = self.yield_tokens_with_condition(|current, _, _| {
                if current.kind == TokenKind::LeftParenthesis {
                    nesting += 1;
                }

                if current.kind == TokenKind::RightParenthesis {
                    if nesting > 0 {
                        nesting -= 1;
                    } else {
                        return true;
                    }
                }

                return false;
            });

            self.advance();
            Statement::new(tokens, 0, &self.body, self.shared).parse().0
        } else {
            AstNode::Literal {
                kind: TokenKind::IntegerLiteral,
                value: ValueKind::Number(1),
                location: location.clone(),
            }
        };

        let mut expression = AstNode::Conversion {
            r#type: Some(Type::Pointer(Box::new(ty.clone()))),
            value: Box::new(AstNode::FunctionCall {
                name: "alloc".into(),
                generics: vec![],
                parameters: vec![
                    (
                        location.clone(),
                        AstNode::FieldAccess {
                            left: Box::new(AstNode::Environment {
                                value: None,
                                location: location.clone(),
                            }),
                            right: Box::new(AstNode::Literal {
                                kind: TokenKind::Identifier,
                                value: ValueKind::String("allocator".into()),
                                location: location.clone(),
                            }),
                            value: None,
                            location: location.clone(),
                        },
                    ),
                    (
                        location.clone(),
                        AstNode::BinaryOperation {
                            left: Box::new(AstNode::Size {
                                value: Ok(ty),
                                location: location.clone(),
                            }),
                            right: Box::new(count),
                            operator: TokenKind::Multiply,
                            treat_as_string: false,
                            dunder_methods: true,
                            location: location.clone(),
                        },
                    ),
                ],
                type_method: true,
                ignore_no_def: false,
                location: location.clone(),
            }),
            location: location.clone(),
        };

        match self.current_token().kind {
            TokenKind::Dot => {
                expression = self.parse_field_access(Some((position, expression, location.clone())))
            }

            TokenKind::LeftBlockBrace => {
                expression = self.parse_offset_store(Some((position, expression, location.clone())))
            }
            _ => {}
        }

        expression
    }

    fn parse_realloc(&mut self) -> AstNode {
        let position = self.position;
        let location = self.current_token().location.clone();
        self.advance();

        self.expect_tokens(vec![TokenKind::LeftParenthesis]);
        self.advance();

        let tokens = self.yield_tokens_with_delimiters(vec![TokenKind::Comma]);
        let ptr = Statement::new(tokens, 0, &self.body, self.shared).parse().0;
        self.advance();

        let ty = self.get_type(Some(self.shared.generics));
        self.advance();

        let count = if self.current_token().kind == TokenKind::Comma {
            self.advance();
            let mut nesting = (self.current_token().kind == TokenKind::LeftParenthesis) as i32;

            let tokens = self.yield_tokens_with_condition(|current, _, _| {
                if current.kind == TokenKind::LeftParenthesis {
                    nesting += 1;
                }

                if current.kind == TokenKind::RightParenthesis {
                    if nesting > 0 {
                        nesting -= 1;
                    } else {
                        return true;
                    }
                }

                return false;
            });

            self.advance();
            Statement::new(tokens, 0, &self.body, self.shared).parse().0
        } else {
            AstNode::Literal {
                kind: TokenKind::IntegerLiteral,
                value: ValueKind::Number(1),
                location: location.clone(),
            }
        };

        let mut expression = AstNode::Conversion {
            r#type: Some(Type::Pointer(Box::new(ty.clone()))),
            value: Box::new(AstNode::FunctionCall {
                name: "realloc".into(),
                generics: vec![],
                parameters: vec![
                    (
                        location.clone(),
                        AstNode::FieldAccess {
                            left: Box::new(AstNode::Environment {
                                value: None,
                                location: location.clone(),
                            }),
                            right: Box::new(AstNode::Literal {
                                kind: TokenKind::Identifier,
                                value: ValueKind::String("allocator".into()),
                                location: location.clone(),
                            }),
                            value: None,
                            location: location.clone(),
                        },
                    ),
                    (location.clone(), ptr),
                    (
                        location.clone(),
                        AstNode::BinaryOperation {
                            left: Box::new(AstNode::Size {
                                value: Ok(ty),
                                location: location.clone(),
                            }),
                            right: Box::new(count),
                            operator: TokenKind::Multiply,
                            treat_as_string: false,
                            dunder_methods: true,
                            location: location.clone(),
                        },
                    ),
                ],
                type_method: true,
                ignore_no_def: false,
                location: location.clone(),
            }),
            location: location.clone(),
        };

        match self.current_token().kind {
            TokenKind::Dot => {
                expression = self.parse_field_access(Some((position, expression, location.clone())))
            }

            TokenKind::LeftBlockBrace => {
                expression = self.parse_offset_store(Some((position, expression, location.clone())))
            }
            _ => {}
        }

        expression
    }

    fn parse_free(&mut self) -> AstNode {
        let location = self.current_token().location.clone();
        self.advance();
        self.expect_tokens(vec![TokenKind::LeftParenthesis]);
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
                if nesting > 0 && !self.is_eof() {
                    nesting -= 1;
                } else {
                    break;
                }
            }
        }

        self.advance();
        let ptr = Statement::new(tokens, 0, &self.body, self.shared).parse().0;

        AstNode::FunctionCall {
            name: "free".into(),
            generics: vec![],
            parameters: vec![
                (
                    location.clone(),
                    AstNode::FieldAccess {
                        left: Box::new(AstNode::Environment {
                            value: None,
                            location: location.clone(),
                        }),
                        right: Box::new(AstNode::Literal {
                            kind: TokenKind::Identifier,
                            value: ValueKind::String("allocator".into()),
                            location: location.clone(),
                        }),
                        value: None,
                        location: location.clone(),
                    },
                ),
                (location.clone(), ptr)
            ],
            type_method: true,
            ignore_no_def: false,
            location: location.clone(),
        }
    }

    fn parse_set_allocator(&mut self) -> AstNode {
        let location = self.current_token().location.clone();
        self.advance();
        self.expect_tokens(vec![TokenKind::LeftParenthesis]);
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
                if nesting > 0 && !self.is_eof() {
                    nesting -= 1;
                } else {
                    break;
                }
            }
        }

        self.advance();
        let allocator = Statement::new(tokens, 0, &self.body, self.shared).parse().0;

        AstNode::SetAllocator {
            value: Box::new(allocator),
            location,
        }
    }

    fn parse_reset_allocator(&mut self) -> AstNode {
        let location = self.current_token().location.clone();
        self.advance();

        self.expect_tokens(vec![TokenKind::LeftParenthesis]);
        self.advance();

        self.expect_tokens(vec![TokenKind::RightParenthesis]);
        self.advance();

        AstNode::SetAllocator {
            value: Box::new(AstNode::FieldAccess {
                left: Box::new(AstNode::Environment {
                    value: None,
                    location: location.clone(),
                }),
                right: Box::new(AstNode::Literal {
                    kind: TokenKind::Identifier,
                    value: ValueKind::String("default_allocator".into()),
                    location: location.clone(),
                }),
                value: None,
                location: location.clone(),
            }),
            location,
        }
    }

    fn parse_declarative_node(&mut self, node: AstNode) -> AstNode {
        let operation = self.current_token();
        let location = self.current_token().location.clone();
        self.advance();

        let tokens = self.yield_tokens_with_delimiters(vec![TokenKind::Semicolon]);
        let mapping = operation.kind.to_non_declarative();

        AstNode::BinaryOperation {
            left: Box::new(node.clone()),
            right: Box::new(Statement::new(tokens, 0, &self.body, self.shared).parse().0),
            operator: mapping,
            treat_as_string: true,
            dunder_methods: true,
            location,
        }
    }

    fn yield_tokens_for_unary(&mut self) -> Vec<Token> {
        let mut nesting = 0;
        let mut brace_nesting = 0;

        self.yield_tokens_with_condition(|token, prev_token, next_token| {
            if prev_token.kind == TokenKind::LeftParenthesis {
                nesting += 1;
            }

            if prev_token.kind == TokenKind::RightParenthesis && nesting > 0 {
                nesting -= 1;
            }

            if prev_token.kind == TokenKind::LeftCurlyBrace {
                brace_nesting += 1;
            }

            if prev_token.kind == TokenKind::RightCurlyBrace && brace_nesting > 0 {
                brace_nesting -= 1;
            }

            let ty_name = prev_token.value.get_string_inner().unwrap_or("".into());

            if token.kind.is_arithmetic() {
                if token.kind == TokenKind::LessThan && next_token.is_some() {
                    let next = next_token.unwrap();
                    let next_name = next.value.get_string_inner().unwrap_or("".into());
                    !(self.shared.struct_pool.borrow().contains_key(&next_name)
                        || self.shared.generics.contains(&next_name)
                        || next.value.is_base_type()
                        || next.kind == TokenKind::LeftParenthesis)
                } else if token.kind == TokenKind::GreaterThan {
                    !(self.shared.struct_pool.borrow().contains_key(&ty_name)
                        || self.shared.generics.contains(&ty_name)
                        || prev_token.value.is_base_type()
                        || prev_token.kind == TokenKind::LeftParenthesis)
                } else {
                    nesting == 0 && brace_nesting == 0
                }
            } else {
                (token.kind.is_declarative()
                    || token.kind == TokenKind::Semicolon
                    || token.kind == TokenKind::Equal
                    || token.kind == TokenKind::Question)
                    && brace_nesting == 0
            }
        })
    }

    fn yield_tokens_with_delimiters(&mut self, delimiters: Vec<TokenKind>) -> Vec<Token> {
        if delimiters.contains(&self.current_token().kind) {
            elle_error!(self.current_token().location.error(format!(
                "Expected expression but got {:?}",
                self.current_token().kind
            )));
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

    fn yield_block(&mut self, expect_semicolon: bool) -> Vec<AstNode> {
        let cell: RefCell<Vec<AstNode>> = RefCell::new(vec![]);

        loop {
            let current = self.current_token();

            match current.kind {
                TokenKind::RightCurlyBrace => {
                    self.advance();

                    if !self.is_eof() && expect_semicolon {
                        self.expect_tokens(vec![TokenKind::Semicolon]);
                    }

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
                    AstNode::Return { .. } => {
                        new_nodes.extend(deferred.clone());
                        new_nodes.push(node);
                        found_return = true;
                    }
                    AstNode::WhileLoopStatement {
                        condition,
                        step,
                        body,
                        location,
                    } => {
                        let mut new_body = body;
                        insert_deferred_statements(&mut new_body, deferred, false);

                        new_nodes.push(AstNode::WhileLoopStatement {
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
            token if token.is_literal() => {
                if let Some(next) = self.next_token() {
                    if next.kind == TokenKind::LeftParenthesis {
                        self.parse_function(None, None, None, None, false)
                    } else {
                        self.parse_literal()
                    }
                } else {
                    self.parse_literal()
                }
            }
            TokenKind::Unary => self.parse_unary(),
            TokenKind::Not => self.parse_not(),
            TokenKind::BitwiseNot => self.parse_bitwise_not(),
            TokenKind::Deref => self.parse_deref(),
            TokenKind::Address => self.parse_address(),
            TokenKind::Size => self.parse_size(),
            TokenKind::ArrayLength => self.parse_array_length(),
            TokenKind::IndexOf => self.parse_indexof(),
            TokenKind::Environment => self.parse_env(),
            TokenKind::Alloc => self.parse_alloc(),
            TokenKind::Realloc => self.parse_realloc(),
            TokenKind::Free => self.parse_free(),
            TokenKind::SetAllocator => self.parse_set_allocator(),
            TokenKind::ResetAllocator => self.parse_reset_allocator(),
            TokenKind::Let => {
                self.advance();
                self.parse_declare(Some(Some(Type::Infer)))
            }
            TokenKind::LeftParenthesis => {
                let next = self.next_token();

                if let Some(token) = next {
                    let ty_name = token.value.get_string_inner().unwrap_or("".into());

                    if token.kind == TokenKind::Identifier
                        && (self.shared.struct_pool.borrow().contains_key(&ty_name)
                            || self.shared.generics.contains(&ty_name)
                            || token.value.is_base_type()
                            || token.kind == TokenKind::LeftParenthesis)
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
            TokenKind::Hashtag => {
                self.advance();

                match self.current_token().kind {
                    TokenKind::LeftBlockBrace => self.parse_array(false),
                    _ => elle_error!(self.current_token().location.error(format!(
                        "Expected left block brace or identifier, got {:?}",
                        self.current_token().kind
                    ))),
                }
            }
            TokenKind::LeftCurlyBrace => self.parse_block(),
            TokenKind::LeftBlockBrace => self.parse_array(true),
            TokenKind::Identifier | TokenKind::ExactLiteral => {
                if self.is_eof()
                    || self
                        .next_token()
                        .is_some_and(|token| token.kind == TokenKind::Semicolon)
                {
                    self.parse_literal()
                } else {
                    let next = self.next_token().expect(
                        &self
                            .current_token()
                            .location
                            .error("Unexpected EOF when parsing an identifier"),
                    );

                    if next.kind == TokenKind::LeftParenthesis {
                        self.parse_function(None, None, None, None, false)
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
                            other => elle_error!(unexpected_error(tie, format!("{other:?}"))),
                        }
                    } else if next.kind == TokenKind::Equal {
                        self.parse_declare(Some(None))
                    } else if next.kind == TokenKind::Colon {
                        if self
                            .next_token_seek(2)
                            .is_some_and(|token| token.kind == TokenKind::Equal)
                        {
                            self.parse_declare(Some(Some(Type::Infer)))
                        } else {
                            elle_error!(next.location.error(
                                "Cannot use a colon in this context. What were you trying to do?"
                            ))
                        }
                    } else if next.kind.is_declarative() {
                        self.parse_declarative_like()
                    } else if next.kind == TokenKind::LessThan {
                        if let Some(token) = self.next_token_seek(2) {
                            let ty_name = token.value.get_string_inner().unwrap_or("".into());

                            if token.value.is_base_type()
                                || self.shared.struct_pool.borrow().contains_key(&ty_name)
                                || self.shared.generics.contains(&ty_name)
                                || token.kind == TokenKind::LeftParenthesis
                            {
                                self.parse_function(None, None, None, None, false)
                            } else {
                                self.parse_arithmetic()
                            }
                        } else {
                            self.parse_arithmetic()
                        }
                    } else if next.kind.is_arithmetic() {
                        self.parse_arithmetic()
                    } else if next.kind.is_ternary_start() {
                        let condition = AstNode::token_to_literal(self.current_token());
                        self.advance();
                        self.parse_ternary_node(condition)
                    } else if next.kind == TokenKind::Identifier {
                        not_valid_struct_or_type!(self)
                    } else if next.kind == TokenKind::DoubleColon {
                        not_valid_struct_or_type!(self)
                    } else {
                        elle_error!(next.location.error(format!(
                            "Expected left parenthesis or arithmetic, got {:?}",
                            next.kind
                        )))
                    }
                }
            }
            _ => elle_error!(self.current_token().location.error(format!(
                "Expected expression, got {:?}\nMaybe you forgot a semicolon nearby?",
                self.current_token().kind
            ))),
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
                let token = self.tokens.get(self.position - 2).unwrap_or(&prev);
                let mut location = (*token.location).clone();

                if location.column == location.ctx.len() - 1 {
                    location.column += 1;
                }

                location.ctx = Rc::from(format!("{} ", location.ctx));
                elle_error!(
                    location.error(format!("Expected semicolon here, but got {:?}", token.kind))
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
            TokenKind::Let => {
                self.advance();
                self.parse_declare(Some(Some(Type::Infer)))
            }
            // Lambda expression `fn(i32 a, i32 b) -> val`
            TokenKind::Function
                if self
                    .next_token()
                    .is_some_and(|next| next.kind == TokenKind::LeftParenthesis) =>
            {
                self.parse_lambda()
            }
            other
                if other == TokenKind::Identifier
                    && (self.shared.struct_pool.borrow().contains_key(&ty_name)
                        || self.shared.generics.contains(&ty_name)
                        || self.current_token().value.is_base_type()
                        || self.current_token().kind == TokenKind::LeftParenthesis)
                    || self.current_token().kind == TokenKind::Function =>
            {
                if let Some(token) = self.next_token() {
                    if token.kind == TokenKind::LeftCurlyBrace {
                        self.parse_struct_init()
                    } else if token.kind == TokenKind::Dot {
                        elle_error!(
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
                            elle_error!(method.location.error(format!(
                                "Expected method name in '{}::{}', but got '{:?}' instead.",
                                ty.value.get_string_inner().unwrap(),
                                method
                                    .value
                                    .get_string_inner()
                                    .unwrap_or(format!("{}", method.value)),
                                method.kind
                            )));
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
