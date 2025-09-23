#![allow(clippy::cognitive_complexity, clippy::single_match)]
use std::cell::RefCell;
use std::iter::FromIterator;
use std::rc::Rc;

use super::enums::{
    Address, Argument, ArrayLength, ArrayLiteral, AstNode, BinaryOperation, BitwiseNot, Buffer,
    Conversion, Declare, Environment, FieldAccess, FunctionCall, IfStatement, Lambda, Literal,
    LogicalNot, MemoryOperation, Primitive, Return, SetAllocator, Size, StructLiteral, Ternary,
    VariadicArgument, VariadicStart,
};

use super::parser::{create_generic_struct, EnumPool, StructPool};
use crate::compiler::qbe::r#type::Type;
use crate::lexer::enums::{Attribute, MutRc};
use crate::misc::constants::ITER_CONSTANT;
use crate::parser::enums::{BlockStatement, TupleDeclare, WhileLoopStatement};
use crate::{
    elle_error, enum_hover, expect_eot, get_type, is_type, set_end, INTERNAL_ITERATOR_FORMAT,
    INTERNAL_VALUE_FORMAT,
};
use crate::{
    lexer::enums::{Location, Token, TokenKind, ValueKind},
    misc::colors::*,
    not_valid_struct_or_type, token_to_node, GENERIC_END, GENERIC_IDENTIFIER,
};

#[derive(Clone, Copy)]
pub struct Shared<'a> {
    pub struct_pool: &'a RefCell<StructPool>,
    pub enum_pool: &'a RefCell<EnumPool>,
    #[allow(unused)]
    pub tree: &'a RefCell<Vec<Primitive>>,
    pub generics: &'a Vec<String>,
    pub known_generics: &'a Vec<Type>,
    pub addr_only: bool,
    pub tmp_counter: &'a RefCell<u64>,
}

pub struct Statement<'a> {
    tokens: Vec<Token>,
    position: usize,
    body: &'a RefCell<Vec<AstNode>>,
    shared: &'a Shared<'a>,
    consumed_addr: bool,
}

impl<'a> Statement<'a> {
    pub const fn new(
        tokens: Vec<Token>,
        position: usize,
        body: &'a RefCell<Vec<AstNode>>,
        shared: &'a Shared<'a>,
    ) -> Self {
        Self {
            tokens,
            position,
            body,
            shared,
            consumed_addr: false,
        }
    }

    pub const fn advance(&mut self) {
        if !self.is_eof() {
            self.position += 1;
        }
    }

    pub fn advance_opt(&mut self) -> Option<Token> {
        if self.is_eof() {
            None
        } else {
            self.position += 1;
            Some(self.current_token())
        }
    }

    fn current_token(&self) -> Token {
        self.tokens[self.position].clone()
    }

    fn next_token(&self) -> Option<Token> {
        if self.is_eof() {
            None
        } else {
            Some(self.tokens[self.position + 1].clone())
        }
    }

    fn next_token_seek(&self, seek: usize) -> Option<Token> {
        if self.position + seek > self.tokens.len() - 1 {
            None
        } else {
            Some(self.tokens[self.position + seek].clone())
        }
    }

    const fn is_eof(&self) -> bool {
        self.position + 1 >= self.tokens.len()
    }

    fn expect_tokens_with_message(&self, expected: &[TokenKind], message: Option<&str>) {
        if !expected.contains(&self.current_token().kind) {
            elle_error!(self.current_token().location.borrow().error(format!(
                "Expected one of [{}], got {:?}. {}",
                expected
                    .iter()
                    .map(|kind| format!("{kind:?}"))
                    .collect::<Vec<String>>()
                    .join(", "),
                self.current_token().kind,
                message.unwrap_or("")
            )))
        }
    }

    fn expect_tokens(&self, expected: &[TokenKind]) {
        self.expect_tokens_with_message(expected, None);
    }

    fn expect_identifier(&self) {
        self.expect_tokens(&[TokenKind::Identifier, TokenKind::ExactLiteral]);
    }

    pub fn get(&self, expected: &[TokenKind]) -> String {
        let mut found = false;

        for kind in expected {
            if &self.current_token().kind == kind {
                found = true;
                break;
            }
        }

        let token = self.current_token();

        if !found {
            elle_error!(token.location.borrow().error(format!(
                "Expected one of {:?} but got {:?}",
                expected, token.kind
            )))
        }

        let identifier = if let Token {
            value: ValueKind::String(identifier),
            ..
        } = self.current_token()
        {
            identifier
        } else {
            token.location.borrow().error(format!(
                "Expected one of {:?} but got {:?}",
                expected, token.kind
            ))
        };

        identifier
    }

    pub fn get_identifier(&self) -> String {
        self.get(&[TokenKind::Identifier, TokenKind::ExactLiteral])
    }

    pub fn get_type(&mut self, generics: Option<&Vec<String>>) -> Type {
        get_type!(
            self,
            generics,
            self.shared.struct_pool,
            self.shared.enum_pool,
            self.shared.tree
        )
    }

    fn parse_declare(&mut self, ty: Option<Option<Type>>) -> AstNode {
        let location = self.current_token().location;

        let r#type = if let Some(ty) = ty {
            ty
        } else {
            let tmp = self.get_type(Some(self.shared.generics));
            self.advance();

            Some(tmp)
        };

        if self.is_eof() {
            elle_error!(self
                .current_token()
                .location
                .borrow()
                .error("Expected identifier here but got EOF."));
        }

        self.expect_identifier();
        let name = self.current_token();

        if let Some(next) = self.next_token()
            && next.kind == TokenKind::Comma
        {
            return self.parse_tuple_declare(Some(r#type));
        }

        self.advance();

        if self.current_token().kind == TokenKind::LeftBlockBrace {
            if r#type.as_ref().is_some_and(|x| x == &Type::Infer) {
                elle_error!(location
                    .borrow()
                    .error("Cannot declare a buffer with an inferred inner type."));
            }

            return self.parse_buffer(Some(name), r#type, Some(location));
        }

        if self.is_eof() || self.current_token().kind == TokenKind::Semicolon {
            set_end!(location, self);

            return AstNode::Declare(Declare {
                name,
                r#type,
                value: None,
                location: location.clone(),
                value_location: location,
            });
        }

        if self.current_token().kind == TokenKind::Colon {
            if r#type.clone().is_none_or(|ty| !ty.is_infer()) {
                elle_error!(
                    self.current_token()
                        .location
                        .borrow()
                        .with_extra_info(format!("Remove this colon to declare \"{name}\" explicitly", name = name.value.get_string_inner().unwrap()))
                        .error(format!(
                            "Cannot use \"{GREEN}:={RESET}\" to declare a variable with a non-inferred type.\nYou can remove the \"{GREEN}:{RESET}\" to declare a variable explicitly.",
                            GREEN = get_GREEN!(),
                            RESET = get_RESET!()
                        ))
                );
            }

            self.advance();
        }

        self.expect_tokens(&[TokenKind::Equal]);
        self.advance();

        let value_location = self.current_token().location;
        let tokens = self.yield_tokens_wrapped_with_semi();
        let res = Statement::new(tokens, 0, self.body, self.shared).parse().0;

        set_end!(value_location, self);
        set_end!(location, self);

        let parsed_res = match res.clone() {
            AstNode::Declare(Declare { name, .. }) => {
                self.body.borrow_mut().push(res);
                token_to_node!(&name, self)
            }
            _ => res,
        };

        AstNode::Declare(Declare {
            name,
            r#type,
            value: Some(Box::new(parsed_res)),
            location,
            value_location,
        })
    }

    fn parse_declarative_like(&mut self) -> AstNode {
        let location = self.current_token().location;
        self.expect_identifier();
        let name = self.current_token();

        self.advance();
        let operation = self.current_token();
        self.advance();

        let value_location = self.current_token().location;
        let tokens = self.yield_tokens_wrapped_with_semi();
        let mapping = operation.kind.to_non_declarative();

        set_end!(value_location, self);
        set_end!(location, self);

        AstNode::Declare(Declare {
            name: name.clone(),
            r#type: None,
            value: Some(Box::new(AstNode::BinaryOperation(BinaryOperation {
                left: Box::new(token_to_node!(&name, self)),
                right: Box::new(Statement::new(tokens, 0, self.body, self.shared).parse().0),
                operator: mapping,
                treat_as_string: true,
                dunder_methods: true,
                location: location.clone(),
            }))),
            location,
            value_location,
        })
    }

    fn parse_tuple_declare(&mut self, existing_ty: Option<Option<Type>>) -> AstNode {
        let location = self.current_token().location;
        self.expect_identifier();
        let first = self.current_token();
        self.advance();

        self.expect_tokens(&[TokenKind::Comma]);
        self.advance();

        self.expect_identifier();
        let second = self.current_token();
        self.advance();

        let third = if self.current_token().kind == TokenKind::Comma {
            self.advance();

            self.expect_identifier();
            let third = self.current_token();
            self.advance();

            Some(third)
        } else {
            None
        };

        let ty = if self.current_token().kind == TokenKind::Colon && existing_ty.is_none() {
            self.advance();
            Some(Type::Infer)
        } else {
            existing_ty.unwrap_or(None)
        };

        self.expect_tokens(&[TokenKind::Equal]);
        self.advance();

        let value_location = self.current_token().location;
        let tokens = self.yield_tokens_wrapped_with_semi();
        let value = Statement::new(tokens, 0, self.body, self.shared).parse().0;

        set_end!(value_location, self);
        set_end!(location, self);

        AstNode::TupleDeclare(TupleDeclare {
            first,
            second,
            third,
            ty,
            value: Box::new(value),
            location,
            value_location,
        })
    }

    fn parse_float(&self, token: &Token) -> AstNode {
        let ValueKind::String(value) = &token.value else {
            todo!()
        };

        if !value.contains('.') {
            elle_error!(token
                .location
                .borrow()
                .error("Invalid float literal provided"));
        }

        if token.tagged {
            elle_error!(format!(
                "hover\n{}\n{}\n{}: f32", // TODO: is there any way to unhardcode this?
                self.current_token().location.borrow().display_plain(false),
                self.current_token().location.borrow().display_plain(true),
                value
            ));
        }

        let nodes: Vec<&str> = value.split('.').collect();
        let left = nodes[0];
        let right = nodes[1];

        let exponent = right.len();
        let original = String::from_iter([left, right]).parse::<i128>().unwrap();

        #[allow(clippy::cast_possible_truncation)]
        AstNode::BinaryOperation(BinaryOperation {
            left: Box::new(AstNode::Literal(Literal {
                kind: TokenKind::FloatLiteral,
                value: ValueKind::Number(original),
                location: token.location.clone(),
                tagged: false,
            })),
            right: Box::new(AstNode::Literal(Literal {
                kind: TokenKind::FloatLiteral,
                value: ValueKind::Number(10_i128.pow(exponent as u32)),
                location: token.location.clone(),
                tagged: false,
            })),
            operator: TokenKind::Divide,
            treat_as_string: false,
            dunder_methods: true,
            location: token.location.clone(),
        })
    }

    fn parse_literal(&mut self) -> AstNode {
        let location = self.current_token().location;
        let position = self.position;
        let current = self.current_token();

        if self.is_eof() {
            token_to_node!(&current, self)
        } else {
            match self.advance_opt() {
                Some(token) => match token.kind {
                    TokenKind::Semicolon => {
                        token_to_node!(&current, self)
                    }
                    TokenKind::LeftBlockBrace => self.parse_offset_store(Some((
                        position,
                        token_to_node!(&current, self),
                        location,
                    ))),
                    TokenKind::Dot => self.parse_field_access(Some((
                        position,
                        token_to_node!(&current, self),
                        location,
                    ))),
                    TokenKind::Question => {
                        self.parse_ternary_node(token_to_node!(&current, self), location)
                    }
                    other if other.is_arithmetic() => {
                        self.position = position;
                        self.parse_arithmetic()
                    }
                    _ => expect_eot!(token),
                },
                None => unreachable!(),
            }
        }
    }

    fn parse_return(&mut self) -> AstNode {
        let location = self.current_token().location;
        self.advance();

        if self.current_token().kind == TokenKind::Semicolon {
            return AstNode::Return(Return {
                value: Box::new(AstNode::Literal(Literal {
                    kind: TokenKind::IntegerLiteral,
                    value: ValueKind::Number(0),
                    location: location.clone(),
                    tagged: false,
                })),
                location,
            });
        }

        let tokens = self.yield_tokens_wrapped_with_semi();
        let res = if tokens.is_empty() {
            AstNode::Literal(Literal {
                kind: TokenKind::IntegerLiteral,
                value: ValueKind::Number(0),
                location: self.current_token().location,
                tagged: false,
            })
        } else {
            Statement::new(tokens, 0, self.body, self.shared).parse().0
        };

        let parsed_res = match res.clone() {
            AstNode::Declare(Declare { name, .. }) => {
                self.body.borrow_mut().push(res);
                token_to_node!(&name, self)
            }
            _ => res,
        };

        set_end!(location, self);

        AstNode::Return(Return {
            value: Box::new(parsed_res),
            location,
        })
    }

    fn parse_function(
        &mut self,
        maybe_name: Option<(MutRc<Location>, Token, Token, String)>,
        maybe_params: Option<Vec<(MutRc<Location>, AstNode)>>,
        maybe_generics: Option<Vec<Type>>,
        maybe_position: Option<usize>,
        type_method: bool,
    ) -> AstNode {
        let position = maybe_position.unwrap_or(self.position);
        let (location, namespace_token, name_token, name) =
            if let Some((location, namespace_token, name_token, name)) = maybe_name {
                (location, namespace_token, name_token, name)
            } else {
                let name_token = self.current_token();
                let tmp = self.get_identifier();
                let location = self.current_token().location;
                self.advance();

                (location, Token::from_ident(""), name_token, tmp)
            };

        let generics = if let Some(generics) = maybe_generics {
            generics
        } else {
            let mut tmp = vec![];

            if self.current_token().kind == TokenKind::LessThan {
                self.advance();

                while self.current_token().kind != TokenKind::GreaterThan && !self.is_eof() {
                    tmp.push(self.get_type(Some(self.shared.generics)));
                    self.advance();

                    if self.current_token().kind == TokenKind::Comma {
                        self.advance();
                    }
                }

                self.expect_tokens(&[TokenKind::GreaterThan]);
                self.advance();
            } else {
                tmp.clone_from(self.shared.known_generics);
            }

            tmp
        };

        if self.current_token().kind != TokenKind::LeftParenthesis {
            set_end!(location, self);

            let mut expression = AstNode::Literal(Literal {
                kind: TokenKind::Identifier,
                value: ValueKind::String(name),
                location: location.clone(),
                tagged: name_token.tagged,
            });

            if !self.is_eof() {
                match self.current_token().kind {
                    TokenKind::Dot => {
                        expression =
                            self.parse_field_access(Some((position, expression, location)));
                    }
                    TokenKind::LeftBlockBrace => {
                        expression =
                            self.parse_offset_store(Some((position, expression, location)));
                    }
                    TokenKind::Semicolon => {}
                    other if other.is_ternary_start() => {
                        return self.parse_ternary_node(expression, location);
                    }
                    other if other.is_arithmetic() => {
                        self.position = position;
                        return self.parse_arithmetic();
                    }
                    _ => expect_eot!(self.current_token()),
                }
            }

            return expression;
        }

        self.expect_tokens(&[TokenKind::LeftParenthesis]);
        self.advance();

        let mut parameters = maybe_params.unwrap_or_default();

        while self.current_token().kind != TokenKind::RightParenthesis && !self.is_eof() {
            let item_location = self.current_token().location;
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
                if self.current_token().kind == TokenKind::LessThan && self.is_type_contextually(1)
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
                            .borrow()
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
                            .borrow()
                            .error("Invalid balance of curly braces"))
                    }
                }

                if self.current_token().kind == TokenKind::GreaterThan && generic_nesting > 0 {
                    generic_nesting -= 1;
                }

                if self.is_eof() {
                    break;
                }
            }

            set_end!(item_location, self);

            parameters.push((
                item_location,
                Statement::new(tokens.clone(), 0, self.body, self.shared)
                    .parse()
                    .0,
            ));
        }

        self.expect_tokens_with_message(
            &[TokenKind::RightParenthesis],
            Some("Perhaps you forgot to close a nested expression?"),
        );
        set_end!(location, self);

        let mut expression = AstNode::FunctionCall(FunctionCall {
            namespace_token,
            name_token,
            name,
            generics,
            parameters,
            type_method,
            ignore_no_def: false,
            location: location.clone(),
        });

        if let Some(token) = self.advance_opt() {
            match token.kind {
                TokenKind::Dot => {
                    expression = self.parse_field_access(Some((position, expression, location)));
                }
                TokenKind::LeftBlockBrace => {
                    expression = self.parse_offset_store(Some((position, expression, location)));
                }
                TokenKind::Semicolon => {}
                other if other.is_ternary_start() => {
                    return self.parse_ternary_node(expression, location);
                }
                other if other.is_arithmetic() => {
                    self.position = position;
                    return self.parse_arithmetic();
                }
                _ => expect_eot!(token),
            }
        }

        expression
    }

    fn find_lowest_precedence(&self) -> usize {
        let tokens = self.tokens.clone();
        let mut precedence = TokenKind::highest_precedence();
        let mut precedence_index = 0;
        let mut nesting = 0;
        let mut curly_nesting = 0;
        let mut block_nesting = 0;
        let mut ternary_nesting = 0;
        let mut index = self.position;

        loop {
            if index >= tokens.len() - 1 {
                break;
            }

            let token = tokens[index].clone();

            match token.kind {
                TokenKind::LeftParenthesis => nesting += 1,
                TokenKind::RightParenthesis if nesting > 0 => nesting -= 1,
                TokenKind::LeftBlockBrace => block_nesting += 1,
                TokenKind::RightBlockBrace if block_nesting > 0 => block_nesting -= 1,
                TokenKind::LeftCurlyBrace => curly_nesting += 1,
                TokenKind::RightCurlyBrace if curly_nesting > 0 => curly_nesting -= 1,
                _ if token.kind.is_ternary_start() => ternary_nesting += 1,
                _ if token.kind.is_ternary_end()
                    && ternary_nesting > 0
                    && tokens
                        .get(index + 1)
                        .is_some_and(|token| token.kind != TokenKind::Equal) =>
                {
                    ternary_nesting -= 1
                }
                TokenKind::Semicolon if block_nesting == 0 && curly_nesting == 0 => break,
                _ => {}
            }

            // Set the precedence to the last lowest precedence found.
            // If the expression is 1 + 2 * 3 + 4 * 5 for example,
            // it'll return the position of the second '+' token
            if token.kind.is_arithmetic()
                && token.kind.precedence() <= precedence
                && nesting == 0
                && block_nesting == 0
                && curly_nesting == 0
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
        let location = self.current_token().location;
        let position = self.find_lowest_precedence();
        let operator = self.tokens[position].clone().kind;

        let tokens = self.tokens.clone();
        let left =
            tokens[self.position..=if position > 0 { position - 1 } else { position }].to_vec();

        let mut raw_right = tokens[position..tokens.len()].to_vec();

        raw_right.remove(0); // Get rid of the operator

        let mut paren_nesting = 0;
        let mut block_nesting = 0;
        let mut curly_nesting = 0;
        let mut position = None;

        for (i, token) in raw_right.iter().enumerate() {
            if token.kind == TokenKind::LeftParenthesis {
                paren_nesting += 1;
            }

            if token.kind == TokenKind::RightParenthesis {
                paren_nesting -= 1;
            }

            if token.kind == TokenKind::LeftBlockBrace {
                block_nesting += 1;
            }

            if token.kind == TokenKind::RightBlockBrace {
                block_nesting -= 1;
            }

            if token.kind == TokenKind::LeftCurlyBrace {
                curly_nesting += 1;
            }

            if token.kind == TokenKind::RightCurlyBrace {
                curly_nesting -= 1;
            }

            if (token.kind == TokenKind::Semicolon || token.kind.is_ternary_start())
                && paren_nesting == 0
                && block_nesting == 0
                && curly_nesting == 0
            {
                position = Some(i);
                break;
            }
        }

        let right_end_index = position.map_or(raw_right.len(), |index| {
            if raw_right[index].kind.is_ternary_start() {
                index
            } else {
                index + 1
            }
        });

        // Separate the right-hand side expression up to a semicolon
        let right = raw_right[..right_end_index].to_vec();

        // Shift the position across the size of the expression
        self.position += left.len() + right_end_index;
        set_end!(location, self);

        let node = AstNode::BinaryOperation(BinaryOperation {
            left: Box::new(Statement::new(left, 0, self.body, self.shared).parse().0),
            right: Box::new(Statement::new(right, 0, self.body, self.shared).parse().0),
            operator,
            treat_as_string: true,
            dunder_methods: true,
            location: location.clone(),
        });

        if self
            .next_token()
            .is_some_and(|token| token.kind.is_ternary_start())
        {
            self.advance();
            self.parse_ternary_node(node, location)
        } else {
            node
        }
    }

    fn parse_expression(&mut self) -> AstNode {
        let location = self.current_token().location;
        let mut node = self.parse_primary();

        while self.current_token().kind.is_arithmetic() && !self.is_eof() {
            let operator = self.current_token().kind;

            self.advance();

            let right = self.parse_primary();

            set_end!(location, self);

            node = AstNode::BinaryOperation(BinaryOperation {
                left: Box::new(node),
                right: Box::new(right),
                operator,
                treat_as_string: true,
                dunder_methods: true,
                location: location.clone(),
            });
        }

        node
    }

    fn parse_buffer(
        &mut self,
        name: Option<Token>,
        ty: Option<Type>,
        loc: Option<MutRc<Location>>,
    ) -> AstNode {
        let location = loc.unwrap_or_else(|| self.current_token().location);

        let name = name.map_or_else(
            || {
                self.expect_identifier();
                let tmp = self.current_token();
                self.advance();

                tmp
            },
            |name| name,
        );

        self.expect_tokens(&[TokenKind::LeftBlockBrace]);
        self.advance();

        let size;

        if self.current_token().kind == TokenKind::RightBlockBrace {
            elle_error!(self.current_token().location.borrow().error(format!(
                "Expected an expression but got: {:?}",
                self.current_token().kind
            )))
        } else {
            let tokens = self.yield_tokens_with_delimiters(&[TokenKind::RightBlockBrace]);
            size = Some(Statement::new(tokens, 0, self.body, self.shared).parse().0);
        }

        self.expect_tokens(&[TokenKind::RightBlockBrace]);
        self.advance();
        self.expect_tokens(&[TokenKind::Semicolon]);
        set_end!(location, self);

        AstNode::Buffer(Buffer {
            name,
            r#type: Some(ty.unwrap_or(Type::Byte)),
            size: Box::new(size.unwrap()),
            location,
        })
    }

    fn parse_array(&mut self, dynamic: bool) -> AstNode {
        let location = self.current_token().location;
        let position = self.position;
        self.expect_tokens(&[TokenKind::LeftBlockBrace]);
        self.advance();

        let mut values = vec![];
        let mut inner_ty = None;

        while self.current_token().kind != TokenKind::RightBlockBrace && !self.is_eof() {
            if dynamic
                && (self.is_type_contextually(0)
                    // namespaced call
                    && !self
                        .next_token()
                        .is_some_and(|token| token.kind == TokenKind::DoubleColon)
                    // struct literal
                    && !self
                        .next_token()
                        .is_some_and(|token| token.kind == TokenKind::LeftCurlyBrace))
            {
                let loc = self.current_token().location;
                let ty = self.get_type(Some(self.shared.generics));

                set_end!(loc, self);

                elle_error!(loc.borrow().error(format!(
                    "Cannot add an explicit array type here. \nNOTE: `{RED}[{res};]{RESET}` syntax has been removed in favour of `{GREEN}[]{res}{RESET}`.",
                        res = ty.display(), RED = get_RED!(), GREEN = get_GREEN!(), RESET = get_RESET!())));
            }

            let item_location = self.current_token().location;
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

                if self.current_token().kind == TokenKind::LessThan && self.is_type_contextually(1)
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
                            .borrow()
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
                            .borrow()
                            .error("Invalid balance of curly braces"))
                    }
                }

                if self.current_token().kind == TokenKind::GreaterThan && generic_nesting > 0 {
                    generic_nesting -= 1;
                }

                if self.is_eof() {
                    break;
                }
            }

            set_end!(item_location, self);
            if self.current_token().kind == TokenKind::Comma {
                self.advance();
            }

            values.push((
                item_location,
                Statement::new(tmp_tokens.clone(), 0, self.body, self.shared)
                    .parse()
                    .0,
            ));
        }

        self.expect_tokens(&[TokenKind::RightBlockBrace]);
        set_end!(location, self);

        macro_rules! expression {
            () => {
                AstNode::ArrayLiteral(ArrayLiteral {
                    values,
                    explicit_inner: inner_ty
                        .or_else(|| self.shared.known_generics.first().cloned()),
                    known_generics: self.shared.known_generics.clone(),
                    location: location.clone(),
                    dynamic,
                })
            };
        }

        while let Some(token) = self.advance_opt() {
            match token.kind {
                TokenKind::Dot => {
                    return self.parse_field_access(Some((position, expression!(), location)));
                }
                TokenKind::LeftBlockBrace => {
                    return self.parse_offset_store(Some((position, expression!(), location)));
                }
                TokenKind::Semicolon => {}
                other if other.is_ternary_start() => {
                    return self.parse_ternary_node(expression!(), location);
                }
                other if other.is_arithmetic() => {
                    self.position = position;
                    return self.parse_arithmetic();
                }
                _ if dynamic => {
                    inner_ty = Some(self.get_type(Some(self.shared.generics)));
                }
                _ => expect_eot!(token),
            }
        }

        expression!()
    }

    fn parse_if_statement(&mut self) -> AstNode {
        let location = self.current_token().location;
        self.advance();

        let tokens = self.yield_tokens_with_delimiters(&[TokenKind::LeftCurlyBrace]);
        let expression = Statement::new(tokens, 0, self.body, self.shared).parse().0;

        self.expect_tokens(&[TokenKind::LeftCurlyBrace]);
        self.advance();

        let body = self.yield_block(false);

        let mut elifs: Vec<(Box<AstNode>, Vec<AstNode>)> = vec![];
        let mut else_body: Vec<AstNode> = vec![];

        while self.current_token().kind == TokenKind::Else {
            self.advance();

            if self.current_token().kind == TokenKind::If {
                self.advance();

                let tokens = self.yield_tokens_with_delimiters(&[TokenKind::LeftCurlyBrace]);
                let elif_condition = Statement::new(tokens, 0, self.body, self.shared).parse().0;

                self.expect_tokens(&[TokenKind::LeftCurlyBrace]);
                self.advance();

                let elif_body = self.yield_block(false);
                elifs.push((Box::new(elif_condition), elif_body));
            } else {
                self.expect_tokens(&[TokenKind::LeftCurlyBrace]);
                self.advance();

                else_body = self.yield_block(false);
                break;
            }
        }

        self.position -= 1;
        set_end!(location, self);

        AstNode::IfStatement(IfStatement {
            condition: Box::new(expression),
            body,
            elifs,
            else_body,
            location,
        })
    }

    fn parse_while_statement(&mut self) -> AstNode {
        let location = self.current_token().location;
        self.advance();

        let tokens = self.yield_tokens_with_delimiters(&[TokenKind::LeftCurlyBrace]);
        let expression = Statement::new(tokens, 0, self.body, self.shared).parse().0;

        self.expect_tokens(&[TokenKind::LeftCurlyBrace]);
        self.advance();

        let body = self.yield_block(false); // While loops are statements

        self.position -= 1;
        set_end!(location, self);

        AstNode::WhileLoopStatement(WhileLoopStatement {
            condition: Box::new(expression),
            step: None,
            body,
            location,
        })
    }

    fn parse_for_statement(&mut self) -> AstNode {
        let location = self.current_token().location;
        self.advance();

        let mut wrapped = false;
        let position = self.position;

        if self.current_token().kind == TokenKind::LeftParenthesis {
            let mut i = self.position;

            wrapped = 'x: {
                while self.tokens[i].kind != TokenKind::LeftCurlyBrace && !self.is_eof() {
                    if self.tokens[i].kind == TokenKind::In {
                        break 'x false;
                    }

                    i += 1;
                }

                true
            };

            self.advance();
        }

        let declare_tokens = if self.current_token().kind == TokenKind::Semicolon {
            vec![]
        } else {
            self.yield_tokens_with_delimiters(&[TokenKind::Semicolon, TokenKind::In])
        };

        if self.current_token().kind == TokenKind::In {
            self.position = position;
            return self.parse_foreach_statement(location);
        }

        let declare = if declare_tokens.is_empty() {
            AstNode::Literal(Literal {
                kind: TokenKind::IntegerLiteral,
                value: ValueKind::Number(0),
                location: self.current_token().location,
                tagged: false,
            })
        } else {
            Statement::new(declare_tokens.clone(), 0, self.body, self.shared)
                .parse()
                .0
        };

        self.expect_tokens(&[TokenKind::Semicolon]);
        self.advance();

        let condition_tokens = if self.current_token().kind == TokenKind::Semicolon {
            vec![]
        } else {
            self.yield_tokens_with_delimiters(&[TokenKind::Semicolon])
        };

        let condition = if condition_tokens.is_empty() {
            AstNode::Literal(Literal {
                kind: TokenKind::IntegerLiteral,
                value: ValueKind::Number(1),
                location: self.current_token().location,
                tagged: false,
            })
        } else {
            Statement::new(condition_tokens, 0, self.body, self.shared)
                .parse()
                .0
        };

        self.expect_tokens(&[TokenKind::Semicolon]);
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
            self.expect_tokens(&[TokenKind::RightParenthesis]);
            self.advance();
        }

        self.expect_tokens(&[TokenKind::LeftCurlyBrace]);
        self.advance();

        let step = if step_tokens.is_empty() {
            AstNode::Literal(Literal {
                kind: TokenKind::IntegerLiteral,
                value: ValueKind::Number(1),
                location: self.current_token().location,
                tagged: false,
            })
        } else {
            Statement::new(step_tokens, 0, self.body, self.shared)
                .parse()
                .0
        };

        let body = self.yield_block(false); // For loops are statements
        let mut statements = vec![];

        self.position -= 1;

        if !declare_tokens.is_empty() {
            statements.push(declare);
        }

        set_end!(location, self);

        statements.push(AstNode::WhileLoopStatement(WhileLoopStatement {
            condition: Box::new(condition),
            step: Some(Box::new(step)),
            body,
            location: location.clone(),
        }));

        AstNode::BlockStatement(BlockStatement {
            body: statements,
            location,
        })
    }

    /// for x in [1, 2, 3] {}
    fn parse_foreach_statement(&mut self, location: MutRc<Location>) -> AstNode {
        self.expect_identifier();
        let first = self.current_token();
        self.advance();

        let second = if self.current_token().kind == TokenKind::Comma {
            self.advance();
            self.expect_identifier();
            let res = self.current_token();
            self.advance();
            Some(res)
        } else {
            None
        };

        let third = if self.current_token().kind == TokenKind::Comma {
            self.advance();
            self.expect_identifier();
            let res = self.current_token();
            self.advance();
            Some(res)
        } else {
            None
        };

        self.expect_tokens(&[TokenKind::In]);
        self.advance();

        let mut nesting = 0;
        let tokens = self.yield_tokens_with_condition(|token, _, _| {
            if token.kind == TokenKind::LeftCurlyBrace {
                if nesting == 0 {
                    return true;
                }

                nesting += 1;
            }

            if token.kind == TokenKind::RightCurlyBrace {
                nesting -= 1;
            }

            false
        });

        let iterator = Statement::new(tokens, 0, self.body, self.shared).parse().0;

        let mut iter_value = first.clone();
        iter_value.value = ValueKind::String(format!(
            INTERNAL_VALUE_FORMAT!(),
            first.value.get_string_inner().unwrap(),
            self.shared.tmp_counter.borrow()
        ));
        iter_value.tagged = false;
        *self.shared.tmp_counter.borrow_mut() += 1;

        let mut iter = first.clone();
        iter.value = ValueKind::String(format!(
            INTERNAL_ITERATOR_FORMAT!(),
            first.value.get_string_inner().unwrap(),
            self.shared.tmp_counter.borrow()
        ));
        iter.tagged = false;
        *self.shared.tmp_counter.borrow_mut() += 1;

        self.expect_tokens(&[TokenKind::LeftCurlyBrace]);
        set_end!(location, self);
        self.advance();

        let mut body = self.yield_block(false); // Foreach is a statement
        self.position -= 1;
        let mut statements = vec![];

        statements.push(AstNode::Declare(Declare {
            name: iter.clone(),
            r#type: Some(Type::Infer),
            value: Some(Box::new(AstNode::FunctionCall(FunctionCall {
                namespace_token: Token::from_ident(""),
                name_token: Token::from_ident(ITER_CONSTANT),
                name: ITER_CONSTANT.into(),
                generics: vec![],
                parameters: vec![(location.clone(), iterator)],
                type_method: true,
                ignore_no_def: false,
                location: location.clone(),
            }))),
            location: location.clone(),
            value_location: location.clone(),
        }));

        let next_node = AstNode::FunctionCall(FunctionCall {
            namespace_token: Token::from_ident(""),
            name_token: Token::from_ident("next"),
            name: "next".into(),
            generics: vec![],
            parameters: vec![(location.clone(), token_to_node!(&iter, self))],
            type_method: true,
            ignore_no_def: false,
            location: location.clone(),
        });

        let condition = AstNode::TupleDeclare(TupleDeclare {
            first: Token::from_ident(""),
            second: iter_value.clone(),
            third: None,
            ty: Some(Type::Infer),
            value: Box::new(next_node),
            location: location.clone(),
            value_location: location.clone(),
        });

        body.insert(
            0,
            if second.is_some() || third.is_some() {
                AstNode::TupleDeclare(TupleDeclare {
                    first,
                    second: second.unwrap(), // AT LEAST second must be there
                    third,
                    ty: Some(Type::Infer),
                    value: Box::new(token_to_node!(&iter_value, self)),
                    location: location.clone(),
                    value_location: location.clone(),
                })
            } else {
                AstNode::Declare(Declare {
                    name: first,
                    r#type: Some(Type::Infer),
                    value: Some(Box::new(token_to_node!(&iter_value, self))),
                    location: location.clone(),
                    value_location: location.clone(),
                })
            },
        );

        statements.push(AstNode::WhileLoopStatement(WhileLoopStatement {
            condition: Box::new(condition),
            step: None,
            body,
            location: location.clone(),
        }));

        AstNode::BlockStatement(BlockStatement {
            body: statements,
            location,
        })
    }

    fn parse_wrapped_statement(&mut self) -> AstNode {
        let location = self.current_token().location;
        let mut nesting = 0;
        let mut index = 0;
        let position = self.position;

        while index < self.tokens.len() {
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

        let mut expression = Statement::new(tokens, 0, self.body, self.shared).parse().0;

        self.expect_tokens(&[TokenKind::RightParenthesis]);
        set_end!(location, self);

        if let Some(token) = self.advance_opt() {
            match token.kind {
                TokenKind::Dot => {
                    expression = self.parse_field_access(Some((position, expression, location)));
                }
                TokenKind::LeftBlockBrace => {
                    expression = self.parse_offset_store(Some((position, expression, location)));
                }
                TokenKind::Semicolon => {}
                other if other.is_ternary_start() => {
                    expression = self.parse_ternary_node(expression, location);
                }

                other if other.is_arithmetic() => {
                    self.position = position;
                    expression = self.parse_arithmetic()
                }

                _ => expect_eot!(token),
            }
        }

        expression
    }

    fn parse_offset_store(&mut self, lhs: Option<(usize, AstNode, MutRc<Location>)>) -> AstNode {
        let position = if let Some(lhs) = lhs.clone() {
            lhs.0
        } else {
            self.position
        };

        let location = if let Some(lhs) = lhs.clone() {
            lhs.2
        } else {
            self.current_token().location
        };

        let left_location = if let Some(lhs) = lhs.clone() {
            self.tokens[lhs.0].location.clone()
        } else {
            self.current_token().location
        };

        let value;

        let left_tokens = if lhs.is_some() {
            vec![]
        } else {
            self.yield_tokens_with_delimiters(&[TokenKind::LeftBlockBrace])
        };

        let left = Box::new(if let Some(lhs) = lhs {
            lhs.1
        } else {
            Statement::new(left_tokens, 0, self.body, self.shared)
                .parse()
                .0
        });

        self.expect_tokens(&[TokenKind::LeftBlockBrace]);
        set_end!(left_location, self);
        self.advance();

        let right_location = self.current_token().location;
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

            false
        });

        let right = Box::new(
            Statement::new(right_tokens, 0, self.body, self.shared)
                .parse()
                .0,
        );

        self.expect_tokens(&[TokenKind::RightBlockBrace]);
        set_end!(right_location, self);
        set_end!(location, self);

        let value_location = self.current_token().location;

        let mut expression = AstNode::MemoryOperation(MemoryOperation {
            left: left.clone(),
            right: right.clone(),
            value: None,
            left_location: left_location.clone(),
            right_location: right_location.clone(),
            value_location: value_location.clone(),
            is_deref: false,
            addr_only: self.shared.addr_only,
        });

        self.consumed_addr = true;

        if let Some(token) = self.advance_opt() {
            match token.kind {
                TokenKind::Equal => {
                    self.advance();
                    let value_tokens = self.yield_tokens_wrapped_with_semi();

                    value = Some(Box::new(
                        Statement::new(value_tokens, 0, self.body, self.shared)
                            .parse()
                            .0,
                    ));

                    set_end!(value_location, self);
                    set_end!(location, self);

                    expression = AstNode::MemoryOperation(MemoryOperation {
                        left,
                        right,
                        value,
                        left_location,
                        right_location,
                        value_location,
                        is_deref: false,
                        addr_only: self.shared.addr_only,
                    });

                    self.consumed_addr = true;
                }
                other if other.is_declarative() => {
                    value = Some(Box::new(self.parse_declarative_node(expression.clone())));
                    set_end!(value_location, self);
                    set_end!(location, self);

                    expression = AstNode::MemoryOperation(MemoryOperation {
                        left,
                        right,
                        value,
                        left_location,
                        right_location,
                        value_location,
                        is_deref: false,
                        addr_only: self.shared.addr_only,
                    });

                    self.consumed_addr = true;
                }
                TokenKind::Dot => {
                    expression = self.parse_field_access(Some((
                        position,
                        AstNode::MemoryOperation(MemoryOperation {
                            left,
                            right,
                            value: None,
                            left_location,
                            right_location,
                            value_location,
                            is_deref: false,
                            addr_only: false,
                        }),
                        location,
                    )));
                }
                TokenKind::LeftBlockBrace => {
                    expression = self.parse_offset_store(Some((
                        position,
                        AstNode::MemoryOperation(MemoryOperation {
                            left,
                            right,
                            value: None,
                            left_location,
                            right_location,
                            value_location,
                            is_deref: false,
                            addr_only: false,
                        }),
                        location,
                    )));
                }
                TokenKind::Semicolon => {}
                other if other.is_ternary_start() => {
                    return self.parse_ternary_node(expression, location)
                }
                other if other.is_arithmetic() => {
                    self.position = position;
                    return self.parse_arithmetic();
                }
                _ => expect_eot!(token),
            }
        }

        expression
    }

    fn parse_variadic(&mut self) -> AstNode {
        let location = self.current_token().location;
        self.advance();
        self.expect_identifier();
        let name = self.current_token();

        self.advance();
        self.expect_tokens(&[TokenKind::Semicolon]);
        set_end!(location, self);

        AstNode::VariadicStart(VariadicStart { name, location })
    }

    fn parse_yield_variadic(&mut self) -> AstNode {
        let location = self.current_token().location;
        let position = self.position;
        self.expect_identifier();
        let name = self.current_token();

        self.advance();
        self.expect_tokens(&[TokenKind::Dot]);
        self.advance();
        self.expect_tokens(&[TokenKind::Yield]);
        self.advance();
        self.expect_tokens(&[TokenKind::LeftParenthesis]);
        self.advance();

        let r#type = self.get_type(Some(self.shared.generics));
        self.advance();

        self.expect_tokens(&[TokenKind::RightParenthesis]);
        set_end!(location, self);

        let mut expression = AstNode::VariadicArgument(VariadicArgument {
            name,
            r#type: Some(r#type),
            location: location.clone(),
        });

        if let Some(token) = self.advance_opt() {
            match token.kind {
                TokenKind::Dot => {
                    expression = self.parse_field_access(Some((position, expression, location)));
                }
                TokenKind::LeftBlockBrace => {
                    expression = self.parse_offset_store(Some((position, expression, location)));
                }
                TokenKind::Semicolon => {}
                other if other.is_ternary_start() => {
                    expression = self.parse_ternary_node(expression, location);
                }

                other if other.is_arithmetic() => {
                    self.position = position;
                    return self.parse_arithmetic();
                }
                _ => expect_eot!(token),
            }
        }

        expression
    }

    fn parse_defer(&mut self) -> AstNode {
        let location = self.current_token().location;
        self.advance();

        let mut tokens = vec![];
        let mut paren_nesting = 0;
        let mut curly_nesting = 0;
        let mut block_nesting = 0;

        while !self.is_eof() {
            if self.current_token().kind == TokenKind::LeftParenthesis {
                paren_nesting += 1;
            }

            if self.current_token().kind == TokenKind::LeftCurlyBrace {
                curly_nesting += 1;
            }

            if self.current_token().kind == TokenKind::LeftBlockBrace {
                block_nesting += 1;
            }

            tokens.push(self.current_token());
            self.advance();

            if self.current_token().kind == TokenKind::Semicolon
                && paren_nesting == 0
                && curly_nesting == 0
                && block_nesting == 0
            {
                break;
            }

            if self.current_token().kind == TokenKind::RightParenthesis && paren_nesting > 0 {
                paren_nesting -= 1;
            }

            if self.current_token().kind == TokenKind::RightCurlyBrace && curly_nesting > 0 {
                curly_nesting -= 1;
            }

            if self.current_token().kind == TokenKind::RightBlockBrace && block_nesting > 0 {
                block_nesting -= 1;
            }
        }

        let value = Box::new(Statement::new(tokens, 0, self.body, self.shared).parse().0);
        set_end!(location, self);

        AstNode::DeferStatement { value, location }
    }

    fn parse_lambda(&mut self) -> AstNode {
        let location = self.current_token().location;

        self.expect_tokens(&[TokenKind::Function]);
        self.advance();

        self.expect_tokens(&[TokenKind::LeftParenthesis]);
        self.advance();

        let mut arguments = vec![];
        let mut return_ty = None;

        while self.current_token().kind != TokenKind::RightParenthesis && !self.is_eof() {
            if self.current_token().kind == TokenKind::Ellipsis {
                elle_error!(self
                    .current_token()
                    .location
                    .borrow()
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
                }
            }

            if self.current_token().kind == TokenKind::Identifier
                && let Some(next) = self.next_token()
                && [TokenKind::Comma, TokenKind::RightParenthesis].contains(&next.kind)
            {
                let name = self.current_token();

                self.advance();
                if self.current_token().kind == TokenKind::Comma {
                    self.advance();
                }

                arguments.push(Ok(name));
                continue;
            }

            let ty = self.get_type(Some(self.shared.generics));
            self.advance();

            let name = self.get_identifier();
            self.advance();

            if self.current_token().kind == TokenKind::Comma {
                self.advance();
            }

            arguments.push(Err(Argument {
                r#type: ty,
                name,
                no_fmt,
                is_unused: false,
            }));
        }

        self.expect_tokens(&[TokenKind::RightParenthesis]);
        self.advance();

        if self.current_token().kind == TokenKind::RightArrow {
            self.advance();
            return_ty = Some(self.get_type(Some(self.shared.generics)));
            self.advance();
        }

        if self.current_token().kind == TokenKind::LeftCurlyBrace {
            self.expect_tokens(&[TokenKind::LeftCurlyBrace]);
            self.advance();

            let body = self.yield_block(true); // Lambdas are expressions
            self.position -= 1;

            set_end!(location, self);

            AstNode::Lambda(Lambda {
                arguments,
                return_ty,
                value: body,
                location,
            })
        } else {
            let mut nesting = 0;
            let mut block_nesting = 0;
            let mut curly_nesting = 0;

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

                if token.kind == TokenKind::LeftBlockBrace {
                    block_nesting += 1;
                }

                if token.kind == TokenKind::RightBlockBrace {
                    if block_nesting > 0 {
                        block_nesting -= 1;
                    } else {
                        return true;
                    }
                }

                if token.kind == TokenKind::LeftCurlyBrace {
                    curly_nesting += 1;
                }

                if token.kind == TokenKind::RightCurlyBrace {
                    if curly_nesting > 0 {
                        curly_nesting -= 1;
                    } else {
                        return true;
                    }
                }

                (token.kind == TokenKind::Semicolon && block_nesting == 0 && curly_nesting == 0)
                    || (nesting == 0
                        && block_nesting == 0
                        && curly_nesting == 0
                        && (token.kind == TokenKind::Comma
                            || next_token
                                .is_some_and(|next| next.kind == TokenKind::RightCurlyBrace)))
            });

            let value = Statement::new(tokens, 0, self.body, self.shared).parse().0;
            set_end!(location, self);

            AstNode::Lambda(Lambda {
                arguments,
                return_ty,
                value: vec![AstNode::Return(Return {
                    value: Box::new(value),
                    location: location.clone(),
                })],
                location,
            })
        }
    }

    fn parse_type_conversion(&mut self) -> AstNode {
        let location = self.current_token().location;
        let position = self.position;

        if self.current_token().tagged {
            elle_error!(format!(
                "hover\n{}\n{}\n#cast(T, cast_expr) -> T\n",
                self.current_token().location.borrow().display_plain(false),
                self.current_token().location.borrow().display_plain(true),
            ));
        }

        self.advance();

        self.expect_tokens(&[TokenKind::LeftParenthesis]);
        self.advance();

        let r#type = self.get_type(Some(self.shared.generics));
        self.advance();

        self.expect_tokens(&[TokenKind::Comma]);
        self.advance();

        let mut tokens = vec![];
        let mut nesting = 0;

        loop {
            if self.current_token().kind == TokenKind::LeftParenthesis {
                nesting += 1;
            }

            tokens.push(self.current_token());
            let res = self.advance_opt();

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

        self.expect_tokens(&[TokenKind::RightParenthesis]);
        set_end!(location, self);

        let stmt = Statement::new(tokens, 0, self.body, self.shared).parse().0;
        let mut expression = AstNode::Conversion(Conversion {
            r#type: Some(r#type),
            value: Box::new(stmt),
            location: location.clone(),
            explicit: true,
        });

        if let Some(token) = self.advance_opt() {
            match token.kind {
                TokenKind::Dot => {
                    expression = self.parse_field_access(Some((position, expression, location)));
                }
                TokenKind::LeftBlockBrace => {
                    expression = self.parse_offset_store(Some((position, expression, location)));
                }
                TokenKind::Semicolon => {}
                other if other.is_ternary_start() => {
                    expression = self.parse_ternary_node(expression, location);
                }

                other if other.is_arithmetic() => {
                    self.position = position;
                    return self.parse_arithmetic();
                }
                _ => expect_eot!(token),
            }
        }

        expression
    }

    fn parse_block(&mut self) -> AstNode {
        let location = self.current_token().location;
        self.expect_tokens(&[TokenKind::LeftCurlyBrace]);
        self.advance();

        let body = self.yield_block(false); // Blocks are statements
        self.position -= 1;
        set_end!(location, self);

        AstNode::BlockStatement(BlockStatement { body, location })
    }

    fn parse_size(&mut self) -> AstNode {
        let location = self.current_token().location;
        let position = self.position;
        self.expect_tokens(&[TokenKind::Size]);

        if self.current_token().tagged {
            elle_error!(format!(
                "hover\n{}\n{}\n#size(T | expr) -> u64\n",
                self.current_token().location.borrow().display_plain(false),
                self.current_token().location.borrow().display_plain(true),
            ));
        }

        self.advance();

        self.expect_tokens(&[TokenKind::LeftParenthesis]);
        self.advance();

        let value = if self.is_type_contextually(0) {
            let ty = self.get_type(Some(self.shared.generics));
            self.advance();
            Ok(ty)
        } else {
            let mut tokens = vec![];
            let mut nesting = 0;

            if self.current_token().kind == TokenKind::Semicolon {
                elle_error!(self
                    .current_token()
                    .location
                    .borrow()
                    .error("Expected size directive but got empty passthrough"))
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

            let value = Box::new(Statement::new(tokens, 0, self.body, self.shared).parse().0);
            Err(value)
        };

        self.expect_tokens(&[TokenKind::RightParenthesis]);
        set_end!(location, self);

        let mut expression = AstNode::Size(Size {
            value,
            location: location.clone(),
        });

        if let Some(token) = self.advance_opt() {
            match token.kind {
                TokenKind::Dot => {
                    expression = self.parse_field_access(Some((position, expression, location)));
                }
                TokenKind::LeftBlockBrace => {
                    expression = self.parse_offset_store(Some((position, expression, location)));
                }
                TokenKind::Semicolon => {}
                other if other.is_ternary_start() => {
                    expression = self.parse_ternary_node(expression, location);
                }
                other if other.is_arithmetic() => {
                    self.position = position;
                    expression = self.parse_arithmetic();
                }
                _ => expect_eot!(token),
            }
        }

        expression
    }

    fn parse_array_length(&mut self) -> AstNode {
        let location = self.current_token().location;
        let position = self.position;
        self.expect_tokens(&[TokenKind::ArrayLength]);

        if self.current_token().tagged {
            elle_error!(format!(
                "hover\n{}\n{}\n#len(static_array_expr) -> i64\n",
                self.current_token().location.borrow().display_plain(false),
                self.current_token().location.borrow().display_plain(true),
            ));
        }

        self.advance();

        self.expect_tokens(&[TokenKind::LeftParenthesis]);
        self.advance();

        let mut tokens = vec![];
        let mut nesting = 0;

        if self.current_token().kind == TokenKind::Semicolon {
            elle_error!(self
                .current_token()
                .location
                .borrow()
                .error("Expected array length directive but got empty passthrough"))
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

        let value = Box::new(Statement::new(tokens, 0, self.body, self.shared).parse().0);

        self.expect_tokens(&[TokenKind::RightParenthesis]);
        set_end!(location, self);

        let mut expression = AstNode::ArrayLength(ArrayLength {
            value,
            location: location.clone(),
        });

        if let Some(token) = self.advance_opt() {
            match token.kind {
                TokenKind::Dot => {
                    expression = self.parse_field_access(Some((position, expression, location)));
                }
                TokenKind::LeftBlockBrace => {
                    expression = self.parse_offset_store(Some((position, expression, location)));
                }
                TokenKind::Semicolon => {}
                other if other.is_ternary_start() => {
                    expression = self.parse_ternary_node(expression, location);
                }
                other if other.is_arithmetic() => {
                    self.position = position;
                    expression = self.parse_arithmetic();
                }
                _ => expect_eot!(token),
            }
        }
        expression
    }

    fn parse_unary(&mut self) -> AstNode {
        let token = self.current_token();
        let location = token.location.clone();
        self.advance();

        let tokens = self.yield_tokens_for_unary();
        let parsed = Box::new(Statement::new(tokens, 0, self.body, self.shared).parse().0);
        set_end!(location, self);

        let node = AstNode::BinaryOperation(BinaryOperation {
            left: parsed,
            right: Box::new(AstNode::token_to_literal(token)),
            operator: TokenKind::Multiply,
            treat_as_string: false,
            dunder_methods: true,
            location: location.clone(),
        });

        if self.current_token().kind.is_ternary_start() {
            self.parse_ternary_node(node, location)
        } else {
            node
        }
    }

    fn parse_not(&mut self) -> AstNode {
        let location = self.current_token().location;
        self.advance();

        let tokens = self.yield_tokens_for_unary();
        let value = Box::new(Statement::new(tokens, 0, self.body, self.shared).parse().0);
        set_end!(location, self);

        let node = AstNode::LogicalNot(LogicalNot {
            value,
            location: location.clone(),
        });

        if self.current_token().kind.is_ternary_start() {
            self.parse_ternary_node(node, location)
        } else {
            node
        }
    }

    fn parse_bitwise_not(&mut self) -> AstNode {
        let location = self.current_token().location;
        self.advance();

        let tokens = self.yield_tokens_for_unary();
        let value = Box::new(Statement::new(tokens, 0, self.body, self.shared).parse().0);
        set_end!(location, self);

        let node = AstNode::BitwiseNot(BitwiseNot {
            value,
            location: location.clone(),
        });

        if self.current_token().kind.is_ternary_start() {
            self.parse_ternary_node(node, location)
        } else {
            node
        }
    }

    fn parse_address(&mut self) -> AstNode {
        let location = self.current_token().location;
        self.advance();

        let tokens = self.yield_tokens_for_unary();
        let (value_node, _, _, ignore) = Statement::new(
            tokens,
            0,
            self.body,
            &Shared {
                addr_only: true,
                ..*self.shared
            },
        )
        .parse();

        let value = Box::new(value_node);
        set_end!(location, self);

        let node = if ignore {
            *value
        } else {
            AstNode::Address(Address {
                value,
                location: location.clone(),
            })
        };

        if self.current_token().kind.is_ternary_start() {
            self.parse_ternary_node(node, location)
        } else {
            node
        }
    }

    fn parse_deref(&mut self) -> AstNode {
        let left_location = self.current_token().location;
        let position = self.position;
        self.advance();
        let mut value = None;

        let tokens = self.yield_tokens_for_unary();
        let left = Box::new(Statement::new(tokens, 0, self.body, self.shared).parse().0);

        let right_location = self.current_token().location;
        let right = Box::new(AstNode::Literal(Literal {
            kind: TokenKind::LongLiteral,
            value: ValueKind::Number(0),
            location: self.current_token().location,
            tagged: false,
        }));

        set_end!(left_location, self);
        let value_location = self.current_token().location;

        if !self.is_eof() {
            match self.current_token().kind {
                TokenKind::Equal => {
                    self.advance();
                    set_end!(value_location, self);
                    set_end!(left_location, self);
                    let value_tokens = self.yield_tokens_wrapped_with_semi();

                    value = Some(Box::new(
                        Statement::new(value_tokens, 0, self.body, self.shared)
                            .parse()
                            .0,
                    ));
                }

                TokenKind::Semicolon => {}
                other if other.is_declarative() => {
                    value = Some(Box::new(self.parse_declarative_node(
                        AstNode::MemoryOperation(MemoryOperation {
                            left: left.clone(),
                            right: right.clone(),
                            value,
                            left_location: left_location.clone(),
                            right_location: right_location.clone(),
                            value_location: value_location.clone(),
                            is_deref: true,
                            addr_only: self.shared.addr_only,
                        }),
                    )));

                    self.consumed_addr = true;
                }

                other if other.is_arithmetic() => {
                    self.position = position;
                    return self.parse_arithmetic();
                }
                _ => expect_eot!(self.current_token()),
            }
        }

        self.consumed_addr = true;
        AstNode::MemoryOperation(MemoryOperation {
            left,
            right,
            value,
            left_location,
            right_location,
            value_location,
            is_deref: true,
            addr_only: self.shared.addr_only,
        })
    }

    fn parse_struct_init(&mut self) -> AstNode {
        let location = self.current_token().location;
        let position = self.position;
        self.expect_identifier();
        let name = self.current_token();
        let plain_name = name.value.get_string_inner().unwrap();

        if !(self.shared.struct_pool.borrow().contains_key(&plain_name)) {
            elle_error!(self.current_token().location.borrow().error(format!(
                "Struct named '{plain_name}' could not be found. Are you sure you typed it correctly?"
            )))
        }

        self.advance();
        self.expect_tokens(&[TokenKind::LeftCurlyBrace]);
        self.advance();

        let mut values = vec![];

        while !self.is_eof() {
            if self.current_token().kind == TokenKind::RightCurlyBrace {
                self.advance();
                break;
            }

            if self.current_token().kind == TokenKind::Comma {
                self.advance();
                continue;
            }

            let name_token = self.current_token();
            let name = self.get_identifier();

            self.advance();

            if [TokenKind::Comma, TokenKind::RightCurlyBrace].contains(&self.current_token().kind) {
                values.push((name, Box::new(token_to_node!(&name_token, self))));
                self.advance();
                continue;
            }

            self.expect_tokens(&[TokenKind::Equal]);
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
                    } else {
                        self.advance();
                        break;
                    }
                }

                if self.current_token().kind == TokenKind::RightParenthesis && paren_nesting > 0 {
                    paren_nesting -= 1;
                }

                if self.current_token().kind == TokenKind::RightBlockBrace && block_nesting > 0 {
                    block_nesting -= 1;
                }

                if self.current_token().kind == TokenKind::RightCurlyBrace && curly_nesting > 0 {
                    curly_nesting -= 1;
                }

                if self.is_eof() {
                    break;
                }
            }

            let value = Box::new(Statement::new(tokens, 0, self.body, self.shared).parse().0);
            values.push((name, value));
        }

        set_end!(location, self);

        let mut expression = AstNode::StructLiteral(StructLiteral {
            name,
            values,
            location: location.clone(),
        });

        if let Some(token) = self.advance_opt() {
            match token.kind {
                TokenKind::Dot => {
                    expression = self.parse_field_access(Some((position, expression, location)));
                }
                TokenKind::LeftBlockBrace => {
                    expression = self.parse_offset_store(Some((position, expression, location)));
                }
                TokenKind::Semicolon => {}
                other if other.is_ternary_start() => {
                    expression = self.parse_ternary_node(expression, location);
                }

                other if other.is_arithmetic() => {
                    self.position = position;
                    expression = self.parse_arithmetic();
                }
                _ => expect_eot!(token),
            }
        }

        expression
    }

    fn parse_field_access(&mut self, lhs: Option<(usize, AstNode, MutRc<Location>)>) -> AstNode {
        let location = if lhs.is_some() {
            lhs.clone().unwrap().2
        } else {
            self.current_token().location
        };

        let valid_tokens = &[TokenKind::Dot];
        let mut value = None;

        let position = if let Some(lhs) = lhs.clone() {
            lhs.0
        } else {
            self.position
        };

        // Parse the initial left-hand side of the field access
        let left = if let Some(lhs) = lhs {
            Box::new(lhs.1)
        } else {
            let left_tokens = self.yield_tokens_with_delimiters(valid_tokens);

            Box::new(
                Statement::new(left_tokens, 0, self.body, self.shared)
                    .parse()
                    .0,
            )
        };

        self.expect_tokens(valid_tokens);
        self.advance();

        self.expect_identifier();

        let name_token = self.current_token();
        let name = self.get_identifier();
        let mut right = Box::new(AstNode::token_to_literal(self.current_token()));

        self.advance();

        let mut tmp = vec![];

        if self.current_token().kind == TokenKind::LessThan && self.is_type_contextually(1) {
            self.advance();

            while self.current_token().kind != TokenKind::GreaterThan && !self.is_eof() {
                tmp.push(self.get_type(Some(self.shared.generics)));
                self.advance();

                if self.current_token().kind == TokenKind::Comma {
                    self.advance();
                }
            }

            self.expect_tokens(&[TokenKind::GreaterThan]);
            self.advance();
        } else {
            tmp.clone_from(self.shared.known_generics);
        }

        if self.current_token().kind == TokenKind::LeftParenthesis {
            set_end!(location, self);

            return self.parse_function(
                Some((location.clone(), Token::from_ident(""), name_token, name)),
                Some(vec![(location, *left)]),
                if tmp.is_empty() { None } else { Some(tmp) },
                Some(position),
                true,
            );
        }

        // Parse the rest of the field accesses
        while valid_tokens.contains(&self.current_token().kind) && !self.is_eof() {
            self.advance(); // Ignore the TokenKind::Dot

            self.expect_identifier();
            let inner_location = self.current_token().location;

            let name_token = self.current_token();
            let name = self.get_identifier();
            let inner = Box::new(AstNode::token_to_literal(self.current_token()));

            self.advance();

            if self.current_token().kind == TokenKind::LessThan && self.is_type_contextually(1) {
                self.advance();

                while self.current_token().kind != TokenKind::GreaterThan && !self.is_eof() {
                    tmp.push(self.get_type(Some(self.shared.generics)));
                    self.advance();

                    if self.current_token().kind == TokenKind::Comma {
                        self.advance();
                    }
                }

                self.expect_tokens(&[TokenKind::GreaterThan]);
                self.advance();
            }

            if self.current_token().kind == TokenKind::LeftParenthesis {
                set_end!(inner_location, self);
                set_end!(location, self);

                self.consumed_addr = true;

                return self.parse_function(
                    Some((inner_location, Token::from_ident(""), name_token, name)),
                    Some(vec![(
                        location.clone(),
                        AstNode::FieldAccess(FieldAccess {
                            left,
                            right,
                            value,
                            location,
                            addr_only: self.shared.addr_only,
                        }),
                    )]),
                    if tmp.is_empty() { None } else { Some(tmp) },
                    Some(position),
                    true,
                );
            }

            set_end!(location, self);

            if let AstNode::FieldAccess(FieldAccess {
                left,
                right: inner_right,
                location,
                ..
            }) = *right
            {
                right = Box::new(AstNode::FieldAccess(FieldAccess {
                    left,
                    right: Box::new(AstNode::FieldAccess(FieldAccess {
                        left: inner_right,
                        right: inner,
                        value: None,
                        addr_only: false,
                        location: location.clone(),
                    })),
                    value: None,
                    addr_only: false,
                    location,
                }));
            } else {
                right = Box::new(AstNode::FieldAccess(FieldAccess {
                    left: right,
                    right: inner,
                    value: None, // Only the root may have a value
                    location: location.clone(),
                    addr_only: false,
                }));
            }
        }

        set_end!(location, self);

        let mut expression = AstNode::FieldAccess(FieldAccess {
            left: left.clone(),
            right: right.clone(),
            value: value.clone(),
            location: location.clone(),
            addr_only: self.shared.addr_only,
        });

        self.consumed_addr = true;

        if !self.is_eof() {
            match self.current_token().kind {
                TokenKind::Equal => {
                    self.advance();
                    let value_tokens = self.yield_tokens_wrapped_with_semi();

                    value = Some(Box::new(
                        Statement::new(value_tokens, 0, self.body, self.shared)
                            .parse()
                            .0,
                    ));

                    expression = AstNode::FieldAccess(FieldAccess {
                        left,
                        right,
                        value,
                        location,
                        addr_only: self.shared.addr_only,
                    });

                    self.consumed_addr = true;
                }
                // foo.a.meow() = meow(foo.a)
                TokenKind::LeftParenthesis => {
                    expression = self.parse_function(
                        Some((location.clone(), Token::from_ident(""), name_token, name)),
                        Some(vec![(location, *left)]),
                        if tmp.is_empty() { None } else { Some(tmp) },
                        Some(position),
                        true,
                    );
                }
                TokenKind::LeftBlockBrace => {
                    expression = self.parse_offset_store(Some((
                        position,
                        AstNode::FieldAccess(FieldAccess {
                            left,
                            right,
                            value,
                            location: location.clone(),
                            addr_only: false, // dont take the address because if required it will now be taken here instead
                        }),
                        location,
                    )));
                }
                other if other.is_declarative() => {
                    value = Some(Box::new(self.parse_declarative_node(expression)));

                    expression = AstNode::FieldAccess(FieldAccess {
                        left,
                        right,
                        value,
                        location,
                        addr_only: self.shared.addr_only,
                    });

                    self.consumed_addr = true;
                }
                other if other.is_ternary_start() => {
                    expression = self.parse_ternary_node(expression, location);
                }
                other if other.is_arithmetic() => {
                    self.position = position;
                    expression = self.parse_arithmetic();
                }
                _ => expect_eot!(self.current_token()),
            }
        }

        expression
    }

    fn parse_ternary_node(&mut self, condition: AstNode, location: MutRc<Location>) -> AstNode {
        self.expect_tokens(&[TokenKind::Question]);
        self.advance();

        let if_true = Box::new(if self.current_token().kind == TokenKind::Colon {
            self.advance();
            condition.clone()
        } else {
            let mut paren_nesting = 0;
            let mut block_nesting = 0;
            let mut curly_nesting = 0;
            let mut nesting = 0;

            let tokens = self.yield_tokens_with_condition(|current, prev, _| {
                if prev.kind == TokenKind::LeftParenthesis {
                    paren_nesting += 1;
                }

                if prev.kind == TokenKind::RightParenthesis {
                    paren_nesting -= 1;
                }

                if prev.kind == TokenKind::LeftBlockBrace {
                    block_nesting += 1;
                }

                if prev.kind == TokenKind::RightBlockBrace {
                    block_nesting -= 1;
                }

                if prev.kind == TokenKind::LeftCurlyBrace {
                    curly_nesting += 1;
                }

                if prev.kind == TokenKind::RightCurlyBrace {
                    curly_nesting -= 1;
                }

                if current.kind.is_ternary_start()
                    && curly_nesting == 0
                    && block_nesting == 0
                    && paren_nesting == 0
                {
                    nesting += 1;
                }

                if current.kind.is_ternary_end()
                    && curly_nesting == 0
                    && block_nesting == 0
                    && paren_nesting == 0
                {
                    if nesting > 0 {
                        nesting -= 1;
                    } else {
                        return true;
                    }
                }

                false
            });

            self.advance();
            Statement::new(tokens, 0, self.body, self.shared).parse().0
        });

        let if_false = Box::new({
            let tokens = self.yield_tokens_wrapped_with_semi();
            Statement::new(tokens, 0, self.body, self.shared).parse().0
        });

        set_end!(location, self);

        AstNode::Ternary(Ternary {
            condition: Box::new(condition),
            if_true,
            if_false,
            location,
        })
    }

    fn parse_env(&mut self) -> AstNode {
        let location = self.current_token().location;
        let position = self.position;

        if self.current_token().tagged {
            elle_error!(format!(
                "hover\n{}\n{}\n#env: ElleEnv*\n",
                self.current_token().location.borrow().display_plain(false),
                self.current_token().location.borrow().display_plain(true),
            ));
        }

        set_end!(location, self);

        let mut expression = AstNode::Environment(Environment {
            value: None,
            location: location.clone(),
        });

        if let Some(token) = self.advance_opt() {
            match token.kind {
                TokenKind::Equal => {
                    self.advance();
                    let value_tokens = self.yield_tokens_wrapped_with_semi();

                    expression = AstNode::Environment(Environment {
                        value: Some(Box::new(
                            Statement::new(value_tokens, 0, self.body, self.shared)
                                .parse()
                                .0,
                        )),
                        location,
                    });
                }

                TokenKind::Dot => {
                    expression = self.parse_field_access(Some((position, expression, location)));
                }

                TokenKind::LeftBlockBrace => {
                    expression = self.parse_offset_store(Some((position, expression, location)));
                }

                TokenKind::Semicolon => {}

                other if other.is_ternary_start() => {
                    expression = self.parse_ternary_node(expression, location);
                }

                other if other.is_declarative() => {
                    expression = AstNode::Environment(Environment {
                        value: Some(Box::new(self.parse_declarative_node(expression))),
                        location,
                    });
                }

                other if other.is_arithmetic() => {
                    self.position = position;
                    return self.parse_arithmetic();
                }
                _ => expect_eot!(token),
            }
        }

        expression
    }

    fn parse_alloc(&mut self) -> AstNode {
        let location = self.current_token().location;
        let position = self.position;

        if self.current_token().tagged {
            elle_error!(format!(
                "hover\n{}\n{}\n#alloc(T, count_expr?) -> T*\n",
                self.current_token().location.borrow().display_plain(false),
                self.current_token().location.borrow().display_plain(true),
            ));
        }

        self.advance();

        self.expect_tokens(&[TokenKind::LeftParenthesis]);
        self.advance();

        let ty = self.get_type(Some(self.shared.generics));
        self.advance();

        let count = if self.current_token().kind == TokenKind::Comma {
            self.advance();
            let mut nesting = i32::from(self.current_token().kind == TokenKind::LeftParenthesis);

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

                false
            });

            self.advance();
            Statement::new(tokens, 0, self.body, self.shared).parse().0
        } else {
            AstNode::Literal(Literal {
                kind: TokenKind::IntegerLiteral,
                value: ValueKind::Number(1),
                location: location.clone(),
                tagged: false,
            })
        };

        set_end!(location, self);

        let mut expression = AstNode::Conversion(Conversion {
            r#type: Some(Type::Pointer(Box::new(ty.clone()))),
            value: Box::new(AstNode::FunctionCall(FunctionCall {
                namespace_token: Token::from_ident(""),
                name_token: Token::from_ident("alloc"),
                name: "alloc".into(),
                generics: vec![],
                parameters: vec![
                    (
                        location.clone(),
                        AstNode::FieldAccess(FieldAccess {
                            left: Box::new(AstNode::Environment(Environment {
                                value: None,
                                location: location.clone(),
                            })),
                            right: Box::new(AstNode::Literal(Literal {
                                kind: TokenKind::Identifier,
                                value: ValueKind::String("allocator".into()),
                                location: location.clone(),
                                tagged: false,
                            })),
                            value: None,
                            addr_only: false,
                            location: location.clone(),
                        }),
                    ),
                    (
                        location.clone(),
                        AstNode::BinaryOperation(BinaryOperation {
                            left: Box::new(AstNode::Size(Size {
                                value: Ok(ty),
                                location: location.clone(),
                            })),
                            right: Box::new(count),
                            operator: TokenKind::Multiply,
                            treat_as_string: false,
                            dunder_methods: true,
                            location: location.clone(),
                        }),
                    ),
                ],
                type_method: true,
                ignore_no_def: false,
                location: location.clone(),
            })),
            location: location.clone(),
            explicit: true,
        });

        if !self.is_eof() {
            match self.current_token().kind {
                TokenKind::Dot => {
                    expression = self.parse_field_access(Some((position, expression, location)));
                }

                TokenKind::LeftBlockBrace => {
                    expression = self.parse_offset_store(Some((position, expression, location)));
                }

                TokenKind::Semicolon => {}
                other if other.is_ternary_start() => {
                    expression = self.parse_ternary_node(expression, location);
                }

                other if other.is_arithmetic() => {
                    self.position = position;
                    return self.parse_arithmetic();
                }
                _ => expect_eot!(self.current_token()),
            }
        }

        expression
    }

    fn parse_realloc(&mut self) -> AstNode {
        let location = self.current_token().location;
        let position = self.position;

        if self.current_token().tagged {
            elle_error!(format!(
                "hover\n{}\n{}\n#realloc(ptr_expr, T, count_expr?) -> T*\n",
                self.current_token().location.borrow().display_plain(false),
                self.current_token().location.borrow().display_plain(true),
            ));
        }

        self.advance();
        self.expect_tokens(&[TokenKind::LeftParenthesis]);
        self.advance();

        let tokens = self.yield_tokens_with_delimiters(&[TokenKind::Comma]);
        let ptr = Statement::new(tokens, 0, self.body, self.shared).parse().0;
        self.advance();

        let ty = self.get_type(Some(self.shared.generics));
        self.advance();

        let count = if self.current_token().kind == TokenKind::Comma {
            self.advance();
            let mut nesting = i32::from(self.current_token().kind == TokenKind::LeftParenthesis);

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

                false
            });

            self.advance();
            Statement::new(tokens, 0, self.body, self.shared).parse().0
        } else {
            AstNode::Literal(Literal {
                kind: TokenKind::IntegerLiteral,
                value: ValueKind::Number(1),
                location: location.clone(),
                tagged: false,
            })
        };

        set_end!(location, self);

        let mut expression = AstNode::Conversion(Conversion {
            r#type: Some(Type::Pointer(Box::new(ty.clone()))),
            value: Box::new(AstNode::FunctionCall(FunctionCall {
                namespace_token: Token::from_ident(""),
                name_token: Token::from_ident("realloc"),
                name: "realloc".into(),
                generics: vec![],
                parameters: vec![
                    (
                        location.clone(),
                        AstNode::FieldAccess(FieldAccess {
                            left: Box::new(AstNode::Environment(Environment {
                                value: None,
                                location: location.clone(),
                            })),
                            right: Box::new(AstNode::Literal(Literal {
                                kind: TokenKind::Identifier,
                                value: ValueKind::String("allocator".into()),
                                location: location.clone(),
                                tagged: false,
                            })),
                            value: None,
                            addr_only: false,
                            location: location.clone(),
                        }),
                    ),
                    (location.clone(), ptr),
                    (
                        location.clone(),
                        AstNode::BinaryOperation(BinaryOperation {
                            left: Box::new(AstNode::Size(Size {
                                value: Ok(ty),
                                location: location.clone(),
                            })),
                            right: Box::new(count),
                            operator: TokenKind::Multiply,
                            treat_as_string: false,
                            dunder_methods: true,
                            location: location.clone(),
                        }),
                    ),
                ],
                type_method: true,
                ignore_no_def: false,
                location: location.clone(),
            })),
            location: location.clone(),
            explicit: true,
        });

        if !self.is_eof() {
            match self.current_token().kind {
                TokenKind::Dot => {
                    expression = self.parse_field_access(Some((position, expression, location)));
                }

                TokenKind::LeftBlockBrace => {
                    expression = self.parse_offset_store(Some((position, expression, location)));
                }

                TokenKind::Semicolon => {}
                other if other.is_ternary_start() => {
                    expression = self.parse_ternary_node(expression, location);
                }

                other if other.is_arithmetic() => {
                    self.position = position;
                    return self.parse_arithmetic();
                }
                _ => expect_eot!(self.current_token()),
            }
        }

        expression
    }

    fn parse_enum_literal(&mut self) -> AstNode {
        let location = self.current_token().location;
        let position = self.position;

        let name = self.get_identifier();
        let name_token = self.current_token();
        self.advance();
        self.expect_tokens(&[TokenKind::DoubleColon]);
        self.advance();
        let variant = self.get_identifier();
        let variant_token = self.current_token();
        set_end!(location, self);

        let enum_def = self
            .shared
            .enum_pool
            .borrow()
            .get(&name)
            .cloned()
            .unwrap_or_else(|| {
                elle_error!(name_token
                    .location
                    .borrow()
                    .error(format!("Unknown enum '{name}'")))
            });

        enum_hover!(name_token, name, enum_def.0);

        let mut expression = AstNode::Conversion(Conversion {
            r#type: Some(Type::Enum(name.clone(), Box::new(enum_def.1))),
            value: Box::new(enum_def.0.iter().find(|x| x.name == variant).map_or_else(
                || {
                    elle_error!(variant_token.location.borrow().error(format!(
                        "Could not find a variant '{variant}' for enum '{name}'",
                    )))
                },
                |x| AstNode::token_to_literal(x.value.clone()),
            )),
            location: location.clone(),
            explicit: true,
        });

        if variant_token.tagged {
            let value = &enum_def
                .0
                .iter()
                .find(|x| x.name == variant)
                .unwrap() // we throw an error above if it doesn't exist
                .value
                .value;

            elle_error!(format!(
                "hover\n{}\n{}\n{}::{} = {}; // size = {}",
                variant_token.location.borrow().display_plain(false),
                variant_token.location.borrow().display_plain(true),
                name,
                variant,
                value.wrapped_display(),
                match value {
                    ValueKind::Nil => 0,
                    ValueKind::Number(_) => Type::Word.size_base(),
                    ValueKind::Character(_) => Type::Char.size_base(),
                    ValueKind::String(_) => Type::Pointer(Box::new(Type::Char)).size_base(),
                }
            ));
        }

        if let Some(token) = self.advance_opt() {
            match token.kind {
                TokenKind::Dot => {
                    expression = self.parse_field_access(Some((position, expression, location)));
                }

                TokenKind::LeftBlockBrace => {
                    expression = self.parse_offset_store(Some((position, expression, location)));
                }

                TokenKind::Semicolon => {}
                other if other.is_ternary_start() => {
                    expression = self.parse_ternary_node(expression, location);
                }

                other if other.is_arithmetic() => {
                    self.position = position;
                    return self.parse_arithmetic();
                }
                _ => expect_eot!(token),
            }
        }

        expression
    }

    fn parse_free(&mut self) -> AstNode {
        let location = self.current_token().location;
        let position = self.position;

        if self.current_token().tagged {
            elle_error!(format!(
                "hover\n{}\n{}\n#free(ptr_expr) -> T*\n",
                self.current_token().location.borrow().display_plain(false),
                self.current_token().location.borrow().display_plain(true),
            ));
        }

        self.advance();
        self.expect_tokens(&[TokenKind::LeftParenthesis]);
        self.advance();

        let mut tokens = vec![];
        let mut nesting = 0;

        while !self.is_eof() {
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

        set_end!(location, self);
        let ptr = Statement::new(tokens, 0, self.body, self.shared).parse().0;

        let mut expression = AstNode::FunctionCall(FunctionCall {
            namespace_token: Token::from_ident(""),
            name_token: Token::from_ident("free"),
            name: "free".into(),
            generics: vec![],
            parameters: vec![
                (
                    location.clone(),
                    AstNode::FieldAccess(FieldAccess {
                        left: Box::new(AstNode::Environment(Environment {
                            value: None,
                            location: location.clone(),
                        })),
                        right: Box::new(AstNode::Literal(Literal {
                            kind: TokenKind::Identifier,
                            value: ValueKind::String("allocator".into()),
                            location: location.clone(),
                            tagged: false,
                        })),
                        value: None,
                        addr_only: false,
                        location: location.clone(),
                    }),
                ),
                (location.clone(), ptr),
            ],
            type_method: true,
            ignore_no_def: false,
            location: location.clone(),
        });

        if let Some(token) = self.advance_opt() {
            match token.kind {
                TokenKind::Dot => {
                    expression = self.parse_field_access(Some((position, expression, location)));
                }

                TokenKind::LeftBlockBrace => {
                    expression = self.parse_offset_store(Some((position, expression, location)));
                }

                TokenKind::Semicolon => {}
                other if other.is_ternary_start() => {
                    expression = self.parse_ternary_node(expression, location);
                }

                other if other.is_arithmetic() => {
                    self.position = position;
                    return self.parse_arithmetic();
                }
                _ => expect_eot!(token),
            }
        }

        expression
    }

    fn parse_set_allocator(&mut self) -> AstNode {
        let location = self.current_token().location;
        let position = self.position;

        if self.current_token().tagged {
            elle_error!(format!(
                "hover\n{}\n{}\n#set_allocator(allocator_expr)\n",
                self.current_token().location.borrow().display_plain(false),
                self.current_token().location.borrow().display_plain(true),
            ));
        }

        self.advance();
        self.expect_tokens(&[TokenKind::LeftParenthesis]);
        self.advance();

        let mut tokens = vec![];
        let mut nesting = 0;

        while !self.is_eof() {
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

        set_end!(location, self);
        let allocator = Statement::new(tokens, 0, self.body, self.shared).parse().0;

        let mut expression = AstNode::SetAllocator(SetAllocator {
            value: Box::new(allocator),
            location: location.clone(),
        });

        if let Some(token) = self.advance_opt() {
            match token.kind {
                TokenKind::Dot => {
                    expression = self.parse_field_access(Some((position, expression, location)));
                }

                TokenKind::LeftBlockBrace => {
                    expression = self.parse_offset_store(Some((position, expression, location)));
                }

                TokenKind::Semicolon => {}
                other if other.is_ternary_start() => {
                    expression = self.parse_ternary_node(expression, location);
                }

                other if other.is_arithmetic() => {
                    self.position = position;
                    return self.parse_arithmetic();
                }
                _ => {}
            }
        }

        expression
    }

    fn parse_reset_allocator(&mut self) -> AstNode {
        let location = self.current_token().location;
        let position = self.position;

        if self.current_token().tagged {
            elle_error!(format!(
                "hover\n{}\n{}\n#reset_allocator()\n",
                self.current_token().location.borrow().display_plain(false),
                self.current_token().location.borrow().display_plain(true),
            ));
        }

        self.advance();
        self.expect_tokens(&[TokenKind::LeftParenthesis]);
        self.advance();

        self.expect_tokens(&[TokenKind::RightParenthesis]);
        set_end!(location, self);

        let mut expression = AstNode::SetAllocator(SetAllocator {
            value: Box::new(AstNode::FieldAccess(FieldAccess {
                left: Box::new(AstNode::Environment(Environment {
                    value: None,
                    location: location.clone(),
                })),
                right: Box::new(AstNode::Literal(Literal {
                    kind: TokenKind::Identifier,
                    value: ValueKind::String("default_allocator".into()),
                    location: location.clone(),
                    tagged: false,
                })),
                value: None,
                addr_only: false,
                location: location.clone(),
            })),
            location: location.clone(),
        });

        if let Some(token) = self.advance_opt() {
            match token.kind {
                TokenKind::Dot => {
                    expression = self.parse_field_access(Some((position, expression, location)));
                }

                TokenKind::LeftBlockBrace => {
                    expression = self.parse_offset_store(Some((position, expression, location)));
                }

                TokenKind::Semicolon => {}
                other if other.is_ternary_start() => {
                    expression = self.parse_ternary_node(expression, location);
                }

                other if other.is_arithmetic() => {
                    self.position = position;
                    return self.parse_arithmetic();
                }
                _ => expect_eot!(token),
            }
        }

        expression
    }

    fn parse_declarative_node(&mut self, node: AstNode) -> AstNode {
        let location = self.current_token().location;
        let operation = self.current_token();
        self.advance();

        let tokens = self.yield_tokens_wrapped_with_semi();
        set_end!(location, self);
        let mapping = operation.kind.to_non_declarative();

        AstNode::BinaryOperation(BinaryOperation {
            left: Box::new(node),
            right: Box::new(Statement::new(tokens, 0, self.body, self.shared).parse().0),
            operator: mapping,
            treat_as_string: true,
            dunder_methods: true,
            location,
        })
    }

    fn yield_tokens_for_unary(&mut self) -> Vec<Token> {
        let mut nesting = 0;
        let mut brace_nesting = 0;
        let mut block_nesting = 0;

        if self.is_eof() && self.current_token().kind == TokenKind::Address {
            elle_error!(self
                .current_token()
                .location
                .borrow()
                .error("Expected to yield tokens for unary but got end of stream."))
        }

        self.yield_tokens_with_condition(|token, prev_token, next_token| {
            if prev_token.kind == TokenKind::LeftParenthesis {
                nesting += 1;
            }

            if prev_token.kind == TokenKind::RightParenthesis {
                if nesting > 0 {
                    nesting -= 1;
                } else {
                    elle_error!(prev_token
                        .location
                        .borrow()
                        .error("Unbalanced brackets found parsing this unary expression"))
                }
            }

            if prev_token.kind == TokenKind::LeftCurlyBrace {
                brace_nesting += 1;
            }

            if prev_token.kind == TokenKind::RightCurlyBrace {
                if brace_nesting > 0 {
                    brace_nesting -= 1;
                } else {
                    elle_error!(prev_token
                        .location
                        .borrow()
                        .error("Unbalanced curly braces found parsing this unary expression"))
                }
            }

            if prev_token.kind == TokenKind::LeftBlockBrace {
                block_nesting += 1;
            }

            if prev_token.kind == TokenKind::RightBlockBrace {
                if block_nesting > 0 {
                    block_nesting -= 1;
                } else {
                    elle_error!(prev_token
                        .location
                        .borrow()
                        .error("Unbalanced block braces found parsing this unary expression"))
                }
            }

            if token.kind.is_arithmetic() {
                if token.kind == TokenKind::LessThan {
                    next_token.is_none_or(|token| {
                        !is_type!(token, self.shared, self.shared.generics, false)
                    })
                } else if token.kind == TokenKind::GreaterThan {
                    !is_type!(prev_token, self.shared, self.shared.generics, false)
                } else {
                    nesting == 0 && brace_nesting == 0 && block_nesting == 0
                }
            } else {
                (token.kind.is_declarative()
                    || token.kind == TokenKind::Semicolon
                    || token.kind == TokenKind::Equal
                    || token.kind == TokenKind::Question)
                    && nesting == 0
                    && brace_nesting == 0
                    && block_nesting == 0
            }
        })
    }

    fn yield_tokens_with_delimiters(&mut self, delimiters: &[TokenKind]) -> Vec<Token> {
        if delimiters.contains(&self.current_token().kind) {
            elle_error!(self.current_token().location.borrow().error(format!(
                "Expected expression but got {:?}",
                self.current_token().kind
            )));
        }

        self.yield_tokens_with_condition(|token, _, _| delimiters.contains(&token.kind))
    }

    fn yield_tokens_wrapped_with_semi(&mut self) -> Vec<Token> {
        let mut curly_nesting = i32::from(self.current_token().kind == TokenKind::LeftCurlyBrace);
        let mut block_nesting = i32::from(self.current_token().kind == TokenKind::LeftBlockBrace);

        self.yield_tokens_with_condition(|token, _, _| {
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
        })
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
                    AstNode::WhileLoopStatement(WhileLoopStatement {
                        condition,
                        step,
                        body,
                        location,
                    }) => {
                        let mut new_body = body;
                        insert_deferred_statements(&mut new_body, deferred, false);

                        new_nodes.push(AstNode::WhileLoopStatement(WhileLoopStatement {
                            condition,
                            step,
                            body: new_body,
                            location,
                        }));
                    }
                    AstNode::BlockStatement(BlockStatement { body, location }) => {
                        let mut new_body = body;
                        insert_deferred_statements(&mut new_body, deferred, false);

                        new_nodes.push(AstNode::BlockStatement(BlockStatement {
                            body: new_body,
                            location,
                        }));
                    }
                    AstNode::IfStatement(IfStatement {
                        condition,
                        body,
                        elifs,
                        else_body,
                        location,
                    }) => {
                        let mut new_body = body;
                        let mut new_else_body = else_body;
                        let mut new_elifs = elifs;

                        insert_deferred_statements(&mut new_body, deferred, false);
                        insert_deferred_statements(&mut new_else_body, deferred, false);

                        for (_, elif) in &mut new_elifs {
                            insert_deferred_statements(elif, deferred, false);
                        }

                        new_nodes.push(AstNode::IfStatement(IfStatement {
                            condition,
                            body: new_body,
                            elifs: new_elifs,
                            else_body: new_else_body,
                            location,
                        }));
                    }
                    _ => new_nodes.push(node),
                }
            }

            if !found_return && root {
                new_nodes.extend(deferred.clone());
            }

            *nodes = new_nodes;
        }

        let cell: RefCell<Vec<AstNode>> = RefCell::new(vec![]);

        while !self.is_eof() {
            let current = self.current_token();

            if current.kind == TokenKind::RightCurlyBrace {
                self.advance();

                if !self.is_eof() && expect_semicolon {
                    self.expect_tokens(&[TokenKind::Semicolon]);
                }

                break;
            }

            let (node, position, tokens, _) =
                Statement::new(self.tokens.clone(), self.position, &cell, self.shared).parse();

            cell.borrow_mut().push(node);
            self.position = position;
            self.tokens = tokens;
            self.advance();
        }

        let mut res = cell.borrow_mut().to_owned();
        let mut deferred: Vec<AstNode> = vec![];

        res.retain(|node| match node.clone() {
            AstNode::DeferStatement { value, .. } => {
                deferred.push(*value);
                false
            }
            _ => true,
        });

        deferred.reverse();
        insert_deferred_statements(&mut res, &deferred, true);
        res
    }

    fn is_type_contextually(&self, mut start: usize) -> bool {
        while let Some(x) = self.next_token_seek(start) {
            if x.kind == TokenKind::LeftParenthesis {
                start += 1;
                continue;
            }

            break;
        }

        if let Some(token) = self.next_token_seek(start) {
            is_type!(token, self.shared, self.shared.generics, true)
                && self // ensures this does not return true in the case of (math::max())
                    .next_token_seek(start + 1)
                    .is_none_or(|token| token.kind != TokenKind::DoubleColon)
                && self // ensures this does not return true in the case of (Foo {})
                    .next_token_seek(start + 1)
                    .is_none_or(|token| token.kind != TokenKind::LeftCurlyBrace)
        } else {
            false
        }
    }

    fn parse_primary(&mut self) -> AstNode {
        while self.current_token().kind == TokenKind::Semicolon {
            self.advance();
        }

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
            TokenKind::Environment => self.parse_env(),
            TokenKind::Alloc => self.parse_alloc(),
            TokenKind::Realloc => self.parse_realloc(),
            TokenKind::Free => self.parse_free(),
            TokenKind::SetAllocator => self.parse_set_allocator(),
            TokenKind::ResetAllocator => self.parse_reset_allocator(),
            TokenKind::Cast => self.parse_type_conversion(),
            TokenKind::Let => {
                self.advance();
                self.parse_declare(Some(Some(Type::Infer)))
            }
            TokenKind::LeftParenthesis => {
                if self.is_type_contextually(0) {
                    self.parse_declare(None)
                } else {
                    self.parse_wrapped_statement()
                }
            }
            TokenKind::Hashtag => {
                self.advance();

                if self.current_token().kind == TokenKind::LeftBlockBrace {
                    self.parse_array(false)
                } else {
                    elle_error!(self.current_token().location.borrow().error(format!(
                        "Expected left block brace or identifier, got {:?}",
                        self.current_token().kind
                    )))
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
                    let next = self.next_token().unwrap_or_else(|| {
                        elle_error!(self
                            .current_token()
                            .location
                            .borrow()
                            .error("Unexpected EOF when parsing an identifier"))
                    });

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
                            token.location.borrow().error(format!(
                                "Expected a field access ({name}.foo) but got {msg}",
                            ))
                        };

                        let tie = self
                            .next_token_seek(2)
                            .unwrap_or_else(|| elle_error!(unexpected_error(next, "EOF".into())));

                        match tie.clone().kind {
                            TokenKind::Yield => self.parse_yield_variadic(),
                            TokenKind::Identifier | TokenKind::ExactLiteral => {
                                self.parse_field_access(None)
                            }
                            other => elle_error!(unexpected_error(tie, format!("{other:?}"))),
                        }
                    } else if next.kind == TokenKind::Equal {
                        self.parse_declare(Some(None))
                    } else if next.kind == TokenKind::Comma {
                        self.parse_tuple_declare(None)
                    } else if next.kind == TokenKind::Colon {
                        if self
                            .next_token_seek(2)
                            .is_some_and(|token| token.kind == TokenKind::Equal)
                        {
                            self.parse_declare(Some(Some(Type::Infer)))
                        } else {
                            elle_error!(next.location.borrow().error(
                                "Cannot use a colon in this context. What were you trying to do?"
                            ))
                        }
                    } else if next.kind.is_declarative() {
                        self.parse_declarative_like()
                    } else if next.kind == TokenKind::LessThan {
                        if self.is_type_contextually(2) {
                            self.parse_function(None, None, None, None, false)
                        } else {
                            self.parse_arithmetic()
                        }
                    } else if next.kind.is_arithmetic() {
                        self.parse_arithmetic()
                    } else if next.kind.is_ternary_start() {
                        let condition = AstNode::token_to_literal(self.current_token());
                        self.advance();
                        self.parse_ternary_node(condition, self.current_token().location)
                    } else if next.kind == TokenKind::Identifier {
                        not_valid_struct_or_type!(self)
                    } else if next.kind == TokenKind::DoubleColon {
                        not_valid_struct_or_type!(self)
                    } else {
                        elle_error!(next.location.borrow().error(format!(
                            "Expected left parenthesis or arithmetic, got {:?}",
                            next.kind
                        )))
                    }
                }
            }
            _ => elle_error!(self.current_token().location.borrow().error(format!(
                "Expected expression, got {:?}\nMaybe you forgot a semicolon nearby?",
                self.current_token().kind
            ))),
        }
    }

    pub fn parse(&mut self) -> (AstNode, usize, Vec<Token>, bool) {
        if self.position >= 2 && self.tokens.len() > 1 {
            let prev = &self.tokens[self.position - 1];
            let kind = &prev.kind;

            if !(kind.is_arithmetic()
                || kind.is_declarative()
                || kind.is_brace()
                || kind == &TokenKind::Semicolon)
            {
                let token = self.tokens.get(self.position - 2).unwrap_or(prev);
                let location = token.location.clone();

                location.borrow_mut().ctx = Rc::from(format!("{} ", location.borrow().ctx));

                elle_error!(location
                    .borrow()
                    .error(format!("Expected semicolon here, but got {:?}", token.kind)))
            }
        }

        let position = self.position;
        let location = self.current_token().location;

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
            _ if is_type!(
                self.current_token(),
                self.shared,
                self.shared.generics,
                true
            ) =>
            {
                if let Some(token) = self.next_token() {
                    if token.kind == TokenKind::LeftCurlyBrace {
                        self.parse_struct_init()
                    } else if token.kind == TokenKind::Dot {
                        elle_error!(
                            token.location.borrow().error(format!(
                                "Cannot access methods on a struct or type '{}' using '.'\nPlease use '::' for non-instance method access.",
                                self.current_token().value.get_string_inner().unwrap()
                            ))
                        )
                    } else if token.kind == TokenKind::DoubleColon {
                        let ty = self.current_token();
                        let namespace = ty.value.get_string_inner().unwrap();
                        let method = self.next_token_seek(2).unwrap_or_else(|| {
                            elle_error!(self.current_token().location.borrow().error(format!(
                                "Expected {} name after '{}::'",
                                if self.shared.struct_pool.borrow().contains_key(&namespace) {
                                    "method"
                                } else {
                                    "variant"
                                },
                                namespace
                            )))
                        });

                        if method.kind != TokenKind::Identifier {
                            elle_error!(method.location.borrow().error(format!(
                                "Expected {} name in '{}::{}', but got '{:?}' instead.",
                                if self.shared.struct_pool.borrow().contains_key(&namespace) {
                                    "method"
                                } else {
                                    "variant"
                                },
                                namespace,
                                method
                                    .value
                                    .get_string_inner()
                                    .unwrap_or_else(|| format!("{}", method.value)),
                                method.kind
                            )));
                        }

                        if self.shared.enum_pool.borrow().contains_key(&namespace)
                            && self
                                .next_token_seek(3)
                                .is_none_or(|token| token.kind != TokenKind::LeftParenthesis)
                        {
                            self.parse_enum_literal()
                        } else {
                            self.advance(); // Skip namespace
                            self.advance(); // Skip double colon
                            self.advance(); // Skip func name

                            self.parse_function(
                                Some((
                                    location,
                                    ty.clone(),
                                    method.clone(),
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
                        }
                    } else {
                        self.parse_declare(None)
                    }
                } else {
                    self.parse_declare(None)
                }
            }
            _ => self.parse_expression(),
        };

        (node, self.position, self.tokens.clone(), self.consumed_addr)
    }
}
