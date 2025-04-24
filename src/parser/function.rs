use std::cell::RefCell;

use crate::{
    compiler::qbe::r#type::Type,
    elle_error,
    lexer::enums::{Attribute, Location, MutRc, Token, TokenKind, ValueKind},
    parser::{
        enums::{BlockStatement, FunctionSource, IfStatement, VariadicStart, WhileLoopStatement},
        statement::Shared,
    },
    set_end, Warning, META_STRUCT_NAME,
};

use super::{
    enums::{Argument, AstNode, Primitive},
    parser::Parser,
    statement::Statement,
};

pub struct Function<'a> {
    parser: &'a mut Parser,
}

impl<'a> Function<'a> {
    pub fn new(parser: &'a mut Parser) -> Self {
        Function { parser }
    }

    pub fn parse(
        &mut self,
        public: bool,
        external: bool,
        should_parse: bool,
        location: MutRc<Location>,
    ) -> Option<Primitive> {
        self.parser.advance();

        if !should_parse {
            if external {
                while self.parser.current_token().kind != TokenKind::Semicolon
                    && !self.parser.is_eof()
                {
                    self.parser.advance();
                }

                self.parser.expect_tokens(vec![TokenKind::Semicolon]);
                self.parser.advance();
            } else {
                while self.parser.current_token().kind != TokenKind::LeftCurlyBrace
                    && !self.parser.is_eof()
                {
                    self.parser.advance();
                }

                self.parser.expect_tokens(vec![TokenKind::LeftCurlyBrace]);
                self.parser.advance();
                let mut nesting = 0;

                while !self.parser.is_eof() {
                    if self.parser.current_token().kind == TokenKind::LeftCurlyBrace {
                        nesting += 1;
                    }

                    if self.parser.current_token().kind == TokenKind::RightCurlyBrace {
                        if nesting > 0 {
                            nesting -= 1;
                        } else {
                            break;
                        }
                    }

                    self.parser.advance();
                }

                self.parser.expect_tokens(vec![TokenKind::RightCurlyBrace]);
                self.parser.advance(); // Go past the right curly brace
            }

            return None;
        }

        let mut name = self.parser.get_identifier();
        let mut namespace_token = Token::from_ident("");
        let mut name_token = self.parser.current_token();

        self.parser.advance();

        if self.parser.current_token().kind == TokenKind::Dot {
            elle_error!(self.parser.current_token().location.borrow().error(format!(
                "Cannot create a method for '{}' using '.'\nPlease use '::' instead.",
                name
            )))
        }

        if self.parser.current_token().kind == TokenKind::DoubleColon {
            if !(self.parser.struct_pool.borrow().contains_key(&name)
                || ValueKind::String(name.clone()).is_base_type())
            {
                elle_error!(
                    location.borrow().error(format!(
                        "Cannot create a method for '{}' because it isn't a struct or primitive type.\n{}",
                        name.clone(), if let Some(map) = ValueKind::similar_mapping(name.clone()) {
                            format!("A similar type exists which might be what you need: '{}'", map)
                        } else {
                            format!("Are you sure you spelt '{}' correctly?", name)
                        }
                    ))
                )
            }

            self.parser.advance();

            let identifier = self.parser.get_identifier();
            name = format!("{}.{}", name, identifier);

            namespace_token = name_token;
            name_token = self.parser.current_token();
            self.parser.advance();
        }

        let mut generics = vec![];

        if self.parser.current_token().kind == TokenKind::LessThan {
            self.parser.advance();

            while self.parser.current_token().kind != TokenKind::GreaterThan {
                generics.push(self.parser.get_identifier());
                self.parser.advance();

                if self.parser.current_token().kind == TokenKind::Comma {
                    self.parser.advance();
                }
            }

            self.parser.expect_tokens(vec![TokenKind::GreaterThan]);
            self.parser.advance();
        }

        self.parser.expect_tokens(vec![TokenKind::LeftParenthesis]);
        self.parser.advance();

        let mut arguments = vec![];
        let mut variadic = false;
        let mut variadic_name = None;

        let ty_name = self
            .parser
            .current_token()
            .value
            .get_string_inner()
            .unwrap_or("".into());

        if self.parser.current_token().kind == TokenKind::Identifier
            && (self.parser.current_token().value.is_base_type()
                || generics.contains(
                    &self
                        .parser
                        .current_token()
                        .value
                        .get_string_inner()
                        .unwrap(),
                )
                || self.parser.struct_pool.borrow().contains_key(&ty_name))
            // TODO: Fix this (start of a tuple type, BIGGGG hack)
            || self.parser.current_token().kind == TokenKind::LeftParenthesis
            || self.parser.current_token().kind == TokenKind::Attribute
            || self.parser.current_token().kind == TokenKind::Ellipsis
            || self.parser.current_token().kind == TokenKind::Function
        {
            while self.parser.current_token().kind != TokenKind::RightParenthesis {
                if self.parser.current_token().kind == TokenKind::Ellipsis {
                    self.parser.advance();

                    if self.parser.current_token().kind == TokenKind::Identifier {
                        variadic_name = Some(self.parser.current_token());
                        self.parser.advance();
                    }

                    variadic = true;
                    break;
                }

                let mut no_fmt = false;

                if self.parser.current_token().kind == TokenKind::Attribute {
                    self.parser.advance();

                    match self.parser.current_token().parse_attribute() {
                        Attribute::NoFormat => {
                            no_fmt = true;
                            self.parser.advance();
                        }
                        _ => {}
                    };
                }

                let r#type = self.parser.get_type(Some(&generics));
                let ty_loc = self.parser.current_token().location.clone();

                self.parser.advance();

                // fn foo(void) isn't supported
                if r#type == Type::Void
                    && self.parser.current_token().kind == TokenKind::RightParenthesis
                {
                    if self.parser.warnings.has_warning(Warning::CStyleVoid) {
                        eprintln!(
                            "{}",
                            ty_loc.borrow().warning("Elle does not support C-style explicit function prototypes.\nPlease remove the 'void' type from this function's signature.\nThis is a warning, which means the compiler will ignore this.")
                        )
                    }

                    break;
                }

                let name = match self.parser.current_token().kind {
                    TokenKind::Identifier => self.parser.get_identifier(),
                    other => elle_error!(self
                        .parser
                        .current_token()
                        .location
                        .borrow()
                        .error(format!("Invalid token type: {:?}", other))),
                };

                self.parser.advance();
                self.parser.match_token(TokenKind::Comma, true);

                arguments.push(Argument {
                    r#type,
                    name,
                    no_fmt,
                })
            }
        }

        self.parser.expect_tokens(vec![TokenKind::RightParenthesis]);
        self.parser.advance();

        if !external
            && variadic
            && (arguments.is_empty()
                || arguments[0].r#type != Type::Struct(META_STRUCT_NAME.into()))
            && self.parser.warnings.has_warning(Warning::VariadicNoMeta)
        {
            eprintln!("{}", location.borrow().warning(
                format!(
                    "Generating a variadic function named '{}' without the ElleMeta struct.\nThis internal structure provides you with arity, it may be useful.\nAre you sure you want to create this function without it?",
                    name
                )
            ));
        }

        let mut r#return = None;
        let mut unaliased = None;
        let mut volatile = false;
        let mut format = false;

        if self.parser.match_token(TokenKind::Attribute, false) {
            while self.parser.current_token().kind == TokenKind::Attribute {
                self.parser.advance();
                let location = self.parser.current_token().location.clone();
                let attribute = self.parser.current_token().parse_attribute();

                match attribute {
                    Attribute::Alias => {
                        self.parser.advance();
                        self.parser.expect_tokens(vec![TokenKind::LeftParenthesis]);
                        self.parser.advance();

                        let mut alias = self.parser.get_identifier();

                        if let Some(token) = self.parser.tokens.get(self.parser.position + 1) {
                            if token.kind == TokenKind::DoubleColon {
                                self.parser.advance(); // past namespace
                                self.parser.advance(); // past ::

                                let identifier = self.parser.get_identifier();
                                alias = format!("{alias}.{identifier}");
                            }
                        }

                        if external {
                            unaliased = Some(name);
                            name = alias;
                        } else {
                            if self.parser.warnings.has_warning(Warning::InvalidAlias) {
                                eprintln!(
                                    "{}",
                                    location.borrow().warning(format!(
                                        "Can't assign aliases to non-external functions\nSkipping alias '{}' for function '{}'",
                                        alias, name.replace(".", "::")
                                    ))
                                )
                            }
                        }

                        self.parser.advance();
                        self.parser.expect_tokens(vec![TokenKind::RightParenthesis]);
                        self.parser.advance();
                    }
                    Attribute::Volatile => {
                        volatile = true;
                        self.parser.advance();
                    }
                    Attribute::Format => {
                        format = true;
                        self.parser.advance();
                    }
                    _ => elle_error!(self.parser.current_token().location.borrow().error(format!(
                        "Unknown attribute for function '{}'",
                        self.parser
                            .current_token()
                            .value
                            .get_string_inner()
                            .unwrap()
                    ))),
                }
            }
        }

        let mut return_location = self.parser.current_token().location.clone();

        if self.parser.match_token(TokenKind::RightArrow, true) {
            return_location = self.parser.current_token().location.clone();
            r#return = Some(self.parser.get_type(Some(&generics)));
            self.parser.advance();
        }

        if external {
            self.parser.expect_tokens(vec![TokenKind::Semicolon]);
            set_end!(location, self.parser);
            self.parser.advance();

            return Some(Primitive::Function(FunctionSource {
                public,
                variadic,
                name_token,
                namespace_token,
                name,
                external,
                builtin: false,
                volatile: false,
                format,
                unaliased,
                generics,
                arguments,
                r#return,
                body: vec![],
                usable: true,
                imported: false,
                location,
                return_location: return_location.clone(),
            }));
        }

        self.parser.expect_tokens(vec![TokenKind::LeftCurlyBrace]);

        let body: RefCell<Vec<AstNode>> = RefCell::new(vec![]);

        if let Some(name) = variadic_name {
            body.borrow_mut()
                .push(AstNode::VariadicStart(VariadicStart {
                    name,
                    location: self.parser.current_token().location,
                }));
        }

        while !self.parser.is_eof() {
            self.parser.advance();

            let current = self.parser.current_token();

            match current.kind {
                TokenKind::RightCurlyBrace => {
                    self.parser.advance();
                    break;
                }
                _ => {
                    let (node, position, tokens) = Statement::new(
                        self.parser.tokens.clone(),
                        self.parser.position.clone(),
                        &body,
                        &Shared {
                            struct_pool: &self.parser.struct_pool,
                            enum_pool: &self.parser.enum_pool,
                            tree: &self.parser.tree,
                            generics: &generics,
                            known_generics: &vec![],
                        },
                    )
                    .parse();

                    body.borrow_mut().push(node);
                    self.parser.position = position;
                    self.parser.tokens = tokens;
                }
            };
        }

        let mut res = body.borrow_mut().to_owned().clone();
        let mut deferred: Vec<AstNode> = vec![];

        res.retain(|node| match node.clone() {
            AstNode::DeferStatement { value, .. } => {
                deferred.push(*value.clone());
                false
            }
            _ => true,
        });

        deferred.reverse();

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

                        for (_cond, elif) in new_elifs.iter_mut() {
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

        insert_deferred_statements(&mut res, &deferred, true);
        set_end!(location, self.parser);

        Some(Primitive::Function(FunctionSource {
            public,
            variadic,
            name,
            name_token,
            namespace_token,
            external,
            builtin: false,
            volatile,
            format,
            unaliased,
            generics,
            arguments,
            r#return,
            body: res,
            usable: true,
            imported: false,
            location,
            return_location,
        }))
    }
}
