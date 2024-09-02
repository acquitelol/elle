use std::collections::HashSet;

use crate::{
    compiler::enums::Type,
    ensure_fn_pointer, get_non_generic_type,
    lexer::enums::{Token, TokenKind, ValueKind},
    parser::{constant::Constant, function::Function, r#struct::Struct},
    Warnings,
};

use super::{enums::Primitive, r#use::Use};

#[derive(Eq, PartialEq)]
pub enum DoOnly {
    FunctionsAndConstants,
    NonGenericImportsAndGenericDefs,
    GenericImports,
    Structs,
}

pub struct Parser {
    pub tokens: Vec<Token>,
    pub position: usize,
    pub tree: Vec<Primitive>,
    pub struct_pool: HashSet<String>,
    pub external_generics: Vec<Type>,
    pub generic_keys: Vec<String>,
    pub generic_defaults: Vec<Option<Type>>,
    pub global_public: bool,
    pub warnings: Warnings,
}

impl Parser {
    pub fn new(
        tokens: Vec<Token>,
        struct_pool: HashSet<String>,
        external_generics: Vec<Type>,
        warnings: Warnings,
    ) -> Self {
        Parser {
            tokens,
            position: 0,
            tree: vec![],
            struct_pool,
            external_generics,
            generic_keys: vec![],
            generic_defaults: vec![],
            global_public: false,
            warnings,
        }
    }

    pub fn current_token(&self) -> Token {
        self.tokens[self.position].clone()
    }

    fn next_token(&mut self) -> Option<Token> {
        match self.is_eof() {
            true => None,
            false => Some(self.tokens[self.position + 1].clone()),
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

    pub fn is_eof(&mut self) -> bool {
        self.position >= self.tokens.len() - 1
    }

    pub fn match_token(&mut self, expected: TokenKind, advance: bool) -> bool {
        if self.current_token().kind == expected {
            match advance {
                true => self.advance(),
                _ => {}
            };

            true
        } else {
            false
        }
    }

    pub fn expect_tokens(&self, expected: Vec<TokenKind>) {
        if !expected.contains(&self.current_token().kind) {
            panic!(
                "{}",
                self.current_token().location.error(format!(
                    "Expected one of [{}], got {:?}.",
                    expected
                        .iter()
                        .map(|kind| format!("{:?}", kind))
                        .collect::<Vec<String>>()
                        .join(", "),
                    self.current_token().kind
                )),
            )
        }
    }

    pub fn get(&self, expected: TokenKind) -> String {
        self.expect_tokens(vec![expected.clone()]);

        let identifier = if let Token {
            value: ValueKind::String(identifier),
            ..
        } = self.current_token()
        {
            identifier.clone()
        } else {
            panic!(
                "Expected {:?} for function name, got {:?}",
                expected.clone(),
                self.current_token()
            );
        };

        identifier
    }

    pub fn get_identifier(&self) -> String {
        self.get(TokenKind::Identifier)
    }

    pub fn get_type(&mut self) -> Type {
        let is_fn_pointer = self.current_token().kind == TokenKind::Function;
        let name = if is_fn_pointer {
            self.current_token().value.get_string_inner().unwrap()
        } else {
            self.get(TokenKind::Identifier)
        };

        let is_valid = is_fn_pointer
            || self.struct_pool.contains(&name)
            || self.generic_keys.contains(&name)
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
            .to_type_string(self.struct_pool.contains(&name))
            .unwrap();

        ty = get_non_generic_type!(self.generic_keys, self.external_generics, ty);
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
                    // Crashes if it hasn't got at least 1 nested pointer for
                    // function pointers, ie `fn main(fn a)` is invalid
                    // you must have `fn main(fn *a)` instead.
                    _ => ensure_fn_pointer!(self, is_fn_pointer, found_ptr),
                }
            } else {
                ensure_fn_pointer!(self, is_fn_pointer, found_ptr)
            }
        }

        get_non_generic_type!(self.generic_keys, self.external_generics, ty)
    }

    // 0 - functions, constants, etc
    // 1 - non-generic imports and generic declarations only
    // 2 - generic imports only
    // 3 - structs only
    pub fn parse(
        &mut self,
        do_only: &DoOnly,
        new_struct_pool: Option<HashSet<String>>,
    ) -> (Vec<Primitive>, HashSet<String>) {
        if new_struct_pool.is_some() {
            self.struct_pool = new_struct_pool.unwrap();
        }

        self.position = 0;
        let mut public = false;
        let mut local = false;
        let mut external = false;

        let mut global_public = self.global_public;

        while self.position < self.tokens.len() - 1 {
            match self.current_token().kind {
                TokenKind::Global => {
                    self.advance();

                    match self.current_token().kind {
                        TokenKind::Public => {
                            global_public = true;
                        }
                        _ => panic!(
                            "{}",
                            self.current_token().location.error(format!(
                                "Invalid global specifier '{}'",
                                self.current_token()
                                    .value
                                    .get_string_inner()
                                    .unwrap_or(self.current_token().kind.to_string())
                            ))
                        ),
                    }

                    self.advance();
                    self.expect_tokens(vec![TokenKind::Semicolon]);
                    self.advance();
                }
                TokenKind::Public => {
                    public = true;
                    self.advance();
                }
                TokenKind::Local => {
                    local = true;
                    self.advance();
                }
                TokenKind::External if do_only == &DoOnly::FunctionsAndConstants => {
                    external = true;
                    self.advance();
                }
                TokenKind::Generic if do_only == &DoOnly::NonGenericImportsAndGenericDefs => {
                    self.advance();

                    while vec![TokenKind::Identifier, TokenKind::Comma, TokenKind::Equal]
                        .contains(&self.current_token().kind)
                    {
                        if self.current_token().kind == TokenKind::Comma {
                            self.advance();
                        }

                        let generic = self.get_identifier();
                        let mut default = None;
                        self.advance();

                        if self.current_token().kind == TokenKind::Equal {
                            self.advance();
                            default = Some(self.get_type());
                            self.advance();
                        }

                        self.generic_keys.push(generic);
                        self.generic_defaults.push(default);
                    }

                    self.expect_tokens(vec![TokenKind::Semicolon]);
                    self.advance();

                    public = false;
                    local = false;
                    external = false;
                }
                TokenKind::Use
                    if do_only == &DoOnly::NonGenericImportsAndGenericDefs
                        || do_only == &DoOnly::GenericImports =>
                {
                    let mut r#use = Use::new(self);
                    let statement = r#use.parse(do_only);

                    if (!r#use.has_generics && do_only == &DoOnly::NonGenericImportsAndGenericDefs)
                        || (r#use.has_generics && do_only == &DoOnly::GenericImports)
                    {
                        self.tree.push(statement);
                    }

                    public = false;
                    local = false;
                    external = false;
                }
                TokenKind::Function if do_only == &DoOnly::FunctionsAndConstants => {
                    if local && public {
                        panic!(
                            "{}",
                            self.current_token()
                                .location
                                .error("Cannot specify a function as both private and public")
                        );
                    }

                    if self.position >= 2 && self.tokens.len() > 2 {
                        let mut position = self.position.clone();

                        // Skip over function meta
                        while vec![
                            TokenKind::Function,
                            TokenKind::External,
                            TokenKind::Public,
                            TokenKind::Local,
                        ]
                        .contains(&self.tokens[position].kind)
                        {
                            position -= 1;
                        }

                        if position >= 1
                            && !vec![TokenKind::Semicolon, TokenKind::RightCurlyBrace]
                                .contains(&self.tokens[position].kind)
                        {
                            let mut location = self
                                .tokens
                                .get(self.position - 2)
                                .unwrap_or(&self.tokens[self.position - 2])
                                .location
                                .clone();

                            location.ctx.push(' ');
                            location.column += 1;

                            panic!(
                                "{}",
                                location.error("Expected semicolon here, but definition has ended")
                            )
                        }
                    }

                    let mut function = Function::new(self);

                    let statement = function.parse(
                        if local {
                            false
                        } else {
                            global_public || public
                        },
                        external,
                    );

                    self.tree.push(statement);

                    public = false;
                    local = false;
                    external = false;
                }
                TokenKind::Constant if do_only == &DoOnly::FunctionsAndConstants => {
                    if external {
                        panic!("{}", self.current_token().location.error("Cannot have an external constant. Please remove the `external` keyword."))
                    }

                    if local && public {
                        panic!(
                            "{}",
                            self.current_token()
                                .location
                                .error("Cannot specify a constant as both private and public")
                        );
                    }

                    let mut constant = Constant::new(self);

                    let statement = constant.parse(if local {
                        false
                    } else {
                        global_public || public
                    });

                    self.tree.push(statement);

                    public = false;
                    local = false;
                    external = false;
                }
                TokenKind::Struct if do_only == &DoOnly::Structs => {
                    if external {
                        panic!("{}", self.current_token().location.error("Cannot have an external struct. Please remove the `external` keyword."))
                    }

                    if local && public {
                        panic!(
                            "{}",
                            self.current_token()
                                .location
                                .error("Cannot specify a struct as both private and public")
                        );
                    }

                    let mut r#struct = Struct::new(self);

                    let statement = r#struct.parse(if local {
                        false
                    } else {
                        global_public || public
                    });

                    self.tree.push(statement);

                    public = false;
                    local = false;
                    external = false;
                }
                _ => {
                    self.advance();
                }
            }
        }

        self.global_public = global_public;
        return (self.tree.clone(), self.struct_pool.clone());
    }
}
