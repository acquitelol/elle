use std::{cell::RefCell, collections::HashMap};

use crate::{
    compiler::enums::Type,
    ensure_fn_pointer,
    lexer::enums::{Location, Token, TokenKind, ValueKind},
    misc::colors::*,
    parser::{constant::Constant, function::Function, r#struct::Struct},
    Warnings, GENERIC_END, GENERIC_IDENTIFIER,
};

use super::{
    enums::{Argument, Primitive},
    r#use::Use,
};

#[derive(Eq, PartialEq)]
pub enum DoOnly {
    FunctionsAndConstants,
    Imports,
    Structs,
}

pub type StructPool = HashMap<String, (Vec<String>, Vec<Argument>, Location)>;

pub fn create_generic_struct(
    name: String,
    generic_name: String,
    mut location: Location,
    known_generics: Vec<Type>,
    struct_pool: &RefCell<StructPool>,
    extra_structs: &RefCell<Vec<Primitive>>,
) {
    let (generics, members, struct_location) = struct_pool.borrow().get(&name).unwrap().clone();

    if generics.len() != known_generics.len() {
        if generics.len() < known_generics.len() {
            todo!("the user passed too many generics");
        }

        let unknown = generics
            .iter()
            .cloned()
            .skip(known_generics.len())
            .collect::<Vec<String>>();

        location.column -= location.ctx.len() - location.ctx.trim().len();
        location.ctx = location.ctx.trim().into();
        location.length = location.column;
        location.column = 0;
        location.above = Some(format!(
            "In struct:\n{GREEN}{BOLD}{}{}{RESET}\n\n",
            " ".repeat(
                location.ctx.len() - location.ctx.trim().len()
                    + format!("{}", location.row + 1).len()
                    + 8
            ),
            struct_location.ctx
        ));

        panic!(
            "{}",
            location.error(format!(
                "Mismatched number of generics in struct {}<{}>.\nCould not find generic{} {} where the function specifies <{}>.",
                name.replace(".", "::"),
                generics.join(", "),
                if unknown.len() == 1 { "" } else { "s" },
                unknown.join(", "),
                generics.join(", ")
            ))
        )
    }

    let parsed_generics = HashMap::from_iter(
        generics
            .iter()
            .enumerate()
            .map(|(i, generic)| (generic.clone(), known_generics[i].clone())),
    );

    let parsed_members = members
        .iter()
        .map(|member| Argument {
            name: member.name.clone(),
            r#type: member.r#type.clone().unknown_to_known(
                Some(struct_pool),
                Some(extra_structs),
                generics.clone(),
                parsed_generics.clone(),
            ),
        })
        .collect::<Vec<Argument>>();

    extra_structs.borrow_mut().push(Primitive::Struct {
        name: generic_name.clone(),
        public: false,
        usable: true,
        imported: false,
        generics: vec![],
        known_generics: parsed_generics,
        members: parsed_members.clone(),
        location: location.clone(),
    });

    struct_pool
        .borrow_mut()
        .insert(generic_name.clone(), (vec![], parsed_members, location));
}

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
    pub extra_structs: RefCell<Vec<Primitive>>,
    pub tree: RefCell<Vec<Primitive>>,
    // Map of struct name to members and generics
    pub struct_pool: RefCell<StructPool>,
    pub global_public: bool,
    pub warnings: Warnings,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, struct_pool: StructPool, warnings: Warnings) -> Self {
        Parser {
            tokens,
            position: 0,
            extra_structs: RefCell::new(vec![]),
            tree: RefCell::new(vec![]),
            struct_pool: RefCell::new(struct_pool),
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

    pub fn get_type(&mut self, generics: Option<&Vec<String>>) -> Type {
        let is_fn_pointer = self.current_token().kind == TokenKind::Function;
        let name = if is_fn_pointer {
            self.current_token().value.get_string_inner().unwrap()
        } else {
            self.get(TokenKind::Identifier)
        };

        let is_struct = self.struct_pool.borrow().contains_key(&name);
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
            .to_type_string(self.struct_pool.borrow().contains_key(&name))
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

                        if !self.struct_pool.borrow().contains_key(&generic_name) {
                            create_generic_struct(
                                name.clone(),
                                generic_name.clone(),
                                location,
                                known_generics,
                                &self.struct_pool,
                                &self.extra_structs,
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

        get_non_generic_type!(self.generic_keys, self.external_generics, ty)
    }

    // 0 - functions, constants, etc
    // 1 - non-generic imports and generic declarations only
    // 2 - generic imports only
    // 3 - structs only
    pub fn parse(
        &mut self,
        do_only: &DoOnly,
        new_struct_pool: Option<StructPool>,
    ) -> (Vec<Primitive>, StructPool, Vec<Primitive>) {
        if new_struct_pool.is_some() {
            self.struct_pool = RefCell::new(new_struct_pool.unwrap());
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
                TokenKind::Use if do_only == &DoOnly::Imports => {
                    let mut r#use = Use::new(self);
                    let statement = r#use.parse();
                    self.tree.borrow_mut().push(statement);

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

                    self.tree.borrow_mut().push(statement);

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

                    self.tree.borrow_mut().push(statement);

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

                    self.tree.borrow_mut().push(statement);

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
        return (
            self.tree.borrow_mut().to_owned(),
            self.struct_pool.borrow_mut().to_owned(),
            self.extra_structs.borrow_mut().to_owned(),
        );
    }
}
