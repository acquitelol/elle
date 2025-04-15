use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    compiler::qbe::r#type::Type,
    elle_error, ensure_fn_pointer,
    lexer::enums::{Location, Token, TokenKind, ValueKind},
    misc::colors::*,
    parser::{constant::Constant, function::Function, r#struct::Struct},
    Warnings, GENERIC_END, GENERIC_IDENTIFIER,
};

use super::{
    enums::{Argument, Primitive, StructSource},
    r#use::Use,
};

#[derive(Eq, PartialEq)]
pub enum DoOnly {
    FunctionsAndConstants,
    Imports,
    Structs,
}

pub type StructPool = HashMap<String, (Vec<String>, Vec<Argument>, Rc<Location>)>;

pub fn create_generic_struct(
    name: String,
    generic_name: String,
    mut location: Location,
    known_generics: Vec<Type>,
    struct_pool: &RefCell<StructPool>,
    tree: &RefCell<Vec<Primitive>>,
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

        location.above = Some(Rc::from(format!(
            "In struct:\n{GREEN}{BOLD}{}{}{RESET}\n\n",
            " ".repeat(location.ctx.len() - location.ctx.trim().len() + 8),
            struct_location.ctx,
            GREEN = get_GREEN!(),
            BOLD = get_BOLD!(),
            RESET = get_RESET!()
        )));

        elle_error!(
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
                Some(tree),
                generics.clone(),
                parsed_generics.clone(),
            ),
            no_fmt: member.no_fmt,
        })
        .collect::<Vec<Argument>>();

    tree.borrow_mut().push(Primitive::Struct(StructSource {
        name: generic_name.clone(),
        public: false,
        usable: true,
        imported: false,
        generics: vec![],
        known_generics: parsed_generics,
        members: parsed_members.clone(),
        keyword_location: Rc::new(location.clone()),
        location: Rc::new(location.clone()),
        ignore_empty: false,
    }));

    struct_pool.borrow_mut().insert(
        generic_name.clone(),
        (vec![], parsed_members, Rc::new(location)),
    );
}

#[macro_export]
macro_rules! get_type {
    ($self:expr, $generics:expr, $struct_pool:expr, $tree:expr) => {{
        let mut is_fn_pointer = false;
        let mut is_struct = false;
        let mut tuple_imported = true;
        let name;

        let location = (*$self.current_token().location).clone();

        let mut ty = if $self.current_token().kind == TokenKind::LeftParenthesis {
            if !$struct_pool.borrow().contains_key("Tuple") {
                tuple_imported = false;
            }

            $self.advance(); // Skip left parenthesis

            let mut types = vec![];

            while $self.current_token().kind != TokenKind::RightParenthesis
                && !$self.is_eof() {
                types.push($self.get_type($generics));
                $self.advance();

                if $self.current_token().kind == TokenKind::Comma {
                    $self.advance();
                }
            }

            name = "Tuple".into();
            let generic_name = format!(
                "{name}.{GENERIC_IDENTIFIER}.{}.{GENERIC_END}",
                types
                    .iter()
                    .map(|ty| ty.to_internal_id().to_string())
                    .collect::<Vec<String>>()
                    .join(".")
            );

            $self.expect_tokens(vec![TokenKind::RightParenthesis]);
            let mut cloned_location = location.clone();
            cloned_location.end = $self.current_token().location.end.clone();

            if !tuple_imported {
                let import_text = "use std/collections/tuple;";

                elle_error!(
                    cloned_location.error(
                        format!(
                            "The tuple module is not imported. Please import it to use tuples.\n\n`{}`",
                            import_text
                        )
                    )
                );
            }

            if !$struct_pool.borrow().contains_key(&generic_name) {
                create_generic_struct(
                    "Tuple".into(),
                    generic_name.clone(),
                    cloned_location,
                    types,
                    &$struct_pool,
                    &$tree,
                )
            }

            Type::Pointer(Box::new(Type::Struct(generic_name)))
        } else {
            is_fn_pointer = $self.current_token().kind == TokenKind::Function;

            name = if is_fn_pointer {
                $self.current_token().value.get_string_inner().unwrap()
            } else {
                $self.get(vec![TokenKind::Identifier])
            };

            is_struct = $struct_pool.borrow().contains_key(&name);
            let is_valid = is_fn_pointer
                || is_struct
                || $generics.unwrap_or(&vec![]).contains(&name)
                || ValueKind::String(name.clone()).is_base_type();

            if !is_valid {
                elle_error!(
                    $self.current_token().location.error(format!(
                        "Type or struct named '{}' could not be found. Are you sure you spelt it correctly?",
                        name
                    ))
                )
            }

            ValueKind::String(name.clone())
                .to_type_string($struct_pool.borrow().contains_key(&name))
                .unwrap()
        };

        let mut found_ptr = false;

        while !$self.is_eof() {
            let tmp = $self.next_token();

            if tmp.is_some() {
                match tmp.unwrap().kind {
                    TokenKind::Multiply | TokenKind::Deref => {
                        found_ptr = true;
                        ty = Type::Pointer(Box::new(ty));
                        $self.advance();
                    }
                    TokenKind::LeftBlockBrace => {
                        $self.advance();
                        $self.advance();

                        if !$struct_pool.borrow().contains_key("Array") {
                            let mut cloned_location = location.clone();
                            cloned_location.end = $self.current_token().location.end.clone();
                            let import_text = "use std/collections/array;";

                            elle_error!(
                                cloned_location.error(
                                    format!(
                                        "The array module is not imported. Please import it to use dynamic arrays.\n\n`{}`",
                                        import_text
                                    )
                                )
                            );
                        }

                        let generic_name = format!(
                            "Array.{GENERIC_IDENTIFIER}.{}.{GENERIC_END}",
                            ty.to_internal_id().to_string()
                        );

                        let mut cloned_location = location.clone();
                        cloned_location.end = $self.current_token().location.end.clone();

                        if !$struct_pool.borrow().contains_key(&generic_name) {
                            create_generic_struct(
                                "Array".into(),
                                generic_name.clone(),
                                cloned_location,
                                vec![ty],
                                &$struct_pool,
                                &$tree,
                            )
                        }

                        ty = Type::Pointer(Box::new(Type::Struct(generic_name)));
                        $self.expect_tokens(vec![TokenKind::RightBlockBrace]);
                    }
                    TokenKind::LessThan if is_struct => {
                        $self.advance();
                        $self.advance();

                        let mut known_generics = vec![];

                        while $self.current_token().kind != TokenKind::GreaterThan
                            && !$self.is_eof() {
                            known_generics.push($self.get_type($generics));
                            $self.advance();

                            if $self.current_token().kind == TokenKind::Comma {
                                $self.advance();
                            }
                        }

                        let mut cloned_location = location.clone();
                        cloned_location.end = $self.current_token().location.end.clone();

                        let generic_name = format!(
                            "{name}.{GENERIC_IDENTIFIER}.{}.{GENERIC_END}",
                            known_generics
                                .iter()
                                .map(|known| known.to_internal_id().to_string())
                                .collect::<Vec<String>>()
                                .join(".")
                        );

                        if !$struct_pool.borrow().contains_key(&generic_name) {
                            create_generic_struct(
                                name.clone(),
                                generic_name.clone(),
                                cloned_location,
                                known_generics,
                                &$struct_pool,
                                &$tree,
                            )
                        }

                        ty = Type::Struct(generic_name);
                        $self.expect_tokens(vec![TokenKind::GreaterThan]);
                    }
                    // Crashes if it hasn't got at least 1 nested pointer for
                    // function pointers, ie `fn main(fn a)` is invalid
                    // you must have `fn main(fn *a)` instead.
                    _ => ensure_fn_pointer!($self, is_fn_pointer, found_ptr),
                }
            } else {
                ensure_fn_pointer!($self, is_fn_pointer, found_ptr)
            }
        }

        ty
    }};
}

pub struct Parser {
    pub tokens: Vec<Token>,
    pub position: usize,
    pub tree: RefCell<Vec<Primitive>>,
    // Map of struct name to members and generics
    pub struct_pool: RefCell<StructPool>,
    pub global_public: bool,
    pub global_external: bool,
    pub warnings: Warnings,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, struct_pool: StructPool, warnings: Warnings) -> Self {
        Parser {
            tokens,
            position: 0,
            tree: RefCell::new(vec![]),
            struct_pool: RefCell::new(struct_pool),
            global_public: false,
            global_external: false,
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
            elle_error!(self.current_token().location.error(format!(
                "Expected one of [{}], got {:?}.",
                expected
                    .iter()
                    .map(|kind| format!("{:?}", kind))
                    .collect::<Vec<String>>()
                    .join(", "),
                self.current_token().kind
            )))
        }
    }

    pub fn get(&self, expected: Vec<TokenKind>) -> String {
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
            elle_error!(token.location.error(format!(
                "Expected one of {:?} for function name, got {:?}",
                expected.clone(),
                self.current_token()
            )));
        };

        identifier
    }

    pub fn get_identifier(&self) -> String {
        self.get(vec![TokenKind::Identifier, TokenKind::ExactLiteral])
    }

    pub fn get_type(&mut self, generics: Option<&Vec<String>>) -> Type {
        get_type!(self, generics, self.struct_pool, self.tree)
    }

    // 0 - functions, constants, etc
    // 1 - non-generic imports and generic declarations only
    // 2 - generic imports only
    // 3 - structs only
    pub fn parse(
        &mut self,
        do_only: &DoOnly,
        new_struct_pool: Option<StructPool>,
    ) -> (Vec<Primitive>, StructPool) {
        if new_struct_pool.is_some() {
            self.struct_pool = RefCell::new(new_struct_pool.unwrap());
        }

        self.position = 0;
        let mut location = (*self.current_token().location).clone();
        let mut public = false;
        let mut local = false;
        let mut defined = false;
        let mut external = false;

        macro_rules! clean {
            () => {{
                location = (*self.current_token().location).clone();
                public = false;
                local = false;
                defined = false;
                external = false;
            }};
        }

        let mut global_public = self.global_public;
        let mut global_external = self.global_external;

        // TODO: Change the parser to group together tokens which makes it easier to skip them for the wrong pass
        // ie: functions should be `fn <name>($1) { $2 }` where $1 and $2 are seperate tagged token streams which can be easily skipped
        while self.position < self.tokens.len() - 1 {
            match self.current_token().kind {
                TokenKind::Global => {
                    self.advance();

                    macro_rules! match_one {
                        () => {{
                            match self.current_token().kind {
                                TokenKind::Public => {
                                    global_public = true;
                                }
                                TokenKind::External => {
                                    global_external = true;
                                }
                                _ => elle_error!(self.current_token().location.error(format!(
                                    "Invalid global identifier named '{}'",
                                    self.current_token()
                                        .value
                                        .get_string_inner()
                                        .unwrap_or(self.current_token().kind.to_string())
                                ))),
                            }

                            self.advance();
                        }};
                    }

                    match_one!(); // Must have one identifier

                    // Match until no more commas
                    while self.current_token().kind == TokenKind::Comma && !self.is_eof() {
                        self.advance();
                        match_one!()
                    }

                    self.expect_tokens(vec![TokenKind::Semicolon]);
                    self.advance();
                }
                TokenKind::Not => {
                    self.advance();

                    match self.current_token().kind {
                        TokenKind::Public => {
                            local = true;
                        }
                        TokenKind::External => {
                            defined = true;
                        }
                        _ => elle_error!(self.current_token().location.error(format!(
                            "Invalid local specifier named '{}'",
                            self.current_token()
                                .value
                                .get_string_inner()
                                .unwrap_or(self.current_token().kind.to_string())
                        ))),
                    }

                    self.advance();
                }
                TokenKind::Public => {
                    public = true;
                    self.advance();
                }
                TokenKind::External => {
                    external = true;
                    self.advance();
                }
                TokenKind::Use => {
                    let mut r#use = Use::new(self);
                    let statement = r#use.parse();

                    if do_only == &DoOnly::Imports {
                        self.tree.borrow_mut().push(statement);
                    }

                    clean!()
                }
                TokenKind::Function => {
                    if local && public {
                        elle_error!(self
                            .current_token()
                            .location
                            .error("Cannot specify a function as both private and public"));
                    }

                    // function pointer
                    if let Some(next) = self.next_token() {
                        if next.kind == TokenKind::Multiply {
                            self.advance();
                            continue;
                        }
                    }

                    let mut function = Function::new(self);

                    let statement = function.parse(
                        if local {
                            false
                        } else {
                            global_public || public
                        },
                        if defined {
                            false
                        } else {
                            global_external || external
                        },
                        do_only == &DoOnly::FunctionsAndConstants,
                        location.clone(),
                    );

                    // Will only be Some(T) if do_only == &DoOnly::FunctionsAndConstants
                    if let Some(statement) = statement {
                        self.tree.borrow_mut().push(statement);
                    }

                    clean!()
                }
                TokenKind::Constant => {
                    if external {
                        elle_error!(self.current_token().location.error("Cannot have an external constant. Please remove the `external` keyword."))
                    }

                    if local && public {
                        elle_error!(self
                            .current_token()
                            .location
                            .error("Cannot specify a constant as both private and public"));
                    }

                    let mut constant = Constant::new(self);

                    let statement = constant.parse(
                        if local {
                            false
                        } else {
                            global_public || public
                        },
                        do_only == &DoOnly::FunctionsAndConstants,
                        location.clone(),
                    );

                    if let Some(statement) = statement {
                        self.tree.borrow_mut().push(statement);
                    }

                    clean!()
                }
                TokenKind::Struct => {
                    if external {
                        elle_error!(self.current_token().location.error(
                            "Cannot have an external struct. Please remove the `external` keyword."
                        ))
                    }

                    if local && public {
                        elle_error!(self
                            .current_token()
                            .location
                            .error("Cannot specify a struct as both private and public"));
                    }

                    let mut r#struct = Struct::new(self);

                    let res = r#struct.parse(
                        if local {
                            false
                        } else {
                            global_public || public
                        },
                        false,
                        do_only == &DoOnly::Structs,
                        location.clone(),
                    );

                    if let Some((statement, mut builtins)) = res {
                        self.tree.borrow_mut().push(statement);
                        self.tree.borrow_mut().append(&mut builtins);
                    }

                    clean!()
                }
                TokenKind::Namespace => {
                    if external {
                        elle_error!(self.current_token().location.error("Cannot have an external namespace. Please remove the `external` keyword."))
                    }

                    let mut r#struct = Struct::new(self);
                    let res =
                        r#struct.parse(false, true, do_only == &DoOnly::Structs, location.clone());

                    if let Some((statement, mut builtins)) = res {
                        self.tree.borrow_mut().push(statement);
                        self.tree.borrow_mut().append(&mut builtins);

                        clean!()
                    }
                }
                _ => elle_error!(self.current_token().location.error(format!(
                    "Unexpected token found while parsing: {:?}",
                    self.current_token().kind
                ))),
            }
        }

        self.global_public = global_public;
        self.global_external = global_external;
        return (
            self.tree.borrow_mut().to_owned(),
            self.struct_pool.borrow_mut().to_owned(),
        );
    }
}
