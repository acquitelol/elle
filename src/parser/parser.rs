use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    compiler::qbe::r#type::Type,
    elle_error, ensure_fn_pointer,
    lexer::enums::{Location, MutRc, Token, TokenKind, ValueKind},
    misc::colors::*,
    parser::{constant::Constant, function::Function, r#enum::Enum, r#struct::Struct},
    Warnings, GENERIC_END, GENERIC_IDENTIFIER,
};

use super::{
    enums::{Argument, Primitive, StructSource, Variant},
    r#use::Use,
};

#[derive(Eq, PartialEq)]
pub enum DoOnly {
    FunctionsAndConstants,
    Imports,
    StructsAndEnums,
}

// Struct name -> (Generics, Fields, Def location)
pub type StructPool = HashMap<String, (Vec<String>, Vec<Argument>, MutRc<Location>)>;

// Enum name -> (Ordered variants (name, loc, offset value), Optional repr type)
pub type EnumPool = HashMap<String, (Vec<Variant>, Option<Type>)>;

pub fn create_generic_struct(
    name: &str,
    generic_name: &str,
    location: &MutRc<Location>,
    known_generics: &[Type],
    struct_pool: &RefCell<StructPool>,
    tree: &RefCell<Vec<Primitive>>,
) {
    let (generics, members, struct_location) = struct_pool.borrow().get(name).unwrap().clone();

    if generics.len() != known_generics.len() {
        if generics.len() < known_generics.len() {
            todo!("the user passed too many generics");
        }

        let unknown = generics
            .iter()
            .skip(known_generics.len())
            .cloned()
            .collect::<Vec<String>>();

        location.borrow_mut().above = Some(Rc::from(format!(
            "In struct:\n{GREEN}{BOLD}{}{}{RESET}\n\n",
            " ".repeat(location.borrow().ctx.len() - location.borrow().ctx.trim().len() + 8),
            struct_location.borrow().ctx,
            GREEN = get_GREEN!(),
            BOLD = get_BOLD!(),
            RESET = get_RESET!()
        )));

        elle_error!(
            location.borrow().error(format!(
                "Mismatched number of generics in struct {}<{}>.\nCould not find generic{} {} where the function specifies <{}>.",
                name.replace('.', "::"),
                generics.join(", "),
                if unknown.len() == 1 { "" } else { "s" },
                unknown.join(", "),
                generics.join(", ")
            ))
        )
    }

    let parsed_generics = generics
        .iter()
        .enumerate()
        .map(|(i, generic)| (generic.clone(), known_generics[i].clone()))
        .collect::<HashMap<_, _>>();

    let parsed_members = members
        .iter()
        .map(|member| Argument {
            name: member.name.clone(),
            r#type: member.r#type.clone().unknown_to_known(
                Some(struct_pool),
                Some(tree),
                &generics,
                &parsed_generics,
            ),
            no_fmt: member.no_fmt,
            is_unused: member.is_unused,
        })
        .collect::<Vec<Argument>>();

    tree.borrow_mut().push(Primitive::Struct(StructSource {
        name_token: Token::from_ident(generic_name),
        name: generic_name.into(),
        public: false,
        usable: true,
        imported: false,
        generics: vec![],
        known_generics: parsed_generics,
        members: parsed_members.clone(),
        keyword_location: location.clone(),
        location: location.clone(),
        ignore_empty: false,
    }));

    struct_pool.borrow_mut().insert(
        generic_name.into(),
        (vec![], parsed_members, location.clone()),
    );
}

#[macro_export]
macro_rules! get_type {
    ($self:expr, $generics:expr, $struct_pool:expr, $enum_pool:expr, $tree:expr) => {{
        let mut is_fn_pointer = false;
        let mut is_struct = false;
        let mut tuple_imported = true;
        let name;

        let location = $self.current_token().location;

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

            $self.expect_tokens(&[TokenKind::RightParenthesis]);
            $crate::set_end!(location, $self);

            if !tuple_imported {
                let import_text = "use std/collections/tuple;";

                elle_error!(
                    location.borrow().error(
                        format!(
                            "The tuple module is not imported. Please import it to use tuples.\n\n`{}`",
                            import_text
                        )
                    )
                );
            }

            if !$struct_pool.borrow().contains_key(&generic_name) {
                create_generic_struct(
                    "Tuple",
                    &generic_name,
                    &location,
                    &types,
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
                $self.get(&[TokenKind::Identifier])
            };

            is_struct = $struct_pool.borrow().contains_key(&name);
            let is_enum = $enum_pool.borrow().contains_key(&name);
            let is_base_type = ValueKind::String(name.clone()).is_base_type();
            let is_valid = is_fn_pointer
                || is_struct
                || is_enum
                || is_base_type
                || $generics.unwrap_or(&vec![]).contains(&name);

            if !is_valid {
                elle_error!(
                    $self.current_token().location.borrow().error(format!(
                        "Type, struct or enum named '{}' could not be found. Are you sure you spelt it correctly?",
                        name
                    ))
                )
            }

            if is_struct && $self.current_token().tagged {
                let pool = $struct_pool.borrow();
                let struct_def = pool.get(&name).unwrap();
                $crate::struct_hover!($self.current_token(), struct_def.1.is_empty(), struct_def.1);
            }

            if is_enum && $self.current_token().tagged {
                let pool = $enum_pool.borrow();
                let enum_def = pool.get(&name).unwrap();
                $crate::enum_hover!($self.current_token(), name, enum_def.0);
            }

            if is_base_type && $self.current_token().tagged {
                let ty = ValueKind::String(name.clone()).to_type_string(false, false, None).unwrap();
                elle_error!(format!(
                    "hover\n{}\n{}\ntype {}; // size = {}",
                    $self.current_token().location.borrow().display_plain(false),
                    $self.current_token().location.borrow().display_plain(true),
                    ty.display(),
                    ty.size_base()
                ));
            }

            ValueKind::String(name.clone())
                .to_type_string(is_struct, is_enum, $enum_pool.borrow().get(&name).map(|x| x.1.clone()).unwrap_or(None))
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
                            $crate::set_end!(location, $self);
                            let import_text = "use std/collections/array;";

                            elle_error!(
                                location.borrow().error(
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

                        $crate::set_end!(location, $self);

                        if !$struct_pool.borrow().contains_key(&generic_name) {
                            create_generic_struct(
                                "Array",
                                &generic_name,
                                &location,
                                &[ty],
                                &$struct_pool,
                                &$tree,
                            )
                        }

                        ty = Type::Pointer(Box::new(Type::Struct(generic_name)));
                        $self.expect_tokens(&[TokenKind::RightBlockBrace]);
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

                        $crate::set_end!(location, $self);

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
                                &name,
                                &generic_name,
                                &location,
                                &known_generics,
                                &$struct_pool,
                                &$tree,
                            )
                        }

                        ty = Type::Struct(generic_name);
                        $self.expect_tokens(&[TokenKind::GreaterThan]);
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
    pub enum_pool: RefCell<EnumPool>,
    pub global_public: bool,
    pub global_external: bool,
    pub warnings: Warnings,
}

impl Parser {
    pub const fn new(
        tokens: Vec<Token>,
        struct_pool: StructPool,
        enum_pool: EnumPool,
        warnings: Warnings,
    ) -> Self {
        Self {
            tokens,
            position: 0,
            tree: RefCell::new(vec![]),
            struct_pool: RefCell::new(struct_pool),
            enum_pool: RefCell::new(enum_pool),
            global_public: false,
            global_external: false,
            warnings,
        }
    }

    pub fn current_token(&self) -> Token {
        self.tokens[self.position].clone()
    }

    fn next_token(&self) -> Option<Token> {
        if self.is_eof() {
            None
        } else {
            Some(self.tokens[self.position + 1].clone())
        }
    }

    pub const fn advance(&mut self) {
        if !self.is_eof() {
            self.position += 1;
        }
    }

    pub const fn is_eof(&self) -> bool {
        self.position >= self.tokens.len() - 1
    }

    pub fn match_token(&mut self, expected: TokenKind, advance: bool) -> bool {
        if self.current_token().kind == expected {
            if advance {
                self.advance();
            }

            true
        } else {
            false
        }
    }

    pub fn expect_tokens(&self, expected: &[TokenKind]) {
        if !expected.contains(&self.current_token().kind) {
            elle_error!(self.current_token().location.borrow().error(format!(
                "Expected one of [{}], got {:?}.",
                expected
                    .iter()
                    .map(|kind| format!("{kind:?}"))
                    .collect::<Vec<String>>()
                    .join(", "),
                self.current_token().kind
            )))
        }
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

        let Token {
            value: ValueKind::String(identifier),
            ..
        } = self.current_token()
        else {
            elle_error!(token.location.borrow().error(format!(
                "Expected one of {:?} for function name, got {:?}",
                expected,
                self.current_token()
            )));
        };

        identifier
    }

    pub fn get_identifier(&self) -> String {
        self.get(&[TokenKind::Identifier, TokenKind::ExactLiteral])
    }

    pub fn get_type(&mut self, generics: Option<&Vec<String>>) -> Type {
        get_type!(self, generics, self.struct_pool, self.enum_pool, self.tree)
    }

    pub fn yield_tokens_wrapped_with_semi(&mut self) -> Vec<Token> {
        let mut paren_nesting = 0;
        let mut block_nesting = 0;
        let mut curly_nesting = 0;
        let mut tokens = vec![];

        while !self.is_eof() {
            if self.current_token().kind == TokenKind::LeftParenthesis {
                paren_nesting += 1;
            }

            if self.current_token().kind == TokenKind::LeftBlockBrace {
                block_nesting += 1;
            }

            if self.current_token().kind == TokenKind::LeftCurlyBrace {
                curly_nesting += 1;
            }

            tokens.push(self.current_token());
            self.advance();

            if self.current_token().kind == TokenKind::RightParenthesis && paren_nesting > 0 {
                paren_nesting -= 1;
            }

            if self.current_token().kind == TokenKind::RightBlockBrace && block_nesting > 0 {
                block_nesting -= 1;
            }

            if self.current_token().kind == TokenKind::RightCurlyBrace && curly_nesting > 0 {
                curly_nesting -= 1;
            }

            if self.current_token().kind == TokenKind::Semicolon
                && paren_nesting == 0
                && block_nesting == 0
                && curly_nesting == 0
            {
                break;
            }
        }

        tokens
    }

    // 0 - functions, constants, etc
    // 1 - non-generic imports and generic declarations only
    // 2 - generic imports only
    // 3 - structs only
    pub fn parse(
        &mut self,
        do_only: &DoOnly,
        new_struct_pool: Option<StructPool>,
        new_enum_pool: Option<EnumPool>,
    ) -> (Vec<Primitive>, StructPool, EnumPool) {
        if let Some(pool) = new_struct_pool {
            self.struct_pool = RefCell::new(pool);
        }

        if let Some(pool) = new_enum_pool {
            self.enum_pool = RefCell::new(pool);
        }

        self.position = 0;
        let mut location = self.current_token().location;
        let mut public = false;
        let mut local = false;
        let mut defined = false;
        let mut external = false;

        macro_rules! clean {
            () => {{
                location = self.current_token().location;
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
                                _ => elle_error!(self.current_token().location.borrow().error(
                                    format!(
                                        "Invalid global identifier named '{}'",
                                        self.current_token()
                                            .value
                                            .get_string_inner()
                                            .unwrap_or(self.current_token().kind.to_string())
                                    )
                                )),
                            }

                            self.advance();
                        }};
                    }

                    match_one!(); // Must have one identifier

                    // Match until no more commas
                    while self.current_token().kind == TokenKind::Comma && !self.is_eof() {
                        self.advance();
                        match_one!();
                    }

                    self.expect_tokens(&[TokenKind::Semicolon]);
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
                        _ => elle_error!(self.current_token().location.borrow().error(format!(
                            "Invalid local specifier named '{}'",
                            self.current_token()
                                .value
                                .get_string_inner()
                                .unwrap_or_else(|| self.current_token().kind.to_string())
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

                    clean!();
                }
                TokenKind::Function => {
                    if local && public {
                        elle_error!(self
                            .current_token()
                            .location
                            .borrow()
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
                    if let Some((statement, should_compile)) = statement
                        && should_compile
                    {
                        self.tree.borrow_mut().push(statement);
                    }

                    clean!();
                }
                TokenKind::Constant => {
                    if external {
                        elle_error!(self.current_token().location.borrow().error("Cannot have an external constant. Please remove the `external` keyword."))
                    }

                    if local && public {
                        elle_error!(self
                            .current_token()
                            .location
                            .borrow()
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

                    clean!();
                }
                TokenKind::Struct => {
                    if external {
                        elle_error!(self.current_token().location.borrow().error(
                            "Cannot have an external struct. Please remove the `external` keyword."
                        ))
                    }

                    if local && public {
                        elle_error!(self
                            .current_token()
                            .location
                            .borrow()
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
                        do_only == &DoOnly::StructsAndEnums,
                        location.clone(),
                    );

                    if let Some((statement, mut builtins, should_compile)) = res
                        && should_compile
                    {
                        self.tree.borrow_mut().push(statement);
                        self.tree.borrow_mut().append(&mut builtins);
                    }

                    clean!();
                }
                TokenKind::Namespace => {
                    if external {
                        elle_error!(self.current_token().location.borrow().error("Cannot have an external namespace. Please remove the `external` keyword."))
                    }

                    let mut r#struct = Struct::new(self);
                    let res = r#struct.parse(
                        false,
                        true,
                        do_only == &DoOnly::StructsAndEnums,
                        location.clone(),
                    );

                    if let Some((statement, mut builtins, should_compile)) = res
                        && should_compile
                    {
                        self.tree.borrow_mut().push(statement);
                        self.tree.borrow_mut().append(&mut builtins);
                        clean!();
                    }
                }
                TokenKind::Enum => {
                    if external {
                        elle_error!(self.current_token().location.borrow().error("Cannot have an external enumeration. Please remove the `external` keyword."))
                    }

                    let mut r#enum = Enum::new(self);
                    let res = r#enum.parse(
                        if local {
                            false
                        } else {
                            global_public || public
                        },
                        do_only == &DoOnly::StructsAndEnums,
                        location.clone(),
                    );

                    if let Some((statement, mut builtins, should_compile)) = res
                        && should_compile
                    {
                        self.tree.borrow_mut().push(statement);
                        self.tree.borrow_mut().append(&mut builtins);
                        clean!();
                    }
                }
                _ => elle_error!(self.current_token().location.borrow().error(format!(
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
            self.enum_pool.borrow_mut().to_owned(),
        );
    }
}
