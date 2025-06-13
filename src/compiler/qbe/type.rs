use core::fmt;
use std::{cell::RefCell, collections::HashMap, iter::Peekable, mem, num::ParseIntError};

use crate::{
    elle_error, get_POINTER_ID, has_unknown_part, hashmap, is_generic,
    lexer::enums::{Location, MutRc, Token},
    misc::{
        colors::{get_GREEN, get_RED, get_RESET, GREEN, RED, RESET},
        constants::{DISPLAY_NESTING_MAX, GENERIC_FUNCTION},
    },
    parser::{
        enums::{Argument, Primitive, StructSource},
        parser::StructPool,
    },
    GENERIC_END, GENERIC_ENUM, GENERIC_IDENTIFIER, GENERIC_POINTER, GENERIC_UNKNOWN, POINTER_ID,
    VOID_POINTER_ID,
};

use super::{function::Function, module::Module};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    UnsignedByte,
    UnsignedHalfword,
    UnsignedWord,
    UnsignedLong,
    Byte,
    Halfword,
    Boolean,
    Word,
    Long,
    Single,
    Double,
    Char,
    Void,
    Null,
    Infer,
    // Inner type
    Pointer(Box<Type>),
    Struct(String),
    Enum(String, Box<Option<Type>>),
    // Unknown generic
    Unknown(String),
    Function(Box<Option<Function>>),
}

impl Type {
    pub fn display_nested(&self, nesting: usize) -> String {
        match self {
            Self::Struct(td) => {
                if is_generic!(td) {
                    let (name, parts) = Self::from_internal_id(td);

                    let mapped = if nesting >= DISPLAY_NESTING_MAX {
                        "...".into()
                    } else {
                        parts
                            .iter()
                            .map(|x| x.display_nested(nesting + 1))
                            .collect::<Vec<_>>()
                            .join(", ")
                    };

                    match name.as_str() {
                        "Array" => format!("{mapped}[]"),
                        "Tuple" | "Triple" => format!("({mapped})"),
                        _ => format!("{name}<{mapped}>"),
                    }
                } else {
                    td.into()
                }
            }
            _ => self.display(),
        }
    }

    pub fn display(&self) -> String {
        match self {
            Self::Byte => "i8".into(),
            Self::UnsignedByte => "u8".into(),
            Self::Char => "char".into(),
            Self::Halfword => "i16".into(),
            Self::UnsignedHalfword => "u16".into(),
            Self::Boolean => "bool".into(),
            Self::Word => "i32".into(),
            Self::UnsignedWord => "u32".into(),
            Self::Long => "i64".into(),
            Self::UnsignedLong => "u64".into(),
            Self::Pointer(inner) => {
                if *inner.as_ref() == Self::Char {
                    "string".into()
                // TODO: this is bad. arrays should just be normal structs
                } else if inner.is_struct() {
                    let td = inner.get_struct_inner().unwrap();

                    if is_generic!(td) {
                        let (name, _) = Self::from_internal_id(&td);

                        match name.as_str() {
                            "Array" => inner.display(),
                            _ => format!("{}*", inner.display()),
                        }
                    } else {
                        format!("{}*", inner.display())
                    }
                } else {
                    format!("{}*", inner.display())
                }
            }
            Self::Single => "f32".into(),
            Self::Double => "f64".into(),
            Self::Void => "void".into(),
            Self::Null => "null".into(),
            Self::Enum(name, ty) => Option::as_ref(ty)
                .map_or_else(|| name.into(), |ty| format!("{name}({})", ty.display())),
            Self::Struct(..) => self.display_nested(0),
            Self::Function(inner) => {
                if let Some(inner) = *inner.to_owned() {
                    format!(
                        "fn{}{}({}{}){}",
                        if inner.lambda { "" } else { " " },
                        if inner.lambda {
                            "".into()
                        } else {
                            let namespaced = inner
                                .name
                                .split('.')
                                .nth(1)
                                .is_some_and(|x| x != GENERIC_IDENTIFIER);

                            if is_generic!(inner.name) {
                                let generic_name = if namespaced {
                                    inner.name.replacen('.', "::", 1)
                                } else {
                                    inner.name.clone()
                                };

                                let (name, parts) = Self::from_internal_id(&generic_name);

                                format!(
                                    "{}<{}>",
                                    name,
                                    if has_unknown_part!(inner.name) {
                                        parts
                                            .iter()
                                            .map(Self::display)
                                            .collect::<Vec<_>>()
                                            .join(", ")
                                    } else {
                                        "...".into()
                                    }
                                )
                            } else {
                                inner.name.replace('.', "::")
                            }
                        },
                        inner
                            .arguments
                            .iter()
                            .map(|arg| format!(
                                "{}{}",
                                arg.0 .0.display(),
                                if inner.lambda {
                                    "".into()
                                } else {
                                    format!(
                                        " {}",
                                        arg.0
                                             .1
                                            .get_string_inner()
                                            .replace('%', "")
                                            .split('.')
                                            .nth(0)
                                            .unwrap()
                                    )
                                }
                            ))
                            .collect::<Vec<_>>()
                            .join(", "),
                        if inner.variadic { ", ..." } else { "" },
                        inner
                            .return_type
                            .map_or_else(String::new, |ty| format!(" -> {}", ty.display()))
                    )
                } else {
                    "<unknown function>".into()
                }
            }
            Self::Unknown(name) => name.into(),
            Self::Infer => unreachable!(),
        }
    }

    pub fn id(&self) -> String {
        match self {
            Self::Char => "char".into(),
            Self::Boolean => "bool".into(),
            Self::Byte => "i8".into(),
            Self::Halfword => "i16".into(),
            Self::Word => "i32".into(),
            Self::Long => "i64".into(),
            Self::UnsignedByte => "u8".into(),
            Self::UnsignedHalfword => "u16".into(),
            Self::UnsignedWord => "u32".into(),
            Self::UnsignedLong => "u64".into(),
            Self::Single => "f32".into(),
            Self::Double => "f64".into(),
            Self::Void => "void".into(),
            Self::Null => "null".into(),
            Self::Enum(name, ..) | Self::Unknown(name) => name.clone(),
            Self::Pointer(..) | Self::Struct(..) | Self::Function(..) => self.display(),
            Self::Infer => unreachable!(),
        }
    }

    pub fn strict_id(&self) -> String {
        match self {
            x if x.is_string() => "string".into(),
            x if x.is_void_pointer() => VOID_POINTER_ID.into(),
            Self::Pointer(_) => get_POINTER_ID!().into(),
            _ => self.id(),
        }
    }

    pub fn to_internal_id(&self) -> String {
        const fn ty_to_num(ty: &Type) -> u8 {
            match ty {
                Type::UnsignedByte => 4,
                Type::UnsignedHalfword => 5,
                Type::UnsignedWord => 6,
                Type::UnsignedLong => 7,
                Type::Byte => 8,
                Type::Halfword => 9,
                Type::Boolean => 10,
                Type::Word => 11,
                Type::Long => 12,
                Type::Single => 13,
                Type::Double => 14,
                Type::Char => 15,
                Type::Void => 16,
                Type::Null => 17,
                _ => 100, // Invalid
            }
        }

        let num: u8 = ty_to_num(self);

        match self {
            Self::Pointer(inner) => format!("{GENERIC_POINTER}.{}", inner.to_internal_id()),
            Self::Enum(name, inner) => format!(
                "{GENERIC_ENUM}.{}.{}",
                name,
                Option::as_ref(inner)
                    .unwrap_or(&Self::Word)
                    .to_internal_id()
            ),
            Self::Function(inner) if let Some(inner) = (**inner).clone() => {
                format!(
                    "{GENERIC_FUNCTION}.{GENERIC_IDENTIFIER}.{}.{}.{GENERIC_END}",
                    inner
                        .arguments
                        .iter()
                        .map(|((ty, _), _)| ty.to_internal_id())
                        .collect::<Vec<_>>()
                        .join("."),
                    inner.return_type.unwrap_or(Type::Void).to_internal_id()
                )
            }
            Self::Struct(name) => name.clone(),
            Self::Unknown(name) => format!("{GENERIC_UNKNOWN}.{name}"),
            _ => num.to_string(),
        }
    }

    // Foo.0.8.10.Bar.ptr.ptr.7.1 turns into
    // ("Foo", vec![Word, Single, Struct("Bar"), Pointer(Pointer(Boolean))])
    pub fn from_internal_id(id: &str) -> (String, Vec<Self>) {
        fn is_num_id(id: &str) -> Result<u8, ParseIntError> {
            if [
                GENERIC_IDENTIFIER,
                GENERIC_END,
                GENERIC_POINTER,
                GENERIC_UNKNOWN,
                GENERIC_ENUM,
                GENERIC_FUNCTION,
            ]
            .contains(&id)
            {
                "-1".parse::<u8>() // Throw an artificial error
            } else {
                id.parse::<u8>()
            }
        }

        fn id_to_ty(id: &str) -> Type {
            id.parse::<u8>().map_or_else(
                |_| Type::Struct(id.to_string()),
                |inner| match inner {
                    4 => Type::UnsignedByte,
                    5 => Type::UnsignedHalfword,
                    6 => Type::UnsignedWord,
                    7 => Type::UnsignedLong,
                    8 => Type::Byte,
                    9 => Type::Halfword,
                    10 => Type::Boolean,
                    11 => Type::Word,
                    12 => Type::Long,
                    13 => Type::Single,
                    14 => Type::Double,
                    15 => Type::Char,
                    16 => Type::Void,
                    17 => Type::Null,
                    _ => todo!("{id}"),
                },
            )
        }

        fn internal_match<'a, T>(parts: &mut Peekable<T>) -> Option<Type>
        where
            T: Iterator<Item = &'a str>,
        {
            parts.peek()?;

            let mut part = parts.next().unwrap();
            match is_num_id(part) {
                Ok(_) => Some(id_to_ty(part)),
                Err(_) => {
                    if part == GENERIC_POINTER {
                        let mut nesting = 0;

                        while part == GENERIC_POINTER {
                            nesting += 1;

                            if *parts.peek()? != GENERIC_POINTER {
                                break;
                            }

                            part = parts.next().unwrap();
                        }

                        internal_match(parts).map(|mut res| {
                            for _ in 0..nesting {
                                res = Type::Pointer(Box::new(res));
                            }

                            res
                        })
                    } else if part == GENERIC_ENUM {
                        let name = parts.next().unwrap();
                        let ty = internal_match(parts).unwrap();
                        Some(Type::Enum(name.to_string(), Box::new(Some(ty))))
                    } else if part == GENERIC_FUNCTION {
                        assert_eq!(parts.next().unwrap(), GENERIC_IDENTIFIER);
                        let mut res = vec![];
                        let mut nesting = 0;

                        loop {
                            if parts.peek().is_some_and(|part| *part == GENERIC_IDENTIFIER) {
                                nesting += 1;
                            }

                            if parts.peek().is_some_and(|part| *part == GENERIC_END) {
                                if nesting > 0 {
                                    nesting -= 1;
                                } else {
                                    parts.next();
                                    break;
                                }
                            }

                            res.push(internal_match(parts).unwrap());
                        }

                        let return_type = res.pop();

                        Some(Type::Function(Box::new(Some(
                            crate::compiler::qbe::function::Function {
                                variadic: false,
                                external: true,
                                builtin: false,
                                volatile: false,
                                format: false,
                                lambda: true,
                                usable: true,
                                imported: true,
                                arguments: res
                                    .into_iter()
                                    .enumerate()
                                    .map(|(i, ty)| {
                                        (
                                            (
                                                ty,
                                                crate::compiler::qbe::value::Value::Temporary(
                                                    format!("_{i}"),
                                                ),
                                            ),
                                            false,
                                        )
                                    })
                                    .collect::<Vec<_>>(),
                                return_type,
                                ..Default::default()
                            },
                        ))))
                    } else if part == GENERIC_UNKNOWN {
                        Some(Type::Unknown(parts.next().unwrap().to_string()))
                    } else if part == GENERIC_END {
                        internal_match(parts)
                    } else {
                        Some(Type::Struct(
                            if parts.peek().is_some_and(|part| *part == GENERIC_IDENTIFIER) {
                                let mut res = vec![];
                                res.push(parts.next().unwrap());
                                let mut nesting = 0;

                                loop {
                                    if parts.peek().is_some_and(|part| *part == GENERIC_IDENTIFIER)
                                    {
                                        nesting += 1;
                                    }

                                    if parts.peek().is_some_and(|part| *part == GENERIC_END) {
                                        if nesting > 0 {
                                            nesting -= 1;
                                        } else {
                                            parts.next();
                                            break;
                                        }
                                    }

                                    res.push(parts.next().unwrap());
                                }

                                format!("{part}.{}.{GENERIC_END}", res.join("."))
                            } else {
                                part.to_string()
                            },
                        ))
                    }
                }
            }
        }

        let mut parts = id.split('.').collect::<Vec<_>>();

        let name = parts.remove(0);
        assert_eq!(parts.remove(0), GENERIC_IDENTIFIER.to_string());

        let mut res = vec![];
        let mut iter = parts.iter().copied().peekable();

        while iter.peek().is_some() {
            if let Some(x) = internal_match(&mut iter) {
                res.push(x);
            } else {
                break;
            }
        }

        (name.to_string(), res)
    }

    pub fn unknown_to_known(
        &self,
        struct_pool: Option<&RefCell<StructPool>>,
        tree: Option<&RefCell<Vec<Primitive>>>,
        generics: &[String],
        known_generics: &HashMap<String, Self>,
    ) -> Self {
        match self {
            Self::Pointer(inner) => Self::Pointer(Box::new(inner.unknown_to_known(
                struct_pool,
                tree,
                generics,
                known_generics,
            ))),
            Self::Unknown(name) => {
                if generics.contains(name) && known_generics.contains_key(name) {
                    known_generics.get(name).unwrap().to_owned()
                } else {
                    self.clone()
                }
            }
            Self::Function(inner) if let Some(inner) = (**inner).clone() => {
                let parsed_arguments = inner
                    .arguments
                    .into_iter()
                    .map(|((ty, val), no_fmt)| {
                        (
                            (
                                ty.unknown_to_known(struct_pool, tree, generics, known_generics),
                                val,
                            ),
                            no_fmt,
                        )
                    })
                    .collect::<Vec<_>>();

                let parsed_return_ty = inner
                    .return_type
                    .map(|ty| ty.unknown_to_known(struct_pool, tree, generics, known_generics));

                Self::Function(Box::new(Some(Function {
                    arguments: parsed_arguments,
                    return_type: parsed_return_ty,
                    ..inner
                })))
            }
            // Self::Struct(name) if is_unknown!(name) => {
            //     let (original_name, parts) = Self::from_internal_id(name);

            //     dbg!(&name, &parts, &known_generics, &generics);

            //     let generic_name = format!(
            //         "{original_name}.{GENERIC_IDENTIFIER}.{}.{GENERIC_END}",
            //         parts
            //             .iter()
            //             .filter_map(Self::get_unknown_inner)
            //             .map(|generic| known_generics.get(&generic).unwrap().to_internal_id())
            //             .collect::<Vec<String>>()
            //             .join(".")
            //     );

            //     if struct_pool.is_some()
            //         && tree.is_some()
            //         && !struct_pool.unwrap().borrow().contains_key(&generic_name)
            //     {
            //         let (generics, members, location) = struct_pool
            //             .unwrap()
            //             .borrow()
            //             .get(&original_name)
            //             .unwrap()
            //             .clone();

            //         let parsed_generics = generics
            //             .iter()
            //             .enumerate()
            //             .filter_map(|(i, _)| parts[i].clone().get_unknown_inner())
            //             .enumerate()
            //             .map(|(i, generic)| {
            //                 (
            //                     generic.clone(),
            //                     known_generics[&parts[i].clone().get_unknown_inner().unwrap()]
            //                         .clone(),
            //                 )
            //             })
            //             .collect::<HashMap<_, _>>();

            //         let parsed_members = members
            //             .iter()
            //             .map(|member| Argument {
            //                 name: member.name.clone(),
            //                 r#type: member.r#type.clone().unknown_to_known(
            //                     struct_pool,
            //                     tree,
            //                     &generics,
            //                     &parsed_generics,
            //                 ),
            //                 no_fmt: member.no_fmt,
            //                 is_unused: member.is_unused,
            //             })
            //             .collect::<Vec<_>>();

            //         tree.unwrap()
            //             .borrow_mut()
            //             .push(Primitive::Struct(StructSource {
            //                 name_token: Token::from_ident(&generic_name),
            //                 name: generic_name.clone(),
            //                 public: false,
            //                 usable: true,
            //                 imported: false,
            //                 generics: vec![],
            //                 known_generics: parsed_generics,
            //                 members: parsed_members.clone(),
            //                 keyword_location: location.clone(),
            //                 location: location.clone(),
            //                 ignore_empty: false,
            //             }));

            //         struct_pool
            //             .unwrap()
            //             .borrow_mut()
            //             .insert(generic_name.clone(), (vec![], parsed_members, location));
            //     }

            //     Self::Struct(generic_name)
            // }
            Self::Struct(name) if is_generic!(name) && has_unknown_part!(name) => {
                let (original_name, mut parts) = Self::from_internal_id(name);

                for (i, x) in parts.clone().iter().enumerate() {
                    parts[i] = x.unknown_to_known(struct_pool, tree, generics, known_generics);
                }

                let generic_name = format!(
                    "{original_name}.{GENERIC_IDENTIFIER}.{}.{GENERIC_END}",
                    parts
                        .iter()
                        .map(Self::to_internal_id)
                        .collect::<Vec<String>>()
                        .join(".")
                );

                if struct_pool.is_some()
                    && tree.is_some()
                    && !struct_pool.unwrap().borrow().contains_key(&generic_name)
                {
                    let (generics, members, location) = struct_pool
                        .unwrap()
                        .borrow()
                        .get(&original_name)
                        .unwrap()
                        .clone();

                    let parsed_generics = generics
                        .iter()
                        .enumerate()
                        .map(|(i, generic)| (generic.clone(), parts[i].clone()))
                        .collect::<HashMap<_, _>>();

                    let parsed_members = members
                        .iter()
                        .map(|member| Argument {
                            name: member.name.clone(),
                            r#type: member.r#type.clone().unknown_to_known(
                                struct_pool,
                                tree,
                                &generics,
                                &parsed_generics,
                            ),
                            no_fmt: member.no_fmt,
                            is_unused: member.is_unused,
                        })
                        .collect::<Vec<_>>();

                    tree.unwrap()
                        .borrow_mut()
                        .push(Primitive::Struct(StructSource {
                            name_token: Token::from_ident(&generic_name),
                            name: generic_name.clone(),
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

                    struct_pool
                        .unwrap()
                        .borrow_mut()
                        .insert(generic_name.clone(), (vec![], parsed_members, location));
                }

                Self::Struct(generic_name)
            }
            other => other.clone(),
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    pub fn has_generic_type(&self) -> bool {
        match self {
            Self::Pointer(inner) => inner.has_generic_type(),
            Self::Unknown(_) => true,
            Self::Function(f) => {
                if let Some(f) = (**f).clone() {
                    f.arguments.iter().any(|x| x.0 .0.has_generic_type())
                        || f.return_type.is_some_and(|x| x.has_generic_type())
                } else {
                    false
                }
            }
            Self::Struct(name) => has_unknown_part!(name),
            _ => false,
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    pub fn deduce_generic_type(
        &self,
        generic_type: &Self,
        location: &MutRc<Location>,
    ) -> Option<HashMap<String, Self>> {
        match (self, generic_type) {
            (Self::Pointer(known_inner), Self::Pointer(generic_inner)) => {
                known_inner.deduce_generic_type(generic_inner, location)
            }
            (Self::Function(known_inner), Self::Function(generic_inner)) => {
                let mut map = hashmap![];

                if let Some(known_inner) = (**known_inner).clone()
                    && let Some(generic_inner) = (**generic_inner).clone()
                {
                    for i in 0..if known_inner.arguments.len() < generic_inner.arguments.len() {
                        known_inner.arguments.len()
                    } else {
                        generic_inner.arguments.len()
                    } {
                        if let Some(new_map) = known_inner.arguments[i]
                            .0
                             .0
                            .deduce_generic_type(&generic_inner.arguments[i].0 .0, location)
                        {
                            map.extend(
                                new_map
                                    .into_iter()
                                    .filter(|x| !x.1.is_unknown())
                                    .collect::<HashMap<_, _>>(),
                            );
                        }
                    }

                    let known_return_ty = known_inner.return_type.unwrap_or(Type::Void);
                    let generic_return_ty = generic_inner.return_type.unwrap_or(Type::Void);
                    if let Some(new_map) =
                        known_return_ty.deduce_generic_type(&generic_return_ty, location)
                    {
                        map.extend(
                            new_map
                                .into_iter()
                                .filter(|x| !x.1.is_unknown())
                                .collect::<HashMap<_, _>>(),
                        );
                    }
                } else {
                    return None;
                }

                Some(map)
            }
            (known, Self::Pointer(other)) if known.is_struct() && other.is_struct() => {
                known.deduce_generic_type(other, location)
            }
            (Self::Pointer(known), other) if known.is_struct() && other.is_struct() => {
                known.deduce_generic_type(other, location)
            }
            (known, Self::Unknown(name)) => Some(hashmap![name.clone() => known.clone()]),
            // Struct<(known)> vs Struct<T>
            (Self::Struct(specialized_name), Self::Struct(name))
                if is_generic!(specialized_name) && is_generic!(name) =>
            {
                let (original_name, known_parts) = Self::from_internal_id(specialized_name);
                let (struct_name, unknown_parts) = Self::from_internal_id(name);

                if original_name != struct_name {
                    elle_error!(location.borrow().error(
                        format!(
                            "Mismatched types when trying to deduce a generic:\nExpected '{GREEN}{}{RESET}' but got '{RED}{}{RESET}' instead",
                            generic_type.display(),
                            self.display(),
                            GREEN = get_GREEN!(),
                            RED = get_RED!(),
                            RESET = get_RESET!()
                        )
                    ));
                }

                let mut res = hashmap![];

                for (i, v) in unknown_parts.iter().enumerate() {
                    if v.has_generic_type()
                        && let Some(new) = known_parts[i].deduce_generic_type(v, location)
                    {
                        res.extend(new);
                    }
                }

                Some(res)
            }
            _ => None,
        }
    }

    pub fn get_pointer_inner(&self) -> Option<Self> {
        match self {
            Self::Pointer(ty) => Some(*ty.clone()),
            _ => None,
        }
    }

    pub fn get_struct_inner(&self) -> Option<String> {
        match self {
            Self::Struct(val, ..) => Some(val.clone()),
            _ => None,
        }
    }

    pub fn get_enum_inner(&self) -> Option<String> {
        match self {
            Self::Enum(val, ..) => Some(val.clone()),
            _ => None,
        }
    }

    pub fn get_enum_repr(&self) -> Option<Self> {
        match self {
            Self::Enum(_, ty) => Some(ty.clone().unwrap_or(Self::Word)),
            _ => None,
        }
    }

    pub fn get_unknown_inner(&self) -> Option<String> {
        match self {
            Self::Unknown(val) => Some(val.clone()),
            _ => None,
        }
    }

    pub fn get_function_inner(&self) -> Option<Function> {
        match self {
            Self::Function(val) => Some(val.clone().unwrap()),
            _ => None,
        }
    }

    pub fn into_abi(self) -> Self {
        match self {
            Self::Boolean
            | Self::Byte
            | Self::Char
            | Self::UnsignedByte
            | Self::Halfword
            | Self::UnsignedHalfword
            | Self::UnsignedWord => Self::Word,
            Self::UnsignedLong => Self::Long,
            Self::Enum(_, inner) => inner.map(Self::into_abi).unwrap_or(Self::Word),
            other => other,
        }
    }

    pub fn into_base(self) -> Self {
        match self {
            Self::Boolean
            | Self::Byte
            | Self::Char
            | Self::UnsignedByte
            | Self::Halfword
            | Self::UnsignedHalfword
            | Self::UnsignedWord => Self::Word,
            Self::UnsignedLong | Self::Struct(..) => Self::Long,
            Self::Enum(_, inner) => inner.map(Self::into_base).unwrap_or(Self::Word),
            other => other,
        }
    }

    pub const fn is_float(&self) -> bool {
        matches!(self, Self::Single | Self::Double)
    }

    pub const fn is_void(&self) -> bool {
        matches!(self, Self::Void)
    }

    pub const fn is_infer(&self) -> bool {
        matches!(self, Self::Infer)
    }

    pub const fn is_int(&self) -> bool {
        !self.is_float()
    }

    pub const fn is_bool(&self) -> bool {
        matches!(self, Self::Boolean)
    }

    pub fn is_strictly_number(&self) -> bool {
        !self.is_string()
            && !self.is_void()
            && !self.is_void_pointer()
            && !self.is_struct()
            && !self.is_function()
            && !self.is_enum()
    }

    pub const fn is_struct(&self) -> bool {
        matches!(self, Self::Struct(..))
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Self::Pointer(inner) if *inner.as_ref() == Self::Char)
    }

    pub fn is_void_pointer(&self) -> bool {
        matches!(self, Self::Pointer(inner) if *inner.as_ref() == Self::Void)
    }

    pub const fn is_unknown(&self) -> bool {
        matches!(self, Self::Unknown(_))
    }

    pub fn is_function(&self) -> bool {
        match self {
            Self::Function(inner) => inner.is_some(),
            _ => false,
        }
    }

    pub fn contextual_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Pointer(lhs), Self::Pointer(rhs)) => {
                lhs == rhs || lhs.is_void() || rhs.is_void()
            }
            (Self::Enum(lhs, _), Self::Enum(rhs, _)) => lhs == rhs,
            (x, y) => x == y,
        }
    }

    pub fn function_eq(&self, other: &Self, location: Option<&MutRc<Location>>) -> bool {
        match (self, other) {
            (Self::Function(a_inner), Self::Function(b_inner))
                if let Some(lhs) = &**a_inner
                    && let Some(rhs) = &**b_inner =>
            {
                if lhs.arguments.len() > rhs.arguments.len() {
                    if let Some(location) = location {
                        elle_error!(location
                        .borrow()
                        .with_extra_info("This callback takes too many arguments")
                        .error(format!("Too many arguments were expected in this callback.\nThe function expects {GREEN}{}{RESET}, but this callback takes {RED}{}{RESET}.",
                            rhs.arguments.len(),
                            lhs.arguments.len(),
                            RESET = get_RESET!(),
                            GREEN = get_GREEN!(),
                            RED = get_RED!(),
                        )))
                    } else {
                        return false;
                    }
                }

                for i in 0..lhs.arguments.len() {
                    let ((lty, _), _) = &lhs.arguments[i];
                    let ((rty, _), _) = &rhs.arguments[i];

                    if !lty.function_eq(rty, location) {
                        return false;
                    }
                }

                let lreturn_ty = lhs.return_type.as_ref().unwrap_or(&Type::Void);
                let rreturn_ty = rhs.return_type.as_ref().unwrap_or(&Type::Void);
                lreturn_ty.function_eq(rreturn_ty, location) || lreturn_ty.contextual_eq(rreturn_ty)
            }
            (x, y) => x.contextual_eq(y),
        }
    }

    pub const fn is_pointer(&self) -> bool {
        matches!(self, Self::Pointer(_))
    }

    pub const fn is_enum(&self) -> bool {
        matches!(self, Self::Enum(_, _))
    }

    pub const fn is_pointer_like(&self) -> bool {
        matches!(self, Self::Pointer(_) | Self::Long)
    }

    pub fn is_primitive(&self) -> bool {
        match self {
            Self::Pointer(x) if matches!(**x, Self::Struct(_)) => false,
            Self::Pointer(x) if matches!(**x, Self::Char) => false,
            Self::Struct(_) => false,
            _ => true,
        }
    }

    pub fn is_smaller_than_int(&self) -> bool {
        match self {
            Self::Boolean
            | Self::Byte
            | Self::UnsignedByte
            | Self::Halfword
            | Self::UnsignedHalfword
            | Self::Char => true,
            Self::Enum(_, inner) => Option::as_ref(inner).is_some_and(Self::is_smaller_than_int),
            _ => false,
        }
    }

    pub fn is_map_to_int(&self) -> bool {
        match self {
            Self::Byte
            | Self::UnsignedByte
            | Self::Halfword
            | Self::UnsignedHalfword
            | Self::Boolean
            | Self::Char
            | Self::Word
            | Self::UnsignedWord => true,
            Self::Enum(_, inner) => Option::as_ref(inner).is_some_and(Self::is_map_to_int),
            _ => false,
        }
    }

    pub fn is_unsigned(&self) -> bool {
        match self {
            Self::UnsignedByte
            | Self::UnsignedHalfword
            | Self::UnsignedWord
            | Self::UnsignedLong => true,
            Self::Enum(_, inner) => Option::as_ref(inner).is_some_and(Self::is_unsigned),
            _ => false,
        }
    }

    pub fn into_signed(self) -> Self {
        match self {
            Self::UnsignedByte => Self::Byte,
            Self::UnsignedHalfword => Self::Halfword,
            Self::UnsignedWord => Self::Word,
            Self::UnsignedLong => Self::Long,
            Self::Enum(_, inner) => inner.map(Self::into_signed).unwrap_or(Self::Word),
            other => other,
        }
    }

    pub fn weight(&self) -> u8 {
        match self {
            Self::Struct(_) => 7,
            Self::Double => 6,
            Self::Single => 5,
            Self::Long | Self::UnsignedLong | Self::Pointer(..) | Self::Function(..) => 4,
            Self::Word | Self::UnsignedWord => 3,
            Self::Halfword | Self::UnsignedHalfword => 2,
            Self::Boolean | Self::Byte | Self::UnsignedByte | Self::Char => 1,
            Self::Enum(_, inner) => Option::as_ref(inner).unwrap_or(&Self::Word).weight(),
            Self::Void | Self::Null | Self::Infer | Self::Unknown(_) => 0,
        }
    }

    pub fn size_base(&self) -> u64 {
        match self {
            Self::Boolean | Self::UnsignedByte | Self::Byte | Self::Char => 1,
            Self::UnsignedHalfword | Self::Halfword => 2,
            Self::UnsignedWord | Self::Word | Self::Single => 4,
            Self::Enum(_, inner) => inner.clone().unwrap_or(Self::Word).size_base(),
            Self::Double => 8,
            // Returns 4 on 32-bit and 8 on 64-bit
            // Functions are just pointers to the start of them
            Self::UnsignedLong | Self::Long | Self::Pointer(..) | Self::Function(..) => {
                mem::size_of::<usize>() as u64
            }
            _ => 0,
        }
    }

    /// Returns number of bytes
    pub fn size(&self, module: &RefCell<Module>) -> u64 {
        match self {
            Self::Struct(val, ..) => {
                let size = module
                    .borrow()
                    .types
                    .iter()
                    .find(|td| td.name == val.clone())
                    .unwrap_or_else(|| {
                        elle_error!(Location::internal_error(format!(
                            "Unable to find aggregate type named '{}'.",
                            self.display()
                        )))
                    })
                    .size(module) as u64;

                size
            }
            Self::Unknown(..) | Self::Null => 0,
            _ => self.size_base(),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Boolean | Self::Byte | Self::Char => write!(formatter, "b"),
            Self::Word | Self::Void => write!(formatter, "w"),
            Self::UnsignedByte => write!(formatter, "ub"),
            Self::Halfword => write!(formatter, "h"),
            Self::UnsignedHalfword => write!(formatter, "uh"),
            Self::UnsignedWord => write!(formatter, "uw"),
            Self::UnsignedLong => write!(formatter, "ul"),
            Self::Single => write!(formatter, "s"),
            Self::Double => write!(formatter, "d"),
            Self::Null => write!(formatter, ""),
            Self::Struct(td) => write!(formatter, ":{td}"),
            Self::Enum(_, inner) => {
                write!(
                    formatter,
                    "{}",
                    Option::as_ref(inner).unwrap_or(&Self::Word)
                )
            }
            Self::Pointer(..) | Self::Long | Self::Function(_) => {
                write!(formatter, "l")
            }
            Self::Unknown(name) => elle_error!(Location::internal_error(format!(
                "Tried to compile with a generic type {name}"
            ))),
            x @ Self::Infer => elle_error!(Location::internal_error(format!(
                "Attempted to format an invalid type: {x:?}"
            ))),
        }
    }
}
