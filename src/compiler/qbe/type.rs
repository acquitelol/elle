use core::fmt;
use std::{cell::RefCell, collections::HashMap, iter::Peekable, mem, num::ParseIntError};

use crate::{
    compiler::enums::{Function, Module},
    elle_error, get_POINTER_ID, hashmap, is_generic, is_unknown,
    lexer::enums::Location,
    parser::{
        enums::{Argument, Primitive, StructSource},
        parser::StructPool,
    },
    GENERIC_END, GENERIC_IDENTIFIER, GENERIC_POINTER, GENERIC_UNKNOWN, POINTER_ID, VOID_POINTER_ID,
};

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
    // Unknown generic
    Unknown(String),
    Function(Box<Option<Function>>),
}

impl Type {
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
                if *inner.as_ref() == Type::Char {
                    "string".into()
                } else {
                    format!("{}*", inner.display())
                }
            }
            Self::Single => "f32".into(),
            Self::Double => "f64".into(),
            Self::Void => "void".into(),
            Self::Null => "null".into(),
            Self::Struct(td, ..) => {
                if is_generic!(td) {
                    let (name, parts) = Type::from_internal_id(td.clone());

                    format!(
                        "{name}<{}>",
                        parts
                            .iter()
                            .map(|ty| ty.display())
                            .collect::<Vec<String>>()
                            .join(", ")
                    )
                } else {
                    td.into()
                }
            }
            Self::Function(inner) => {
                if let Some(inner) = *inner.to_owned() {
                    format!(
                        "fn({})",
                        inner
                            .arguments
                            .iter()
                            .map(|arg| arg.0 .0.clone().display())
                            .collect::<Vec<String>>()
                            .join(", ")
                    )
                } else {
                    "<unknown function>".into()
                }
            }
            Self::Unknown(name) => name.into(),
            _ => unreachable!(),
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
            Self::Pointer(inner) => {
                if *inner.as_ref() == Type::Char {
                    "string".into()
                } else {
                    format!("{}*", (*inner).clone().id())
                }
            }
            Self::Single => "f32".into(),
            Self::Double => "f64".into(),
            Self::Void => "void".into(),
            Self::Null => "null".into(),
            Self::Struct(td, ..) => {
                if is_generic!(td) {
                    let (name, parts) = Type::from_internal_id(td.clone());

                    format!(
                        "{name}<{}>",
                        parts
                            .iter()
                            .map(|ty| ty.id())
                            .collect::<Vec<String>>()
                            .join(", ")
                    )
                } else {
                    td.into()
                }
            }
            Self::Function(_) => self.display(),
            _ => "".into(),
        }
    }

    pub fn strict_id(&self) -> String {
        match self {
            x if x.is_string() => "string".into(),
            x if x.is_void_pointer() => VOID_POINTER_ID.into(),
            Type::Pointer(_) => get_POINTER_ID!().into(),
            _ => self.id(),
        }
    }

    pub fn to_internal_id(&self) -> String {
        let num: u8 = match self {
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
            Type::Function(_) => 18,
            _ => 100, // Invalid
        };

        match self {
            Type::Pointer(inner) => format!("{GENERIC_POINTER}.{}", inner.to_internal_id()),
            Type::Struct(name) => name.clone(),
            Type::Unknown(name) => format!("{GENERIC_UNKNOWN}.{name}"),
            _ => num.to_string(),
        }
    }

    // Foo.0.8.10.Bar.ptr.ptr.7.1 turns into
    // ("Foo", vec![Word, Single, Struct("Bar"), Pointer(Pointer(Boolean))])
    pub fn from_internal_id(id: String) -> (String, Vec<Type>) {
        fn is_num_id(id: String) -> Result<u8, ParseIntError> {
            if [
                GENERIC_IDENTIFIER,
                GENERIC_END,
                GENERIC_POINTER,
                GENERIC_UNKNOWN,
            ]
            .contains(&id.as_str())
            {
                "-1".parse::<u8>() // Throw an artificial error
            } else {
                id.parse::<u8>()
            }
        }

        fn id_to_ty(id: String) -> Type {
            match id.parse::<u8>() {
                Ok(inner) => match inner {
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
                    18 => Type::Function(Box::new(None)),
                    _ => todo!("{}", id),
                },
                Err(_) => Type::Struct(id),
            }
        }

        fn internal_match<T>(parts: &mut Peekable<T>) -> Option<Type>
        where
            T: Iterator<Item = String>,
        {
            if parts.peek().is_none() {
                return None;
            }

            let mut part = parts.next().unwrap();
            match is_num_id(part.clone()) {
                Ok(_) => Some(id_to_ty(part)),
                Err(_) => {
                    if &part == GENERIC_POINTER {
                        let mut nesting = 0;

                        while &part == GENERIC_POINTER {
                            if parts.peek().is_none() {
                                return None;
                            }

                            nesting += 1;

                            if parts.peek().is_some_and(|next| next != GENERIC_POINTER) {
                                break;
                            }

                            part = parts.next().unwrap();
                        }

                        let res = internal_match(parts);

                        if let Some(mut res) = res {
                            for _ in 0..nesting {
                                res = Type::Pointer(Box::new(res));
                            }

                            Some(res)
                        } else {
                            None
                        }
                    } else if &part == GENERIC_UNKNOWN {
                        Some(Type::Unknown(parts.next().unwrap()))
                    } else if &part == GENERIC_END {
                        internal_match(parts)
                    } else {
                        Some(Type::Struct(
                            if parts.peek().is_some_and(|part| part == GENERIC_IDENTIFIER) {
                                let mut res = vec![];
                                res.push(parts.next().unwrap());
                                let mut nesting = 0;

                                loop {
                                    if parts.peek().is_some_and(|part| part == GENERIC_IDENTIFIER) {
                                        nesting += 1;
                                    }

                                    if parts.peek().is_some_and(|part| part == GENERIC_END) {
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
                                part
                            },
                        ))
                    }
                }
            }
        }

        let mut parts = id
            .split('.')
            .map(|arg| arg.to_string())
            .collect::<Vec<String>>();

        let name = parts.remove(0);
        assert_eq!(parts.remove(0), GENERIC_IDENTIFIER.to_string());

        let mut res = vec![];
        let mut iter = parts.iter().cloned().peekable();

        while iter.peek().is_some() {
            if let Some(x) = internal_match(&mut iter) {
                res.push(x);
            } else {
                break;
            }
        }

        (name, res)
    }

    pub fn unknown_to_known(
        self,
        struct_pool: Option<&RefCell<StructPool>>,
        tree: Option<&RefCell<Vec<Primitive>>>,
        generics: Vec<String>,
        known_generics: HashMap<String, Type>,
    ) -> Type {
        match self.clone() {
            Type::Pointer(inner) => Type::Pointer(Box::new(inner.unknown_to_known(
                struct_pool,
                tree,
                generics,
                known_generics,
            ))),
            Type::Unknown(name) => {
                if !generics.contains(&name) {
                    self
                } else {
                    known_generics.get(&name).unwrap().to_owned()
                }
            }
            Type::Struct(name) if is_unknown!(name) => {
                let (original_name, generics) = Type::from_internal_id(name.clone());

                let generic_name = format!(
                    "{original_name}.{GENERIC_IDENTIFIER}.{}.{GENERIC_END}",
                    generics
                        .iter()
                        .map(|v| v.get_unknown_inner())
                        .filter(|v| v.is_some())
                        .map(|v| v.unwrap())
                        .map(|generic| {
                            known_generics
                                .get(&generic)
                                .unwrap()
                                .to_internal_id()
                                .to_string()
                        })
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

                    let parsed_members = members
                        .iter()
                        .map(|member| Argument {
                            name: member.name.clone(),
                            r#type: member.r#type.clone().unknown_to_known(
                                struct_pool,
                                tree,
                                generics.clone(),
                                known_generics.clone(),
                            ),
                            manual: member.manual,
                            no_fmt: member.no_fmt,
                        })
                        .collect::<Vec<Argument>>();

                    tree.unwrap().borrow_mut().insert(
                        0,
                        Primitive::Struct(StructSource {
                            name: generic_name.clone(),
                            public: false,
                            usable: true,
                            imported: false,
                            generics: vec![],
                            known_generics: known_generics.clone(),
                            members: parsed_members.clone(),
                            keyword_location: location.clone(),
                            location: location.clone(),
                            ignore_empty: false,
                        }),
                    );

                    struct_pool
                        .unwrap()
                        .borrow_mut()
                        .insert(generic_name.clone(), (vec![], parsed_members, location));
                }

                Type::Struct(generic_name)
            }
            other => other,
        }
    }

    pub fn has_generic_type(self, ty: Type) -> bool {
        match ty.clone() {
            Type::Pointer(inner) => self.has_generic_type(*inner),
            Type::Unknown(_) => true,
            Type::Struct(name) => is_unknown!(name),
            _ => false,
        }
    }

    pub fn deduce_generic_type(self, generic_type: Type) -> Option<HashMap<String, Type>> {
        match (self, generic_type) {
            (Type::Pointer(known_inner), Type::Pointer(generic_inner)) => {
                known_inner.deduce_generic_type(*generic_inner)
            }
            (known, Type::Pointer(other)) if known.is_struct() && other.is_struct() => {
                known.deduce_generic_type(*other)
            }
            (Type::Pointer(known), other) if known.is_struct() && other.is_struct() => {
                known.deduce_generic_type(other)
            }
            (known, Type::Unknown(name)) => Some(hashmap![name => known]),
            // Struct<(known)> vs Struct<T>
            (Type::Struct(specialized_name), Type::Struct(name))
                if is_generic!(specialized_name) && is_generic!(name) =>
            {
                let (original_name, known_parts) = Type::from_internal_id(specialized_name.clone());
                let (struct_name, unknown_parts) = Type::from_internal_id(name.clone());

                if original_name != struct_name {
                    todo!()
                }

                // assert_eq!(known_parts.len(), unknown_parts.len());

                Some(HashMap::from_iter(
                    unknown_parts
                        .iter()
                        .cloned()
                        .enumerate()
                        .map(|(i, v)| {
                            if !matches!(v, Type::Unknown(_)) {
                                if let Some(new) =
                                    known_parts[i].clone().deduce_generic_type(v.clone())
                                {
                                    if new.is_empty() {
                                        return (None, known_parts[i].clone());
                                    }

                                    return (
                                        new.keys().nth(0).cloned(),
                                        new.values().nth(0).cloned().unwrap(),
                                    );
                                }
                            }

                            (v.get_unknown_inner(), known_parts[i].clone())
                        })
                        .filter(|(unknown, _)| unknown.is_some())
                        .map(|(unknown, known)| (unknown.unwrap(), known)),
                ))
            }
            _ => None,
        }
    }

    pub fn get_pointer_inner(&self) -> Option<Type> {
        match self {
            Self::Pointer(ty) => Some(*ty.clone()),
            _ => None,
        }
    }

    pub fn get_struct_inner(&self) -> Option<String> {
        match self.clone() {
            Self::Struct(val, ..) => Some(val),
            _ => None,
        }
    }

    pub fn get_unknown_inner(&self) -> Option<String> {
        match self.clone() {
            Self::Unknown(val) => Some(val),
            _ => None,
        }
    }

    pub fn get_function_inner(&self) -> Option<Option<Function>> {
        match self.clone() {
            Self::Function(val) => Some(*val),
            _ => None,
        }
    }

    pub fn into_abi(self) -> Self {
        match self {
            Self::Byte
            | Self::Char
            | Self::UnsignedByte
            | Self::Halfword
            | Self::UnsignedHalfword
            | Self::UnsignedWord => Self::Word,
            Self::UnsignedLong => Self::Long,
            other => other,
        }
    }

    pub fn into_base(self) -> Self {
        match self {
            Self::Byte
            | Self::Char
            | Self::UnsignedByte
            | Self::Halfword
            | Self::UnsignedHalfword
            | Self::UnsignedWord => Self::Word,
            Self::UnsignedLong => Self::Long,
            Self::Struct(..) => Self::Long,
            other => other,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Self::Single | Self::Double => true,
            _ => false,
        }
    }

    pub fn is_void(&self) -> bool {
        match self {
            Self::Void => true,
            _ => false,
        }
    }

    pub fn is_infer(&self) -> bool {
        match self {
            Self::Infer => true,
            _ => false,
        }
    }

    pub fn is_int(&self) -> bool {
        !self.is_float()
    }

    pub fn is_strictly_number(&self) -> bool {
        !self.is_string() && !self.is_void_pointer() && !self.is_struct() && !self.is_function()
    }

    pub fn is_struct(&self) -> bool {
        match self {
            Self::Struct(..) => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            Self::Pointer(inner) => *inner.as_ref() == Self::Char,
            _ => false,
        }
    }

    pub fn is_void_pointer(&self) -> bool {
        match self {
            Self::Pointer(inner) => *inner.as_ref() == Self::Void,
            _ => false,
        }
    }

    pub fn is_unknown(&self) -> bool {
        match self {
            Self::Unknown(_) => true,
            _ => false,
        }
    }

    pub fn is_function(&self) -> bool {
        match self {
            Self::Function(inner) => inner.is_some(),
            _ => false,
        }
    }

    pub fn is_pointer(&self) -> bool {
        match self {
            Self::Pointer(_) => true,
            _ => false,
        }
    }

    pub fn is_pointer_like(&self) -> bool {
        match self {
            Self::Pointer(_) | Self::Long => true,
            _ => false,
        }
    }

    pub fn is_primitive(&self) -> bool {
        match self {
            Self::Pointer(x) if matches!(**x, Self::Struct(_)) => false,
            Self::Pointer(x) if matches!(**x, Self::Char) => false,
            Self::Struct(_) => false,
            _ => true,
        }
    }

    pub fn is_map_to_int(&self) -> bool {
        match self {
            Self::Byte
            | Self::UnsignedByte
            | Self::Halfword
            | Self::UnsignedHalfword
            | Self::UnsignedWord
            | Self::Boolean
            | Self::Char => true,
            _ => false,
        }
    }

    pub fn is_unsigned(&self) -> bool {
        match self {
            Self::UnsignedByte
            | Self::UnsignedHalfword
            | Self::UnsignedWord
            | Self::UnsignedLong => true,
            _ => false,
        }
    }

    pub fn weight(&self) -> u8 {
        match self {
            Self::Double => 4,
            Self::Single => 3,
            Self::Void
            | Self::Long
            | Self::UnsignedLong
            | Self::Pointer(..)
            | Self::Function(..) => 2,
            Self::Word => 1,
            other if other.is_map_to_int() => 1,
            _ => 0,
        }
    }

    pub fn size_base(&self) -> u64 {
        match self {
            Self::UnsignedByte | Self::Byte | Self::Char => 1,
            Self::UnsignedHalfword | Self::Halfword => 2,
            Self::Boolean | Self::UnsignedWord | Self::Word | Self::Single => 4,
            Self::Double => 8,
            // Returns 4 on 32-bit and 8 on 64-bit
            // Functions are just pointers to the start of them
            Self::Void
            | Self::UnsignedLong
            | Self::Long
            | Self::Pointer(..)
            | Self::Function(..) => mem::size_of::<usize>() as u64,
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
                    .expect(&format!(
                        "Unable to find aggregate type named '{}'.",
                        self.display()
                    ))
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
            Self::Byte => write!(formatter, "b"),
            Self::UnsignedByte => write!(formatter, "ub"),
            Self::Char => write!(formatter, "b"),
            Self::Halfword => write!(formatter, "h"),
            Self::UnsignedHalfword => write!(formatter, "uh"),
            Self::Boolean => write!(formatter, "w"),
            Self::Word => write!(formatter, "w"),
            Self::UnsignedWord => write!(formatter, "uw"),
            Self::Long => write!(formatter, "l"),
            Self::UnsignedLong => write!(formatter, "ul"),
            Self::Pointer(..) => write!(formatter, "l"),
            Self::Single => write!(formatter, "s"),
            Self::Double => write!(formatter, "d"),
            Self::Void => write!(formatter, "l"),
            Self::Null => write!(formatter, ""),
            Self::Struct(td) => write!(formatter, ":{}", td),
            Self::Function(_) => write!(formatter, "l"),
            Self::Unknown(name) => elle_error!(Location::base()
                .internal_error(format!("Tried to compile with a generic type {name}"))),
            x => elle_error!(Location::base()
                .internal_error(format!("Attempted to format an invalid type: {x:?}"))),
        }
    }
}
