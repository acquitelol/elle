// Roughly references https://github.com/garritfra/qbe-rs/blob/main/src/lib.rs
// https://github.com/garritfra/qbe-rs/blob/main/LICENSE-MIT
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt::{self},
    iter::Peekable,
    mem,
    num::ParseIntError,
    u8,
};

use crate::{
    hashmap, is_generic,
    lexer::enums::Location,
    parser::{
        enums::{Argument, Primitive},
        parser::StructPool,
    },
    GENERIC_END, GENERIC_IDENTIFIER, GENERIC_POINTER, GENERIC_UNKNOWN,
};

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Copy)]
pub enum Comparison {
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Equal,
    NotEqual,
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Instruction {
    Add(Value, Value),
    Subtract(Value, Value),
    Multiply(Value, Value),
    Divide(Value, Value),
    Modulus(Value, Value),
    BitwiseAnd(Value, Value),
    BitwiseOr(Value, Value),
    BitwiseXor(Value, Value),
    Compare(Type, Comparison, Value, Value),
    Copy(Value),
    // Location in AST for reporting inconsistent return types
    Return(Option<(Type, Value, Location)>),
    JumpNonZero(Value, String, String),
    Jump(String),
    Call(Value, Vec<(Type, Value)>),
    // Cast(Value),
    VAArg(Value),
    VAStart(Value),
    // Alloc4(Type, Value),
    Alloc8(Value),
    // Alloc16(u128),
    Store(Type, Value, Value),
    Load(Type, Value),
    Literal(Value),
    Conversion(Type, Type, Value),
    Extension(Type, Value),
    Truncate(Value),
    ShiftLeft(Value, Value),
    ArithmeticShiftRight(Value, Value),
    // LogicalShiftRight(Value, Value),
    #[cfg(debug_assertions)]
    Comment(String),
}

impl Instruction {
    fn is_global_used(&self, global_name: &str) -> bool {
        match self {
            Self::Add(v1, v2)
            | Self::Subtract(v1, v2)
            | Self::Multiply(v1, v2)
            | Self::Divide(v1, v2)
            | Self::Modulus(v1, v2)
            | Self::BitwiseAnd(v1, v2)
            | Self::BitwiseOr(v1, v2)
            | Self::BitwiseXor(v1, v2)
            | Self::Compare(_, _, v1, v2)
            | Self::Store(_, v1, v2)
            | Self::ShiftLeft(v1, v2)
            // | Self::LogicalShiftRight(v1, v2)
            | Self::ArithmeticShiftRight(v1, v2) => {
                matches!(v1, Value::Global(name) if name == global_name)
                    || matches!(v2, Value::Global(name) if name == global_name)
            }
            Self::Load(_, v)
            | Self::Conversion(_, _, v)
            | Self::Extension(_, v)
            | Self::Truncate(v)
            // | Self::Cast(v)
            | Self::VAArg(v)
            | Self::VAStart(v)
            | Self::Literal(v)
            | Self::Copy(v)
            | Self::JumpNonZero(v, _, _)
            | Self::Alloc8(v) => matches!(v, Value::Global(name) if name == global_name),
            Self::Return(val) => match val {
                Some((_, v, _)) => matches!(v, Value::Global(name) if name == global_name),
                None => false,
            },
            Self::Call(v, args) => {
                let found = matches!(v, Value::Global(name) if name == global_name);

                if found {
                    found
                } else {
                    for arg in args.iter().cloned() {
                        if matches!(arg.1, Value::Global(name) if name == global_name) {
                            return true;
                        }
                    }

                    false
                }
            }
            #[cfg(debug_assertions)]
            Self::Comment(_) => false,
            Self::Jump(_) => false
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Add(lhs, rhs) => write!(formatter, "add {}, {}", lhs, rhs),
            Self::Subtract(lhs, rhs) => write!(formatter, "sub {}, {}", lhs, rhs),
            Self::Multiply(lhs, rhs) => write!(formatter, "mul {}, {}", lhs, rhs),
            Self::Divide(lhs, rhs) => write!(formatter, "div {}, {}", lhs, rhs),
            Self::Modulus(lhs, rhs) => write!(formatter, "rem {}, {}", lhs, rhs),
            Self::Compare(ty, comparison, lhs, rhs) => {
                assert!(
                    !matches!(ty, Type::Struct(..)),
                    "Cannot compare struct types"
                );

                write!(
                    formatter,
                    // All comparisons start with c
                    "c{}{} {}, {}",
                    if ty.is_float() || ty.is_unsigned() {
                        match comparison {
                            Comparison::LessThan => "lt",
                            Comparison::LessThanEqual => "le",
                            Comparison::GreaterThan => "gt",
                            Comparison::GreaterThanEqual => "ge",
                            Comparison::Equal => "eq",
                            Comparison::NotEqual => "ne",
                        }
                    } else {
                        match comparison {
                            Comparison::LessThan => "slt",
                            Comparison::LessThanEqual => "sle",
                            Comparison::GreaterThan => "sgt",
                            Comparison::GreaterThanEqual => "sge",
                            Comparison::Equal => "eq",
                            Comparison::NotEqual => "ne",
                        }
                    },
                    ty.clone().into_abi(),
                    lhs,
                    rhs,
                )
            }
            Self::BitwiseAnd(lhs, rhs) => write!(formatter, "and {}, {}", lhs, rhs),
            Self::BitwiseOr(lhs, rhs) => write!(formatter, "or {}, {}", lhs, rhs),
            Self::BitwiseXor(lhs, rhs) => write!(formatter, "xor {}, {}", lhs, rhs),
            Self::Copy(val) => write!(formatter, "copy {}", val),
            // Self::Cast(val) => write!(formatter, "cast {}", val),
            Self::VAArg(val) => write!(formatter, "vaarg {}", val),
            Self::VAStart(val) => write!(formatter, "vastart {}", val),
            Self::Return(val) => match val {
                Some((_, val, _)) => write!(formatter, "ret {}", val),
                None => write!(formatter, "ret"),
            },
            Self::JumpNonZero(val, if_nonzero, if_zero) => {
                write!(formatter, "jnz {}, @{}, @{}", val, if_nonzero, if_zero)
            }
            Self::Jump(label) => write!(formatter, "jmp @{}", label),
            Self::Call(name, args) => {
                write!(
                    formatter,
                    "call {}({})",
                    name,
                    args.iter()
                        .map(|(ty, temp)| match ty {
                            Type::Null => format!("{}", temp),
                            _ => format!("{} {}", ty.clone().into_abi(), temp),
                        })
                        .collect::<Vec<String>>()
                        .join(", "),
                )
            }
            // Self::Alloc4(ty, val) => {
            //     write!(formatter, "alloc4 {}", val)
            // }
            Self::Alloc8(val) => {
                write!(formatter, "alloc8 {}", val)
            }
            // Self::Alloc16(size) => write!(formatter, "alloc16 {}", size),
            Self::Store(r#type, dest, value) => {
                write!(
                    formatter,
                    "store{} {}, {}",
                    if r#type.clone() != Type::Char {
                        r#type.clone().into_base()
                    } else {
                        r#type.clone()
                    },
                    value,
                    dest
                )
            }
            Self::Load(r#type, src) => {
                write!(
                    formatter,
                    "load{} {}",
                    if !r#type.is_unsigned() && r#type.is_map_to_int() {
                        format!("s{}", r#type.clone())
                    } else {
                        if r#type.is_struct() {
                            r#type.clone().into_base()
                        } else {
                            r#type.clone()
                        }
                        .to_string()
                    },
                    src
                )
            }
            Self::Literal(val) => {
                write!(formatter, "{}", val)
            }
            Self::Conversion(first, second, value) => {
                write!(
                    formatter,
                    "{}to{} {}",
                    if first.is_float() {
                        first.to_string()
                    } else {
                        format!("s{}", first.clone().into_abi())
                    },
                    if second.is_float() { "f" } else { "si" },
                    value
                )
            }
            Self::Extension(ty, value) => {
                write!(
                    formatter,
                    "ext{} {}",
                    if ty.is_float() {
                        ty.to_string()
                    } else {
                        format!("s{}", ty)
                    },
                    value
                )
            }
            Self::Truncate(value) => {
                write!(formatter, "truncd {}", value)
            }
            Self::ShiftLeft(val, amount) => {
                write!(formatter, "shl {}, {}", val, amount)
            }
            // Self::LogicalShiftRight(val, amount) => {
            //     write!(formatter, "shr {}, {}", val, amount)
            // }
            Self::ArithmeticShiftRight(val, amount) => {
                write!(formatter, "sar {}, {}", val, amount)
            }
            #[cfg(debug_assertions)]
            Self::Comment(val) => {
                write!(formatter, "# {}", val)
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
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
    // Inner type
    Pointer(Box<Type>),
    Struct(String),
    // Unknown generic
    Unknown(String),
}

impl Type {
    pub fn display(&self) -> String {
        match self {
            Self::Byte => "byte".into(),
            Self::UnsignedByte => "ubyte".into(),
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
            Self::Unknown(name) => name.into(),
        }
    }

    pub fn id(&self) -> String {
        match self {
            Self::Char => "char".into(),
            Self::Boolean => "bool".into(),
            Self::Word => "i32".into(),
            Self::Long => "i64".into(),
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
            _ => "".into(),
        }
    }

    pub fn to_internal_id(&self) -> String {
        let num: u8 = match self {
            Type::UnsignedByte => 4,
            Type::UnsignedHalfword => 5,
            Type::UnsignedWord => 6,
            Type::UnsignedLong => 7,
            Type::Byte => 9,
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
                "-1".parse::<u8>()
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
                            part = parts.next().unwrap();
                        }

                        let mut res = id_to_ty(part);

                        for _ in 0..nesting {
                            res = Type::Pointer(Box::new(res));
                        }

                        Some(res)
                    } else if &part == GENERIC_UNKNOWN {
                        Some(Type::Unknown(parts.next().unwrap()))
                    } else if &part == GENERIC_END {
                        None
                    } else {
                        Some(Type::Struct(part))
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
        extra_structs: Option<&RefCell<Vec<Primitive>>>,
        generics: Vec<String>,
        known_generics: HashMap<String, Type>,
    ) -> Type {
        match self.clone() {
            Type::Pointer(inner) => Type::Pointer(Box::new(inner.unknown_to_known(
                struct_pool,
                extra_structs,
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
            Type::Struct(name) if is_generic!(name) => {
                let (original_name, _) = Type::from_internal_id(name.clone());

                let generic_name = format!(
                    "{original_name}.{GENERIC_IDENTIFIER}.{}.{GENERIC_END}",
                    generics
                        .iter()
                        .map(|generic| {
                            known_generics
                                .get(generic)
                                .unwrap()
                                .to_internal_id()
                                .to_string()
                        })
                        .collect::<Vec<String>>()
                        .join(".")
                );

                if struct_pool.is_some()
                    && extra_structs.is_some()
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
                                extra_structs,
                                generics.clone(),
                                known_generics.clone(),
                            ),
                        })
                        .collect::<Vec<Argument>>();

                    extra_structs.unwrap().borrow_mut().push(Primitive::Struct {
                        name: generic_name.clone(),
                        public: false,
                        usable: true,
                        imported: false,
                        generics: vec![],
                        known_generics: known_generics.clone(),
                        members: parsed_members.clone(),
                        location: location.clone(),
                    });

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
            Type::Struct(name) => {
                name.contains(&format!(".{}", Type::Unknown("T".into()).to_internal_id()))
            }
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

                assert_eq!(known_parts.len(), unknown_parts.len());

                Some(HashMap::from_iter(
                    unknown_parts
                        .iter()
                        .cloned()
                        .enumerate()
                        .map(|(i, v)| (v.get_unknown_inner().unwrap(), known_parts[i].clone())),
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

    pub fn is_int(&self) -> bool {
        !self.is_float()
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

    pub fn is_unknown(&self) -> bool {
        match self {
            Self::Unknown(_) => true,
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
            Self::Long | Self::UnsignedLong | Self::Pointer(..) => 2,
            Self::Word => 1,
            other if other.is_map_to_int() => 1,
            _ => 0,
        }
    }

    /// Returns number of bytes
    pub fn size(&self, module: &RefCell<Module>) -> u64 {
        match self {
            Self::UnsignedByte | Self::Byte | Self::Char => 1,
            Self::UnsignedHalfword | Self::Halfword => 2,
            Self::UnsignedWord | Self::Word | Self::Single => 4,
            Self::Double => 8,
            // Returns 4 on 32-bit and 8 on 64-bit
            Self::UnsignedLong | Self::Long | Self::Pointer(..) => mem::size_of::<usize>() as u64,
            Self::Struct(val, ..) => {
                let size = module
                    .borrow()
                    .types
                    .iter()
                    .find(|td| td.name == val.clone())
                    .expect(&format!(
                        "Unable to find aggregate type named '{}'",
                        self.display()
                    ))
                    .size(module) as u64;

                if size < mem::size_of::<usize>() as u64 {
                    mem::size_of::<usize>() as u64
                } else {
                    size
                }
            }
            _ => 0,
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
            Self::Void => write!(formatter, "w"),
            Self::Null => write!(formatter, ""),
            Self::Struct(td) => write!(formatter, ":{}", td),
            Self::Unknown(name) => panic!("Tried to compile with a generic type {name}"),
        }
    }
}

/// QBE aggregate type definition
#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct TypeDef {
    pub name: String,
    pub align: Option<u64>,
    pub known_generics: HashMap<String, Type>,
    pub items: Vec<(Type, usize)>,
    pub public: bool,
    pub usable: bool,
    pub imported: bool,
}

impl TypeDef {
    pub fn size(&self, module: &RefCell<Module>) -> usize {
        let mut size = 0;

        for (ty, _) in self.items.iter().cloned() {
            if ty.is_struct() {
                let tmp_size = module
                    .borrow()
                    .types
                    .iter()
                    .find(|td| td.name == ty.get_struct_inner().unwrap())
                    .expect(&format!(
                        "Unable to find struct named '{}'",
                        ty.get_struct_inner().unwrap(),
                    ))
                    .size(module);

                size += if tmp_size < mem::size_of::<usize>() {
                    mem::size_of::<usize>()
                } else {
                    tmp_size
                }
            } else {
                size += ty.size(module) as usize;
            }
        }

        size
    }
}

impl fmt::Display for TypeDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "type :{} = ", self.name)?;
        if let Some(align) = self.align {
            write!(f, "align {} ", align)?;
        }

        write!(
            f,
            "{{ {} }}",
            self.items
                .iter()
                .map(|(ty, count)| if *count > 1 {
                    format!(
                        "{} {}",
                        if !ty.is_struct() {
                            ty.clone().into_base()
                        } else {
                            ty.clone()
                        },
                        count
                    )
                } else {
                    format!(
                        "{}",
                        if !ty.is_struct() {
                            ty.clone().into_base()
                        } else {
                            ty.clone()
                        }
                    )
                })
                .collect::<Vec<String>>()
                .join(", "),
        )
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Value {
    Temporary(String),
    Global(String),

    /// Const(prefix, literal)
    Const(Type, i128),
    Literal(String),
}

impl Value {
    pub fn get_string_inner(&self) -> String {
        match self.clone() {
            Self::Temporary(val) => val,
            Self::Global(val) => val,
            Self::Literal(val) => val,
            _ => panic!("Invalid value type {}", self),
        }
    }
}

impl fmt::Display for Value {
    /// Value prefixes based on sigils
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Temporary(name) => write!(formatter, "%{}", name),
            Self::Global(name) => write!(formatter, "${}", name),
            Self::Const(ty, value) => {
                let prefix = if ty.clone() == Type::Double {
                    "d_"
                } else if ty.clone() == Type::Single {
                    "s_"
                } else {
                    ""
                };

                write!(formatter, "{}", format!("{}{}", prefix, value))
            }
            Self::Literal(value) => write!(formatter, "{}", value),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Default)]
pub struct Data {
    pub linkage: Linkage,
    pub name: String,
    pub align: Option<u64>,
    pub items: Vec<(Type, DataItem)>,
}

impl Data {
    pub fn new(
        linkage: Linkage,
        name: String,
        align: Option<u64>,
        items: Vec<(Type, DataItem)>,
    ) -> Self {
        Self {
            linkage,
            name,
            align,
            items,
        }
    }
}

impl fmt::Display for Data {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "{}data ${} = ", self.linkage, self.name)?;

        if let Some(align) = self.align {
            write!(formatter, "align {} ", align)?;
        }
        write!(
            formatter,
            "{{ {} }}",
            self.items
                .iter()
                .map(|(ty, item)| format!("{} {}", ty, item))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum DataItem {
    String(String),
    Const(i128),
}

impl fmt::Display for DataItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(string) => write!(f, "\"{}\"", string),
            Self::Const(val) => write!(f, "{}", val),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Statement {
    Assign(Value, Type, Instruction),
    Volatile(Instruction),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Assign(temp, ty, instr) => {
                assert!(matches!(temp, Value::Temporary(_)));
                write!(f, "{} ={} {}", temp, ty, instr)
            }
            Self::Volatile(instr) => write!(f, "{}", instr),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Default)]
pub struct Block {
    pub label: String,
    pub statements: Vec<Statement>,
}

impl Block {
    pub fn add_instruction(&mut self, instr: Instruction) {
        self.statements.push(Statement::Volatile(instr));
    }

    pub fn assign_instruction(&mut self, temp: &Value, r#type: &Type, instruction: Instruction) {
        self.statements.push(Statement::Assign(
            temp.to_owned(),
            r#type.to_owned().into_abi(),
            instruction,
        ));
    }

    /// Returns true if the block's last instruction is a jump
    pub fn jumps(&self) -> bool {
        let last = self.statements.last();

        if let Some(Statement::Volatile(instruction)) = last {
            matches!(
                instruction,
                Instruction::Return(_) | Instruction::Jump(_) | Instruction::JumpNonZero(..)
            )
        } else {
            false
        }
    }
}

impl fmt::Display for Block {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        writeln!(formatter, "@{}", self.label)?;

        write!(
            formatter,
            "{}",
            self.statements
                .iter()
                .map(
                    |instr| if let Statement::Assign(val, ty, ins) = instr.clone() {
                        if matches!(ins, Instruction::Copy(_) | Instruction::Load(_, _)) {
                            Statement::Assign(val, ty.into_base(), ins)
                        } else {
                            instr.clone()
                        }
                    } else {
                        instr.clone()
                    }
                )
                .map(|instr| format!("\t{}", instr))
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct Function {
    pub linkage: Linkage,
    pub name: String,
    pub variadic: bool,
    pub manual: bool,
    pub external: bool,
    pub builtin: bool,
    pub volatile: bool,
    pub unaliased: Option<String>,
    pub usable: bool,
    pub imported: bool,
    pub generics: Vec<String>,
    pub known_generics: HashMap<String, Type>,
    pub arguments: Vec<(Type, Value)>,
    pub return_type: Option<Type>,
    pub blocks: Vec<Block>,
}

impl Function {
    pub fn add_block(&mut self, label: impl Into<String>) -> &mut Block {
        self.blocks.push(Block {
            label: label.into(),
            statements: vec![],
        });

        self.blocks.last_mut().unwrap()
    }

    pub fn last_block(&self) -> &Block {
        self.blocks
            .last()
            .expect("Function must have at least one block")
    }

    pub fn add_instruction(&mut self, instruction: Instruction) {
        self.blocks
            .last_mut()
            .expect("Couldn't find last block!")
            .add_instruction(instruction);
    }

    pub fn assign_instruction(&mut self, temp: &Value, r#type: &Type, instruction: Instruction) {
        self.blocks
            .last_mut()
            .expect("Couldn't find last block!")
            .assign_instruction(temp, r#type, instruction);
    }

    pub fn returns(&self) -> bool {
        let last = self.last_block().statements.last();

        last.map_or(false, |i| {
            matches!(i, Statement::Volatile(Instruction::Return(_)))
        })
    }
}

impl fmt::Display for Function {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "{}function", self.linkage)?;

        if let Some(r#type) = &self.return_type {
            write!(formatter, " {}", r#type)?;
        }

        let mut arguments_clone = self
            .arguments
            .iter()
            .map(|(r#type, temp)| format!("{} {}", r#type, temp))
            .collect::<Vec<String>>()
            .clone();

        if self.variadic {
            arguments_clone.push("...".to_string());
        }

        writeln!(
            formatter,
            " ${name}({args}) {{",
            name = self.name,
            args = arguments_clone.join(", "),
        )?;

        for blk in self.blocks.iter() {
            writeln!(formatter, "{}", blk)?;
        }

        write!(formatter, "}}")
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Default)]
pub struct Linkage {
    pub exported: bool,
    pub section: Option<String>,
    pub secflags: Option<String>,
}

impl Linkage {
    pub fn private() -> Linkage {
        Linkage {
            exported: false,
            section: None,
            secflags: None,
        }
    }

    pub fn public() -> Linkage {
        Linkage {
            exported: true,
            section: None,
            secflags: None,
        }
    }
}

impl fmt::Display for Linkage {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        if self.exported {
            write!(formatter, "export ")?;
        }
        if let Some(section) = &self.section {
            write!(formatter, "section \"{}\"", section)?;

            if let Some(secflags) = &self.secflags {
                write!(formatter, " \"{}\"", secflags)?;
            }

            write!(formatter, " ")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct Module {
    pub functions: Vec<Function>,
    pub types: Vec<TypeDef>,
    pub data: Vec<Data>,
}

impl Module {
    pub fn new() -> Module {
        Module {
            functions: vec![],
            types: vec![],
            data: vec![],
        }
    }

    pub fn add_function(&mut self, function: Function) -> &mut Function {
        self.functions.push(function);
        return self.functions.last_mut().unwrap();
    }

    pub fn add_type(&mut self, def: TypeDef) -> &mut TypeDef {
        self.types.push(def);
        self.types.last_mut().unwrap()
    }

    pub fn add_data(&mut self, data: Data) -> &mut Data {
        self.data.push(data);
        self.data.last_mut().unwrap()
    }

    pub fn remove_unused_functions(&mut self) {
        let mut passes = 5; // should be enough to remove most if not all unused functions

        while passes > 0 {
            passes -= 1;

            let mut used_functions: HashSet<String> = HashSet::new();

            for func in self.functions.iter() {
                for block in func.blocks.iter() {
                    for statement in block.statements.iter() {
                        match statement {
                            Statement::Assign(_, _, instr) | Statement::Volatile(instr) => {
                                for other in self.functions.iter() {
                                    if instr.is_global_used(&other.name) {
                                        used_functions.insert(other.name.clone());
                                    }
                                }
                            }
                        }
                    }
                }
            }

            used_functions.insert("main".to_string());

            self.functions.retain(|func| {
                if !used_functions.contains(&func.name) && !func.volatile {
                    false
                } else {
                    true
                }
            });
        }
    }

    // doesn't need multiple passes because will run after functions
    pub fn remove_unused_data(&mut self) {
        let mut used_data_sections: HashSet<String> = HashSet::new();

        for func in self.functions.iter() {
            for block in func.blocks.iter() {
                for statement in block.statements.iter() {
                    match statement {
                        Statement::Assign(_, _, instr) | Statement::Volatile(instr) => {
                            for data in self.data.iter() {
                                if instr.is_global_used(&data.name) {
                                    used_data_sections.insert(data.name.clone());
                                }
                            }
                        }
                    }
                }
            }
        }

        self.data.retain(|data| {
            if !used_data_sections.contains(&data.name) {
                false
            } else {
                true
            }
        });
    }

    pub fn remove_generics(&mut self) {
        self.types.retain(|ty| {
            ty.known_generics
                .iter()
                .find(|inner| match inner {
                    (_, Type::Unknown(_)) => true,
                    _ => false,
                })
                .is_none()
        })
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for r#type in self.types.iter() {
            writeln!(f, "{}", r#type)?;
        }

        for data in self.data.iter() {
            writeln!(f, "{}", data)?;
        }

        for func in self.functions.iter() {
            // ensure we retain external functions until this point
            // because some data sections may rely on these functions
            // if we remove them the data sections will also be removed
            if !func.external {
                writeln!(f, "{}", func)?;
            }
        }

        Ok(())
    }
}
