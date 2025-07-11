use std::fmt;

use crate::lexer::enums::{Location, MutRc};

use super::{comparison::Comparison, r#type::Type, value::Value};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Instruction {
    Add(Value, Value),
    Subtract(Value, Value),
    Multiply(Value, Value),
    Divide(Value, Value),
    Modulus(Value, Value),
    BitwiseAnd(Value, Value),
    BitwiseOr(Value, Value),
    BitwiseXor(Value, Value),
    BitwiseNot(Value),
    Negate(Value),
    Compare(Type, Comparison, Value, Value),
    Copy(Value),
    // Location in AST for reporting inconsistent return types
    Return(Option<(Type, Value, MutRc<Location>)>),
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
    Conversion(Type, Type, Value),
    Extension(Type, Value),
    Truncate(Value),
    ShiftLeft(Value, Value),
    ArithmeticShiftRight(Value, Value),
    Blit(Value, Value, u64),
    Phi(Vec<(String, Value)>),
    // LogicalShiftRight(Value, Value),
    #[cfg(debug_assertions)]
    Comment(String),
}

impl Instruction {
    pub fn is_global_used(&self, global_name: &str) -> bool {
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
            | Self::ArithmeticShiftRight(v1, v2)
            | Self::Blit(v1, v2, _) => {
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
            | Self::Copy(v)
            | Self::JumpNonZero(v, _, _)
            | Self::Alloc8(v)
            | Self::Negate(v)
            | Self::BitwiseNot(v) => matches!(v, Value::Global(name) if name == global_name),
            Self::Return(val) => match val {
                Some((_, v, _)) => matches!(v, Value::Global(name) if name == global_name),
                None => false,
            },
            Self::Call(v, args) => {
                let found = matches!(v, Value::Global(name) if name == global_name);

                if found {
                    found
                } else {
                    for arg in args {
                        if matches!(&arg.1, Value::Global(name) if name == global_name) {
                            return true;
                        }
                    }

                    false
                }
            }
            #[cfg(debug_assertions)]
            Self::Comment(_) => false,
            Self::Jump(_) => false,
            Self::Phi(_) => false
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Add(lhs, rhs) => write!(formatter, "add {lhs}, {rhs}"),
            Self::Subtract(lhs, rhs) => write!(formatter, "sub {lhs}, {rhs}"),
            Self::Multiply(lhs, rhs) => write!(formatter, "mul {lhs}, {rhs}"),
            Self::Divide(lhs, rhs) => write!(formatter, "div {lhs}, {rhs}"),
            Self::Modulus(lhs, rhs) => write!(formatter, "rem {lhs}, {rhs}"),
            Self::Compare(ty, comparison, lhs, rhs) => {
                assert!(
                    !matches!(ty, Type::Struct(..)),
                    "Cannot compare struct types ({})",
                    ty
                );

                write!(
                    formatter,
                    // All comparisons start with c
                    "c{}{} {lhs}, {rhs}",
                    if ty.is_float() {
                        match comparison {
                            Comparison::LessThan => "lt",
                            Comparison::LessThanEqual => "le",
                            Comparison::GreaterThan => "gt",
                            Comparison::GreaterThanEqual => "ge",
                            Comparison::Equal => "eq",
                            Comparison::NotEqual => "ne",
                        }
                    } else if ty.is_unsigned() {
                        match comparison {
                            Comparison::LessThan => "ult",
                            Comparison::LessThanEqual => "ule",
                            Comparison::GreaterThan => "ugt",
                            Comparison::GreaterThanEqual => "uge",
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
                )
            }
            Self::BitwiseAnd(lhs, rhs) => write!(formatter, "and {lhs}, {rhs}"),
            Self::BitwiseOr(lhs, rhs) => write!(formatter, "or {lhs}, {rhs}"),
            Self::BitwiseXor(lhs, rhs) => write!(formatter, "xor {lhs}, {rhs}"),
            Self::BitwiseNot(val) => write!(formatter, "xor {val}, -1"),
            Self::Negate(val) => write!(formatter, "neg {val}"),
            Self::Copy(val) => write!(formatter, "copy {val}"),
            // Self::Cast(val) => write!(formatter, "cast {}", val),
            Self::VAArg(val) => write!(formatter, "vaarg {val}"),
            Self::VAStart(val) => write!(formatter, "vastart {val}"),
            Self::Return(val) => match val {
                Some((_, val, _)) => write!(formatter, "ret {val}"),
                None => write!(formatter, "ret"),
            },
            Self::JumpNonZero(val, if_nonzero, if_zero) => {
                write!(formatter, "jnz {val}, @{if_nonzero}, @{if_zero}")
            }
            Self::Jump(label) => write!(formatter, "jmp @{label}"),
            Self::Call(name, args) => {
                write!(
                    formatter,
                    "call {name}({})",
                    args.iter()
                        .map(|(ty, temp)| match ty {
                            Type::Null => temp.to_string(),
                            _ => format!("{} {temp}", ty.clone().into_abi()),
                        })
                        .collect::<Vec<_>>()
                        .join(", "),
                )
            }
            // Self::Alloc4(ty, val) => {
            //     write!(formatter, "alloc4 {}", val)
            // }
            Self::Alloc8(val) => {
                write!(formatter, "alloc8 {val}")
            }
            // Self::Alloc16(size) => write!(formatter, "alloc16 {}", size),
            Self::Store(r#type, dest, value) => {
                write!(
                    formatter,
                    "store{} {}, {}",
                    if r#type.is_unsigned() {
                        r#type.clone().into_signed()
                    } else if r#type.is_struct() {
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
                        format!("s{type}")
                    } else {
                        if r#type.is_struct() {
                            r#type.clone().into_base()
                        } else if r#type.is_unsigned() && !r#type.is_map_to_int() {
                            r#type.clone().into_signed()
                        } else {
                            r#type.clone()
                        }
                        .to_string()
                    },
                    src
                )
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
                    if ty.is_float() || ty.is_unsigned() {
                        ty.to_string()
                    } else {
                        format!("s{ty}")
                    },
                    value
                )
            }
            Self::Truncate(value) => {
                write!(formatter, "truncd {value}")
            }
            Self::ShiftLeft(val, amount) => {
                write!(formatter, "shl {val}, {amount}")
            }
            // Self::LogicalShiftRight(val, amount) => {
            //     write!(formatter, "shr {}, {}", val, amount)
            // }
            Self::ArithmeticShiftRight(val, amount) => {
                write!(formatter, "sar {val}, {amount}")
            }
            Self::Blit(src, dst, size) => {
                write!(formatter, "blit {src}, {dst}, {size}")
            }
            Self::Phi(values) => {
                write!(formatter, "phi ")?;

                for (i, (label, value)) in values.iter().enumerate() {
                    write!(formatter, "@{label} {value}")?;

                    if i + 1 < values.len() {
                        write!(formatter, ", ")?;
                    }
                }

                Ok(())
            }
            #[cfg(debug_assertions)]
            Self::Comment(val) => {
                write!(formatter, "# {val}")
            }
        }
    }
}
