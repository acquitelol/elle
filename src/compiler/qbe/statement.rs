use std::fmt;

use super::{instruction::Instruction, r#type::Type, value::Value};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Statement {
    Assign(Value, Type, Instruction),
    Volatile(Instruction),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Assign(temp, ty, instr) => {
                assert!(matches!(temp, Value::Temporary(_)));
                write!(
                    f,
                    "{} ={} {}",
                    temp,
                    if matches!(instr, Instruction::Call(..)) {
                        ty.clone().into_abi()
                    } else {
                        ty.clone().into_base()
                    },
                    instr
                )
            }
            Self::Volatile(instr) => write!(f, "{instr}"),
        }
    }
}
