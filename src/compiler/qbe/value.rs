use std::fmt;

use crate::{elle_error, ensure_ascii, lexer::enums::Location};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Value {
    Temporary(String),
    Global(String),

    /// Const(prefix, literal)
    Const(String, i128),
    Literal(String),
}

impl Value {
    pub fn get_string_inner(&self) -> String {
        match self.clone() {
            Self::Temporary(val) => ensure_ascii!(val, encode),
            Self::Global(val) => ensure_ascii!(val, wrap),
            Self::Literal(val) => val,
            Self::Const(..) => {
                elle_error!(Location::internal_error(format!(
                    "Invalid value type {self}"
                )))
            }
        }
    }
}

impl fmt::Display for Value {
    /// Value prefixes based on sigils
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Temporary(name) => write!(formatter, "%{}", ensure_ascii!(name.clone(), encode)),
            Self::Global(name) => write!(formatter, "${}", ensure_ascii!(name.clone(), wrap)),
            Self::Const(prefix, value) => {
                write!(formatter, "{prefix}{value}")
            }
            Self::Literal(value) => write!(formatter, "{value}"),
        }
    }
}
