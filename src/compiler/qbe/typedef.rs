use std::{cell::RefCell, collections::HashMap, fmt};

use super::{module::Module, r#type::Type};
use crate::{elle_error, lexer::enums::Location};

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
                    .unwrap_or_else(|| {
                        elle_error!(Location::internal_error(format!(
                            "Unable to find struct named '{}'",
                            ty.get_struct_inner().unwrap(),
                        )))
                    })
                    .size(module);

                size += tmp_size
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
