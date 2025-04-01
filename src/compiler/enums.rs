// Roughly references https://github.com/garritfra/qbe-rs/blob/main/src/lib.rs
// https://github.com/garritfra/qbe-rs/blob/main/LICENSE-MIT
use std::{collections::HashSet, fmt};

use super::qbe::{
    data::Data, function::Function, r#type::Type, statement::Statement, typedef::TypeDef,
};
use crate::{get_MAIN_ID, DEAD_CODE_ELIMINATION_PASSES, MAIN_ID};

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

    pub fn remove_unused_functions(&mut self, object_output: bool) {
        let mut passes = DEAD_CODE_ELIMINATION_PASSES; // should be enough to remove most if not all unused functions

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

            used_functions.insert("main".into());
            used_functions.insert(get_MAIN_ID!().into());

            self.functions.retain(|func| {
                used_functions.contains(&func.name) || func.volatile || object_output
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
        self.types.retain(|ty: &TypeDef| {
            ty.items
                .iter()
                .find(|item| Type::Void.has_generic_type(item.0.clone()))
                .is_none()
        })
    }

    pub fn remove_empty_structs(&mut self) {
        self.types.retain(|ty| !ty.items.is_empty())
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
