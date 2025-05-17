use std::{
    collections::{HashMap, HashSet},
    fmt,
};

use super::{data::Data, function::Function, r#type::Type, statement::Statement, typedef::TypeDef};
use crate::{get_MAIN_ID, hashmap, DEAD_CODE_ELIMINATION_PASSES, MAIN_ID};

#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct Module {
    pub functions: HashMap<String, Function>,
    pub types: Vec<TypeDef>,
    pub data: Vec<Data>,
}

impl Module {
    pub fn new() -> Self {
        Self {
            functions: hashmap![],
            types: vec![],
            data: vec![],
        }
    }

    pub fn add_function(&mut self, function: Function) {
        self.functions.insert(function.name.clone(), function);
    }

    pub fn add_type(&mut self, def: TypeDef) {
        self.types.push(def);
    }

    pub fn add_data(&mut self, data: Data) {
        self.data.push(data);
    }

    pub fn remove_unused_functions(&mut self, object_output: bool) {
        let mut passes = DEAD_CODE_ELIMINATION_PASSES; // should be enough to remove most if not all unused functions

        while passes > 0 {
            passes -= 1;

            let mut used_functions: HashSet<String> = HashSet::new();

            for func in self.functions.values() {
                for block in &func.blocks {
                    for statement in &block.statements {
                        match statement {
                            Statement::Assign(_, _, instr) | Statement::Volatile(instr) => {
                                for other in self.functions.values() {
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

            self.functions.retain(|_, func| {
                used_functions.contains(&func.name) || func.volatile || object_output
            });
        }
    }

    // doesn't need multiple passes because will run after functions
    pub fn remove_unused_data(&mut self) {
        let mut used_data_sections: HashSet<String> = HashSet::new();

        for func in self.functions.values() {
            for block in &func.blocks {
                for statement in &block.statements {
                    match statement {
                        Statement::Assign(_, _, instr) | Statement::Volatile(instr) => {
                            for data in &self.data {
                                if instr.is_global_used(&data.name) {
                                    used_data_sections.insert(data.name.clone());
                                }
                            }
                        }
                    }
                }
            }
        }

        self.data
            .retain(|data| used_data_sections.contains(&data.name));
    }

    pub fn remove_generics(&mut self) {
        self.types.retain(|ty: &TypeDef| {
            !ty.items
                .iter()
                .any(|item| Type::Void.has_generic_type(&item.0))
        });
    }

    pub fn remove_empty_structs(&mut self) {
        self.types.retain(|ty| !ty.items.is_empty());
    }
}

fn print_type_recursively(
    f: &mut impl std::fmt::Write,
    name: &str,
    types: &Vec<TypeDef>,
    printed: &mut HashSet<String>,
) -> std::fmt::Result {
    if printed.contains(name) {
        return Ok(());
    }

    if let Some(r#type) = types.iter().find(|t| t.name == name) {
        for item in &r#type.items {
            if let Some(inner) = item.0.get_struct_inner() {
                print_type_recursively(f, &inner, types, printed)?;
            }
        }

        writeln!(f, "{type}")?;
        printed.insert(name.to_string());
    }

    Ok(())
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut printed = HashSet::new();

        for r#type in &self.types {
            print_type_recursively(f, &r#type.name, &self.types, &mut printed)?;
        }

        for data in &self.data {
            writeln!(f, "{data}")?;
        }

        for func in self.functions.values() {
            // ensure we retain external functions until this point
            // because some data sections may rely on these functions
            // if we remove them the data sections will also be removed
            if !func.external {
                writeln!(f, "{func}")?;
            }
        }

        Ok(())
    }
}
