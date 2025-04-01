// Roughly references https://github.com/garritfra/qbe-rs/blob/main/src/lib.rs
// https://github.com/garritfra/qbe-rs/blob/main/LICENSE-MIT
use std::{
    collections::{HashMap, HashSet},
    fmt,
};

use super::qbe::{
    data::Data, instruction::Instruction, r#type::Type, statement::Statement, typedef::TypeDef,
    value::Value,
};
use crate::{get_MAIN_ID, lexer::enums::Location, DEAD_CODE_ELIMINATION_PASSES, MAIN_ID};

#[derive(Debug, Clone, Eq, PartialEq, Default)]
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

    pub fn assign_instruction_front(
        &mut self,
        temp: &Value,
        r#type: &Type,
        instruction: Instruction,
    ) {
        self.statements.insert(
            0,
            Statement::Assign(temp.to_owned(), r#type.to_owned().into_abi(), instruction),
        );
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
    pub format: bool,
    pub lambda: bool,
    pub unaliased: Option<String>,
    pub usable: bool,
    pub imported: bool,
    pub generics: Vec<String>,
    pub known_generics: HashMap<String, Type>,
    pub arguments: Vec<((Type, Value), bool)>,
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
            .expect(&Location::base().internal_error("Function must have at least one block"))
    }

    pub fn add_instruction(&mut self, instruction: Instruction) {
        self.blocks
            .last_mut()
            .expect(&Location::base().internal_error("Couldn't find last block!"))
            .add_instruction(instruction);
    }

    pub fn assign_instruction(&mut self, temp: &Value, r#type: &Type, instruction: Instruction) {
        self.blocks
            .last_mut()
            .expect(&Location::base().internal_error("Couldn't find last block!"))
            .assign_instruction(temp, r#type, instruction);
    }

    pub fn assign_instruction_front(
        &mut self,
        temp: &Value,
        r#type: &Type,
        instruction: Instruction,
    ) {
        self.blocks
            .first_mut()
            .expect(&Location::base().internal_error("Couldn't find last block!"))
            .assign_instruction_front(temp, r#type, instruction);
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
            write!(formatter, " {}", r#type.clone().into_abi())?;
        }

        let mut arguments_clone = self
            .arguments
            .iter()
            .map(|((r#type, temp), _)| format!("{} {}", r#type, temp))
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
