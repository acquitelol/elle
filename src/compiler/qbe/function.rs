use std::{collections::HashMap, fmt};

use crate::{elle_error, ensure_ascii, lexer::enums::Location};

use super::{
    block::Block, instruction::Instruction, linkage::Linkage, r#type::Type, statement::Statement,
    value::Value,
};

#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct Function {
    pub linkage: Linkage,
    pub name: String,
    pub constant: bool, // used to differentate functions and constants when the constant is callable
    pub variadic: bool,
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
        self.blocks.last().unwrap_or_else(|| {
            elle_error!(Location::internal_error(
                "Function must have at least one block"
            ))
        })
    }

    pub fn add_instruction(&mut self, instruction: Instruction) {
        self.blocks
            .last_mut()
            .unwrap_or_else(|| elle_error!(Location::internal_error("Couldn't find last block!")))
            .add_instruction(instruction);
    }

    pub fn assign_instruction(&mut self, temp: &Value, r#type: &Type, instruction: Instruction) {
        self.blocks
            .last_mut()
            .unwrap_or_else(|| elle_error!(Location::internal_error("Couldn't find last block!")))
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
            .unwrap_or_else(|| elle_error!(Location::internal_error("Couldn't find last block!")))
            .assign_instruction_front(temp, r#type, instruction);
    }

    pub fn returns(&self) -> bool {
        self.last_block()
            .statements
            .last()
            .is_some_and(|i| matches!(i, Statement::Volatile(Instruction::Return(_))))
    }
}

impl fmt::Display for Function {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "{}function", self.linkage)?;

        if let Some(r#type) = &self.return_type {
            let ty = if r#type.is_smaller_than_int() {
                if r#type.is_unsigned() {
                    r#type.to_string()
                } else {
                    format!("s{type}")
                }
            } else {
                r#type.clone().into_abi().to_string()
            };

            write!(formatter, " {ty}")?;
        }

        let mut arguments_clone = self
            .arguments
            .iter()
            .map(|((r#type, temp), _)| {
                let ty = if r#type.is_smaller_than_int() {
                    if r#type.is_unsigned() {
                        r#type.to_string()
                    } else {
                        format!("s{type}")
                    }
                } else {
                    r#type.clone().into_abi().to_string()
                };

                format!("{ty} {temp}")
            })
            .collect::<Vec<_>>();

        if self.variadic {
            arguments_clone.push("...".to_string());
        }

        writeln!(
            formatter,
            " ${name}({args}) {{",
            name = ensure_ascii!(self.name.clone(), wrap),
            args = arguments_clone.join(", "),
        )?;

        for blk in &self.blocks {
            writeln!(formatter, "{blk}")?;
        }

        write!(formatter, "}}")
    }
}
