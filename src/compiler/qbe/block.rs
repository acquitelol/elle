use std::fmt;

use super::{instruction::Instruction, r#type::Type, statement::Statement, value::Value};

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
                .map(|instr| format!("\t{instr}"))
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}
