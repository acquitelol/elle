use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        qbe::{instruction::Instruction, r#type::Type, value::Value},
    },
    elle_error, hashmap,
    parser::enums::IfStatement,
};

macro_rules! ensure_jumps {
    ($ctx:ident, $end_label:expr) => {
        if !$ctx
            .func
            .borrow_mut()
            .blocks
            .last()
            .map_or(false, |b| b.jumps())
        {
            $ctx.func
                .borrow_mut()
                .add_instruction(Instruction::Jump($end_label.clone()));
        }
    };
}

impl Codegen<'_> for IfStatement {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        gen.scopes.push(hashmap![]);

        gen.tmp_counter += 1;
        let mut current_false_label = format!("iff.{}", gen.tmp_counter);
        let end_label = format!("end.{}", gen.tmp_counter);

        let (_, if_value) = self.condition.compile(gen, ctx).unwrap_or_else(|| {
            elle_error!(self
                .location
                .error("Unexpected error when trying to compile the condition of an if statement"))
        });

        let if_true_label = format!("ift.{}", gen.tmp_counter);

        ctx.func
            .borrow_mut()
            .add_instruction(Instruction::JumpNonZero(
                if_value,
                if_true_label.clone(),
                if self.elifs.is_empty() && self.else_body.is_empty() {
                    end_label.clone()
                } else {
                    current_false_label.clone()
                },
            ));

        ctx.func.borrow_mut().add_block(if_true_label);
        for statement in &self.body {
            statement.clone().compile(gen, ctx);
        }

        ensure_jumps!(ctx, end_label);
        let elifs_len = self.elifs.len();

        for (i, (elif_cond, elif_body)) in self.elifs.into_iter().enumerate() {
            let elif_true_label = format!("elift.{}.{}", gen.tmp_counter, i);
            let next_false_label = format!("eliff.{}.{}", gen.tmp_counter, i);

            ctx.func.borrow_mut().add_block(current_false_label.clone());

            let (_, cond_val) = elif_cond.compile(gen, ctx).unwrap_or_else(|| {
                elle_error!(self
                    .location
                    .error("Unexpected error when compiling else if condition"))
            });

            ctx.func
                .borrow_mut()
                .add_instruction(Instruction::JumpNonZero(
                    cond_val,
                    elif_true_label.clone(),
                    if self.else_body.is_empty() && i == elifs_len - 1 {
                        end_label.clone()
                    } else {
                        next_false_label.clone()
                    },
                ));

            ctx.func.borrow_mut().add_block(elif_true_label);

            for stmt in elif_body {
                stmt.compile(gen, ctx);
            }

            ensure_jumps!(ctx, end_label);
            current_false_label = next_false_label;
        }

        if !self.else_body.is_empty() {
            ctx.func.borrow_mut().add_block(current_false_label.clone());

            for statement in &self.else_body {
                statement.clone().compile(gen, ctx);
            }

            ensure_jumps!(ctx, end_label);
        }

        ctx.func.borrow_mut().add_block(end_label);
        gen.scopes.pop();

        None
    }
}
