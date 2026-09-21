use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        qbe::{block::Block, instruction::Instruction, r#type::Type, value::Value},
    },
    elle_error, hashmap,
    parser::enums::WhileLoopStatement,
};

impl Codegen<'_> for WhileLoopStatement {
    fn compile(self, compiler: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        compiler.scopes.push(hashmap![]);

        compiler.tmp_counter += 1;
        let cond_label = format!("loop.{}.cond", compiler.tmp_counter);
        let step_label = format!("loop.{}.step", compiler.tmp_counter);
        let body_label = format!("loop.{}.body", compiler.tmp_counter);
        let end_label = format!("loop.{}.end", compiler.tmp_counter);

        compiler
            .loop_labels
            .push(format!("loop.{}", compiler.tmp_counter));
        ctx.func.borrow_mut().add_block(cond_label.clone());

        let (_, value) = self.condition.compile(compiler, ctx).unwrap_or_else(|| {
            elle_error!(
                self.location
                    .borrow()
                    .error("Unexpected error when trying to compile the condition of a while loop")
            )
        });

        ctx.func
            .borrow_mut()
            .add_instruction(Instruction::JumpNonZero(
                value,
                body_label.clone(),
                end_label.clone(),
            ));

        ctx.func.borrow_mut().add_block(step_label.clone());

        if let Some(step) = self.step {
            step.compile(compiler, ctx);
        }

        ctx.func
            .borrow_mut()
            .add_instruction(Instruction::Jump(cond_label));

        ctx.func.borrow_mut().add_block(body_label);

        for statement in &self.body {
            statement.clone().compile(compiler, ctx);
        }

        if !ctx
            .func
            .borrow_mut()
            .blocks
            .last()
            .is_some_and(Block::jumps)
        {
            ctx.func
                .borrow_mut()
                .add_instruction(Instruction::Jump(step_label));
        }

        ctx.func.borrow_mut().add_block(end_label);
        compiler.loop_labels.pop();
        compiler.scopes.pop();

        None
    }
}
