use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        qbe::{instruction::Instruction, r#type::Type, value::Value},
    },
    elle_error, hashmap,
    parser::enums::WhileLoopStatement,
};

impl Codegen<'_> for WhileLoopStatement {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        gen.scopes.push(hashmap![]);

        gen.tmp_counter += 1;
        let cond_label = format!("loop.{}.cond", gen.tmp_counter);
        let step_label = format!("loop.{}.step", gen.tmp_counter);
        let body_label = format!("loop.{}.body", gen.tmp_counter);
        let end_label = format!("loop.{}.end", gen.tmp_counter);

        gen.loop_labels.push(format!("loop.{}", gen.tmp_counter));
        ctx.func.borrow_mut().add_block(cond_label.clone());

        let (_, value) = self.condition.compile(gen, ctx).unwrap_or_else(|| {
            elle_error!(self
                .location
                .borrow()
                .error("Unexpected error when trying to compile the condition of a while loop"))
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
            step.compile(gen, ctx);
        }

        ctx.func
            .borrow_mut()
            .add_instruction(Instruction::Jump(cond_label.clone()));

        ctx.func.borrow_mut().add_block(body_label.clone());

        for statement in self.body.iter() {
            statement.clone().compile(gen, ctx);
        }

        if !ctx
            .func
            .borrow_mut()
            .blocks
            .last()
            .map_or(false, |b| b.jumps())
        {
            ctx.func
                .borrow_mut()
                .add_instruction(Instruction::Jump(step_label));
        }

        ctx.func.borrow_mut().add_block(end_label);
        gen.loop_labels.pop();
        gen.scopes.pop();

        None
    }
}
