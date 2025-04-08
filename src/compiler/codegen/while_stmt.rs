use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        qbe::{instruction::Instruction, r#type::Type, value::Value},
    },
    hashmap,
    lexer::enums::TokenKind,
    parser::enums::{AstNode, Literal, WhileLoopStatement},
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

        let (_, value) = self.condition.compile(gen, ctx).expect(
            &self
                .location
                .error("Unexpected error when trying to compile the condition of a while loop"),
        );

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
            match statement {
                AstNode::Literal(Literal { kind, .. }) => match kind {
                    TokenKind::Break | TokenKind::Continue => {
                        statement.clone().compile(gen, ctx);
                    }
                    _ => {}
                },
                _ => {
                    statement.clone().compile(gen, ctx);
                }
            }
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
