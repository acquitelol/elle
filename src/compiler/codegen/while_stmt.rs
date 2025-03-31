use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        enums::{Instruction, Type, Value},
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

        let (_, value) = gen
            .generate_statement(
                ctx.func,
                ctx.module,
                *self.condition,
                ctx.ty.clone(),
                None,
                false,
            )
            .expect(
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
            gen.generate_statement(ctx.func, ctx.module, *step, ctx.ty.clone(), None, false);
        }

        ctx.func
            .borrow_mut()
            .add_instruction(Instruction::Jump(cond_label.clone()));

        ctx.func.borrow_mut().add_block(body_label.clone());

        for statement in self.body.iter() {
            match statement {
                AstNode::Literal(Literal { kind, .. }) => match kind {
                    TokenKind::ExactLiteral => {
                        if let Some((_, value)) = gen.generate_statement(
                            ctx.func,
                            ctx.module,
                            statement.clone(),
                            None,
                            None,
                            false,
                        ) {
                            ctx.func
                                .borrow_mut()
                                .add_instruction(Instruction::Literal(value));
                        }
                    }
                    TokenKind::Break | TokenKind::Continue => {
                        gen.generate_statement(
                            ctx.func,
                            ctx.module,
                            statement.clone(),
                            None,
                            None,
                            false,
                        );
                    }
                    _ => {}
                },
                _ => {
                    gen.generate_statement(
                        ctx.func,
                        ctx.module,
                        statement.clone(),
                        None,
                        None,
                        false,
                    );
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
