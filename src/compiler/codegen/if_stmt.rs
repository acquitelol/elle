use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        enums::{Instruction, Type, Value},
    },
    hashmap,
    lexer::enums::TokenKind,
    parser::enums::{AstNode, IfStatement, Literal},
};

impl Codegen<'_> for IfStatement {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        gen.scopes.push(hashmap![]);

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
                &self.location.error(
                    "Unexpected error when trying to compile the condition of an if statement",
                ),
            );

        gen.tmp_counter += 1;

        let true_label = format!("ift.{}", gen.tmp_counter);
        let false_label = format!("iff.{}", gen.tmp_counter);
        let end_label = format!("end.{}", gen.tmp_counter);

        ctx.func
            .borrow_mut()
            .add_instruction(Instruction::JumpNonZero(
                value,
                true_label.clone(),
                if self.else_body.len() > 0 {
                    false_label.clone()
                } else {
                    end_label.clone()
                },
            ));

        ctx.func.borrow_mut().add_block(true_label);

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
                                .add_instruction(Instruction::Literal(value))
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

        if self.else_body.len() > 0 {
            if !ctx
                .func
                .borrow_mut()
                .blocks
                .last()
                .map_or(false, |b| b.jumps())
            {
                ctx.func
                    .borrow_mut()
                    .add_instruction(Instruction::Jump(end_label.clone()));
            }

            ctx.func.borrow_mut().add_block(false_label.clone());

            for statement in self.else_body.iter() {
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
                                    .add_instruction(Instruction::Literal(value))
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
        }

        ctx.func.borrow_mut().add_block(end_label);
        gen.scopes.pop();

        None
    }
}
