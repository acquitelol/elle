use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        enums::{Type, Value},
        qbe::instruction::Instruction,
    },
    hashmap,
    lexer::enums::TokenKind,
    parser::enums::{AstNode, BlockStatement, Literal},
};

impl Codegen<'_> for BlockStatement {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        gen.scopes.push(hashmap![]);
        gen.tmp_counter += 1;

        let body_label = format!("block.start.{}", gen.tmp_counter);
        let end_label = format!("block.end.{}", gen.tmp_counter);
        ctx.func.borrow_mut().add_block(body_label.clone());

        for statement in self.body.iter() {
            match statement {
                AstNode::Literal(Literal { kind, .. }) => match kind {
                    TokenKind::ExactLiteral => {
                        if let Some((_, value)) = statement.clone().compile(gen, ctx) {
                            ctx.func
                                .borrow_mut()
                                .add_instruction(Instruction::Literal(value));
                        }
                    }
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

        ctx.func.borrow_mut().add_block(end_label);
        gen.scopes.pop();
        None
    }
}
