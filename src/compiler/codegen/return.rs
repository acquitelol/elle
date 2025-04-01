use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        qbe::{instruction::Instruction, r#type::Type, value::Value},
    },
    parser::enums::Return,
};

impl Codegen<'_> for Return {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let res = self.value.compile(
            gen,
            &CodegenContext {
                is_return: true,
                ..ctx.clone()
            },
        );

        if !ctx.func.borrow_mut().manual {
            ctx.func.borrow_mut().add_instruction(Instruction::Return(
                res.map(|(ty, val)| (ty, val, self.location)),
            ))
        }

        None
    }
}
