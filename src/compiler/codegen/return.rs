use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        enums::{Instruction, Type, Value},
    },
    parser::enums::Return,
};

impl Codegen<'_> for Return {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let res = gen.generate_statement(
            ctx.func,
            ctx.module,
            *self.value,
            ctx.ty.clone(),
            None,
            true,
        );

        if !ctx.func.borrow_mut().manual {
            ctx.func.borrow_mut().add_instruction(Instruction::Return(
                res.map(|(ty, val)| (ty, val, self.location)),
            ))
        }

        None
    }
}
