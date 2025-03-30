use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        enums::{Instruction, Type, Value},
    },
    parser::enums::Return,
};

impl Codegen<'_> for Return {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        match gen.generate_statement(
            ctx.func,
            ctx.module,
            *self.value,
            ctx.ty.clone(),
            None,
            true,
        ) {
            Some((ret_ty, value)) => {
                if !ctx.func.borrow_mut().manual {
                    ctx.func
                        .borrow_mut()
                        .add_instruction(Instruction::Return(Some((ret_ty, value, self.location))))
                }
            }
            None => {
                if !ctx.func.borrow_mut().manual {
                    ctx.func
                        .borrow_mut()
                        .add_instruction(Instruction::Return(None))
                }
            }
        }

        None
    }
}
