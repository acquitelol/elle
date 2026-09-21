use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        qbe::{instruction::Instruction, r#type::Type, value::Value},
    },
    elle_error,
    parser::enums::BitwiseNot,
};

impl Codegen<'_> for BitwiseNot {
    fn compile(self, compiler: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let (ty, val) = self.value.compile(compiler, ctx).unwrap_or_else(|| {
            elle_error!(self.location.borrow().error(
                "Unexpected error when trying to compile the value of a `bitwise not` expression",
            ))
        });

        let temp = compiler.new_temporary(Some("negate"), true);

        ctx.func.borrow_mut().assign_instruction(
            &temp,
            &ty,
            if ty.is_float() {
                Instruction::Negate(val)
            } else {
                Instruction::BitwiseNot(val)
            },
        );

        Some((ty, temp))
    }
}
