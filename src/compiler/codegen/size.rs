use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        qbe::{instruction::Instruction, r#type::Type, value::Value},
    },
    elle_error,
    parser::enums::Size,
};

impl Codegen<'_> for Size {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        match self.value {
            Ok(ty) => {
                let tmp_ty = Type::UnsignedLong;
                let temp = gen.new_temporary(Some("size"), true);

                ctx.func.borrow_mut().assign_instruction(
                    &temp,
                    &tmp_ty,
                    Instruction::Copy(Value::Const(String::new(), i128::from(ty.size(ctx.module)))),
                );

                Some((tmp_ty, temp))
            }

            Err(value) => {
                let (_, val) =
                    value.compile(gen, ctx).unwrap_or_else(|| {
                        elle_error!(self.location.borrow().error(
                            "Unexpected error when trying to compile the size of an expression",
                        ))
                    });

                let size = gen.new_temporary(Some("size"), true);
                let ty = Type::UnsignedLong;

                if ty.is_pointer()
                    && let Some((_, buf_val)) = gen.buf_metadata.get(&val)
                {
                    ctx.func.borrow_mut().assign_instruction(
                        &size,
                        &ty,
                        Instruction::Copy(buf_val.clone()),
                    );

                    return Some((ty, size));
                }

                ctx.func.borrow_mut().assign_instruction(
                    &size,
                    &ty,
                    Instruction::Copy(Value::Const(String::new(), i128::from(ty.size(ctx.module)))),
                );

                Some((ty, size))
            }
        }
    }
}
