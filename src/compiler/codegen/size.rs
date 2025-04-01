use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        qbe::{instruction::Instruction, r#type::Type, value::Value},
    },
    parser::enums::Size,
};

impl Codegen<'_> for Size {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        match self.value {
            Ok(ty) => {
                let tmp_ty = Type::Long;
                let temp = gen.new_temporary(Some("size"), true);

                ctx.func.borrow_mut().assign_instruction(
                    &temp,
                    &tmp_ty,
                    Instruction::Copy(Value::Const("".into(), ty.size(ctx.module) as i128)),
                );

                Some((tmp_ty, temp))
            }

            Err(value) => {
                let (ty, val) = value.compile(gen, ctx).expect(
                    &self
                        .location
                        .error("Unexpected error when trying to compile the size of a statement"),
                );

                let size = gen.new_temporary(Some("size"), true);

                match &ty {
                    &Type::Pointer(_) => {
                        let ty = Type::Long;

                        if let Some((_, buf_val)) = gen.buf_metadata.get(&val).cloned() {
                            ctx.func.borrow_mut().assign_instruction(
                                &size,
                                &ty,
                                Instruction::Copy(buf_val),
                            );

                            return Some((ty, size));
                        }

                        ctx.func.borrow_mut().assign_instruction(
                            &size,
                            &ty,
                            Instruction::Copy(Value::Const("".into(), ty.size(ctx.module) as i128)),
                        );

                        Some((ty, size))
                    }
                    other => {
                        ctx.func.borrow_mut().assign_instruction(
                            &size,
                            &other,
                            Instruction::Copy(Value::Const(
                                if other.clone() == Type::Double {
                                    "d_"
                                } else if other.clone() == Type::Single {
                                    "s_"
                                } else {
                                    ""
                                }
                                .into(),
                                ty.size(ctx.module) as i128,
                            )),
                        );

                        Some((other.to_owned(), size))
                    }
                }
            }
        }
    }
}
