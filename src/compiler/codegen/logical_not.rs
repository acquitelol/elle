use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        qbe::{comparison::Comparison, instruction::Instruction, r#type::Type, value::Value},
    },
    elle_error,
    parser::enums::LogicalNot,
};

impl Codegen<'_> for LogicalNot {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let (ty, val) = self.value.compile(gen, ctx).unwrap_or_else(|| {
            elle_error!(&self.location.borrow().error(
                "Unexpected error when trying to compile the value of a `logical not` expression"
            ))
        });

        let temp = gen.new_temporary(Some("not"), true);
        let return_ty = Type::Boolean;

        ctx.func.borrow_mut().assign_instruction(
            &temp,
            &return_ty,
            Instruction::Compare(
                Type::Boolean,
                Comparison::Equal,
                val,
                Value::Const(
                    if ty == Type::Double {
                        "d_"
                    } else if ty == Type::Single {
                        "s_"
                    } else {
                        ""
                    }
                    .into(),
                    0,
                ),
            ),
        );

        Some((return_ty, temp))
    }
}
