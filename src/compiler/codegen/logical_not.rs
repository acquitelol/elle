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
            elle_error!(&self.location.error(
                "Unexpected error when trying to compile the value of a `logical not` expression"
            ))
        });

        let temp = gen.new_temporary(Some("not"), true);

        ctx.func.borrow_mut().assign_instruction(
            &temp,
            &Type::Boolean,
            Instruction::Compare(
                Type::Boolean,
                Comparison::Equal,
                val,
                Value::Const(
                    if ty.clone() == Type::Double {
                        "d_"
                    } else if ty.clone() == Type::Single {
                        "s_"
                    } else {
                        ""
                    }
                    .into(),
                    0,
                ),
            ),
        );

        Some((ty, temp))
    }
}
