use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        enums::{Comparison, Instruction, Type, Value},
    },
    parser::enums::LogicalNot,
};

impl Codegen<'_> for LogicalNot {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let (ty, val) = gen
            .generate_statement(
                ctx.func,
                ctx.module,
                *self.value,
                ctx.ty.clone(),
                None,
                false,
            )
            .expect(
                &self
                    .location
                    .error("Unexpected error when trying to compile the value of a not statement"),
            );

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
