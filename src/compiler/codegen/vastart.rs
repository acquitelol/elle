use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        qbe::{instruction::Instruction, r#type::Type, value::Value},
    },
    parser::enums::VariadicStart,
    VA_LIST_SIZE_BYTES,
};

impl Codegen<'_> for VariadicStart {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let ty = Type::Long;
        let val = gen.new_variable(&ty, &self.name, Some(ctx.func), false, false);

        ctx.func.borrow_mut().assign_instruction(
            &val,
            &ty,
            Instruction::Alloc8(Value::Const("".into(), VA_LIST_SIZE_BYTES as i128)),
        );

        ctx.func
            .borrow_mut()
            .add_instruction(Instruction::VAStart(val.clone()));

        Some((ty, val))
    }
}
