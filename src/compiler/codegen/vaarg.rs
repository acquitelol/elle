use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        qbe::{instruction::Instruction, r#type::Type, value::Value},
    },
    parser::enums::VariadicArgument,
};

impl Codegen<'_> for VariadicArgument {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let ptr = gen
            .get_variable_lazy(
                &self.name,
                Some(ctx.func),
                Some(ctx.module),
                self.location.clone(),
            )
            .expect(&self.location.error(format!(
                "Unexpected error when trying to get a variable named '{}'",
                self.name
            )))
            .1;

        let ty = self.r#type.unwrap_or(Type::Long);
        let tmp = gen.new_temporary(Some("next"), true);

        ctx.func.borrow_mut().assign_instruction(
            &tmp,
            &ty.clone().into_base(),
            Instruction::VAArg(ptr),
        );

        Some((ty, tmp))
    }
}
