use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        qbe::{instruction::Instruction, r#type::Type, value::Value},
    },
    elle_error,
    parser::enums::VariadicArgument,
};

impl Codegen<'_> for VariadicArgument {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let plain_name = self.name.value.get_string_inner().unwrap();

        let ptr = gen
            .get_variable_lazy(
                &plain_name,
                Some(ctx.func),
                Some(ctx.module),
                self.location.clone(),
            )
            .unwrap_or_else(|| {
                elle_error!(self.location.error(format!(
                    "Unexpected error when trying to get a variable named '{}'",
                    plain_name
                )))
            })
            .1;

        let ty = self.r#type.unwrap_or(Type::Pointer(Box::new(Type::Void)));
        let tmp = gen.new_temporary(Some("next"), true);

        ctx.func.borrow_mut().assign_instruction(
            &tmp,
            &ty.clone().into_base(),
            Instruction::VAArg(ptr),
        );

        let res = (ty, tmp);

        if self.name.tagged {
            elle_error!(format!(
                "hover\n{}\n{}\nlet {plain_name}: {}",
                self.name.location.display_plain(false),
                self.name.location.display_plain(true),
                // This is the vararg object
                // NOT the value itself
                Type::Pointer(Box::new(Type::Void)).display()
            ));
        }

        Some(res)
    }
}
