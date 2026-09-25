use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        qbe::{instruction::Instruction, r#type::Type, value::Value},
    },
    elle_error,
    parser::enums::VariadicArgument,
};

impl Codegen<'_> for VariadicArgument {
    fn compile(self, compiler: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let plain_name = self.name.value.get_string_inner().unwrap();

        let (_, ptr) = compiler.get_variable_lazy(
            &plain_name,
            Some(ctx.func),
            Some(ctx.module),
            &self.location,
        );

        let ty = self
            .r#type
            .unwrap_or_else(|| Type::Pointer(Box::new(Type::Void)));
        let tmp = compiler.new_temporary(Some("next"), true);

        ctx.func
            .borrow_mut()
            .assign_instruction(&tmp, &ty, Instruction::VAArg(ptr));

        let res = (ty, tmp);

        if self.name.tagged {
            elle_error!(format!(
                "hover\n{}\n{}\nlet {plain_name}: {}",
                self.name.location.borrow().display_plain(false),
                self.name.location.borrow().display_plain(true),
                // This is the vararg object
                // NOT the value itself
                Type::Pointer(Box::new(Type::Void)).display()
            ));
        }

        Some(res)
    }
}
