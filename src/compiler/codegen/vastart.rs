use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        qbe::{instruction::Instruction, r#type::Type, value::Value},
    },
    elle_error,
    parser::enums::VariadicStart,
    VA_LIST_SIZE_BYTES,
};

impl Codegen<'_> for VariadicStart {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let plain_name = self.name.value.get_string_inner().unwrap();
        let ty = Type::Pointer(Box::new(Type::Void));
        let val = gen.new_variable(&ty, &plain_name, Some(ctx.func), false, false);

        ctx.func.borrow_mut().assign_instruction(
            &val,
            &ty,
            Instruction::Alloc8(Value::Const("".into(), VA_LIST_SIZE_BYTES as i128)),
        );

        ctx.func
            .borrow_mut()
            .add_instruction(Instruction::VAStart(val.clone()));

        let res = (ty, val);

        if self.name.tagged {
            elle_error!(format!(
                "hover\n{}\n{}\nlet {plain_name}: {}",
                self.name.location.display_plain(false),
                self.name.location.display_plain(true),
                res.0.display()
            ));
        }

        Some(res)
    }
}
