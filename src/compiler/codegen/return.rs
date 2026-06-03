use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        lib::convert::convert_to_type,
        qbe::{instruction::Instruction, r#type::Type, value::Value},
    },
    parser::enums::Return,
};

impl Codegen<'_> for Return {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let mut res = self.value.compile(
            gen,
            &CodegenContext {
                is_return: true,
                ..ctx.clone()
            },
        );

        let ret_ty = ctx.func.borrow().return_type.clone();
        if let Some((ref ty, ref val)) = res
            && let Some(ret_ty) = ret_ty
        {
            res.replace(convert_to_type(
                gen,
                ctx.func,
                ty.clone(),
                ret_ty.clone(),
                val.clone(),
                &self.location,
                &self.location,
                false,
            ));
        }

        ctx.func.borrow_mut().add_instruction(Instruction::Return(
            res.map(|(ty, val)| (ty, val, self.location)),
        ));

        None
    }
}
