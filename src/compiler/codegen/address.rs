use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        qbe::{instruction::Instruction, r#type::Type, value::Value},
    },
    elle_error,
    parser::enums::Address,
};

impl Codegen<'_> for Address {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let (ty, val) = self.value.compile(gen, ctx).unwrap_or_else(|| {
            elle_error!(self.location.borrow().error(
                "Unexpected error when trying to compile the value of an address expression",
            ))
        });

        if ty.is_struct() {
            return Some((Type::Pointer(Box::new(ty)), val));
        }

        if let Some(addr_val) = gen.address_pool.get(&val) {
            Some((Type::Pointer(Box::new(ty)), addr_val.clone()))
        } else {
            let addr_val = gen.new_temporary(Some("tmp.addr"), true);
            let addr_ty = Type::Pointer(Box::new(ty.clone()));

            ctx.func.borrow_mut().assign_instruction_front(
                &addr_val,
                &addr_ty,
                Instruction::Alloc8(Value::Const(String::new(), i128::from(ty.size(ctx.module)))),
            );

            ctx.func.borrow_mut().add_instruction(Instruction::Store(
                ty,
                addr_val.clone(),
                val.clone(),
            ));

            Some((addr_ty, addr_val))
        }
    }
}
