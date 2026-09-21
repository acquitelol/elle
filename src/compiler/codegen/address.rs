use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        qbe::{instruction::Instruction, r#type::Type, value::Value},
    },
    elle_error,
    parser::enums::Address,
};

impl Codegen<'_> for Address {
    fn compile(self, compiler: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let (ty, val) = self.value.compile(compiler, ctx).unwrap_or_else(|| {
            elle_error!(self.location.borrow().error(
                "Unexpected error when trying to compile the value of an address expression",
            ))
        });

        if ty.is_struct() || ty.is_static_array() {
            return Some((Type::Pointer(Box::new(ty)), val));
        }

        if let Some(addr_val) = compiler.address_pool.get(&val) {
            Some((Type::Pointer(Box::new(ty)), addr_val.clone()))
        } else {
            let addr_val = compiler.new_temporary(Some("tmp.addr"), true);
            let addr_ty = Type::Pointer(Box::new(ty.clone()));

            ctx.func.borrow_mut().assign_instruction_front(
                &addr_val,
                &addr_ty,
                Instruction::Alloc8(Value::Const(String::new(), i128::from(ty.size(ctx.module)))),
            );

            ctx.func.borrow_mut().add_instruction(Instruction::Store(
                ty,
                addr_val.clone(),
                val,
            ));

            Some((addr_ty, addr_val))
        }
    }
}
