use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        lib::{convert::convert_to_type, field_utils::process_field_access},
        qbe::{instruction::Instruction, r#type::Type, value::Value},
    },
    elle_error,
    parser::enums::FieldAccess,
};

impl Codegen<'_> for FieldAccess {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let (ty, left) = self.left.compile(gen, ctx).unwrap_or_else(|| {
            elle_error!(self.location.borrow().error(
                "Unexpected error when trying to compile the left side of a struct field access",
            ))
        });

        let (field_ty, offset_tmp) = process_field_access(
            gen,
            ctx.func,
            ctx.module,
            ty,
            left,
            *self.right,
            false,
            self.addr_only,
            &self.location,
        );

        if let Some(value) = self.value {
            let (ty, compiled) = value
                .compile(
                    gen,
                    &CodegenContext {
                        ty: Some(field_ty.clone()),
                        ..ctx.clone()
                    },
                )
                .unwrap_or_else(|| {
                    elle_error!(self.location.borrow().error(
                        "Unexpected error when trying to compile the value of a store statement",
                    ))
                });

            let (final_ty, final_val) = convert_to_type(
                gen,
                ctx.func,
                ty,
                field_ty,
                compiled,
                &self.location,
                &self.location,
                false,
            );

            if final_ty.is_struct() {
                ctx.func.borrow_mut().add_instruction(Instruction::Blit(
                    final_val,
                    offset_tmp.clone(),
                    final_ty.size(ctx.module),
                ));
            } else {
                ctx.func.borrow_mut().add_instruction(Instruction::Store(
                    final_ty.clone(),
                    offset_tmp.clone(),
                    final_val,
                ));
            }

            return Some((final_ty, offset_tmp));
        }

        let temp = gen.new_temporary(Some("field"), true);

        // Structs are stored in contiguous memory.
        // Any field that is a struct should not be dereferenced
        // because that will break everything.
        if field_ty.is_struct() || self.addr_only {
            Some((field_ty, offset_tmp))
        } else {
            ctx.func.borrow_mut().assign_instruction(
                &temp,
                &field_ty,
                Instruction::Load(field_ty.clone(), offset_tmp),
            );

            Some((field_ty, temp))
        }
    }
}
