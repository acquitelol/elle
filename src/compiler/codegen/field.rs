use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        enums::{Instruction, Type, Value},
    },
    parser::enums::FieldAccess,
};

impl Codegen<'_> for FieldAccess {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let (ty, left) = gen
            .generate_statement(
                ctx.func,
                ctx.module,
                *self.left,
                ctx.ty.clone(),
                None,
                false,
            )
            .expect(&self.location.error(
                "Unexpected error when trying to compile the left side of a struct field access",
            ));

        let (field_ty, offset_tmp) = gen.process_field_access(
            ctx.func,
            ctx.module,
            ty,
            left,
            *self.right,
            false,
            &self.location,
        );

        if let Some(value) = self.value {
            let (_, compiled) = gen
                .generate_statement(
                    ctx.func,
                    ctx.module,
                    *value,
                    Some(field_ty.clone()),
                    None,
                    false,
                )
                .expect(&self.location.error(
                    "Unexpected error when trying to compile the value of a store statement",
                ));

            ctx.func.borrow_mut().add_instruction(Instruction::Store(
                field_ty.clone(),
                offset_tmp.clone(),
                compiled,
            ));

            return Some((field_ty, offset_tmp));
        }

        let temp = gen.new_temporary(Some("field"), true);

        // Structs are stored in contiguous memory.
        // Any field that is a struct should not be dereferenced
        // because that will break everything.
        if field_ty.is_struct() {
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
