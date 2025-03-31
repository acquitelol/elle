use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        enums::{Instruction, Type, Value},
    },
    parser::enums::Ternary,
};

impl Codegen<'_> for Ternary {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let temp = gen.new_temporary(Some("ternary"), false);

        let true_label = format!("ift.{}", gen.tmp_counter);
        let false_label = format!("iff.{}", gen.tmp_counter);
        let end_label = format!("end.{}", gen.tmp_counter);

        let (_, condition_val) = gen
            .generate_statement(ctx.func, ctx.module, *self.condition, None, None, false)
            .expect(
                &self
                    .location
                    .error("Unexpected error when trying to compile the `condition` of a ternary"),
            );

        ctx.func
            .borrow_mut()
            .add_instruction(Instruction::JumpNonZero(
                condition_val,
                true_label.clone(),
                false_label.clone(),
            ));

        ctx.func.borrow_mut().add_block(true_label);

        let (if_true_ty, if_true_val) = gen
            .generate_statement(
                ctx.func,
                ctx.module,
                *self.if_true,
                None,
                None,
                ctx.is_return,
            )
            .expect(
                &self
                    .location
                    .error("Unexpected error when trying to compile the `true` path of a ternary"),
            );

        ctx.func.borrow_mut().assign_instruction(
            &temp,
            &if_true_ty,
            Instruction::Copy(if_true_val),
        );

        ctx.func
            .borrow_mut()
            .add_instruction(Instruction::Jump(end_label.clone()));

        ctx.func.borrow_mut().add_block(false_label);

        let (if_false_ty, if_false_val) = gen
            .generate_statement(
                ctx.func,
                ctx.module,
                *self.if_false,
                None,
                None,
                ctx.is_return,
            )
            .expect(
                &self
                    .location
                    .error("Unexpected error when trying to compile the `false` path of a ternary"),
            );

        ctx.func.borrow_mut().assign_instruction(
            &temp,
            &if_false_ty,
            Instruction::Copy(if_false_val),
        );

        ctx.func
            .borrow_mut()
            .add_instruction(Instruction::Jump(end_label.clone()));

        ctx.func.borrow_mut().add_block(end_label);
        Some((if_true_ty, temp))
    }
}
