use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        lib::weighted_cast::handle_weighted_cast,
        qbe::{instruction::Instruction, r#type::Type, value::Value},
    },
    elle_error,
    parser::enums::Ternary,
};

impl Codegen<'_> for Ternary {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        gen.tmp_counter += 1;
        let true_label = format!("ift.{}", gen.tmp_counter);
        let false_label = format!("iff.{}", gen.tmp_counter);
        let conv_label = format!("conv.{}", gen.tmp_counter);
        let end_label = format!("end.{}", gen.tmp_counter);
        let matches_true_label = format!("ift.match.{}", gen.tmp_counter);
        let matches_false_label = format!("iff.match.{}", gen.tmp_counter);

        let (_, cond_val) =
            self.condition
                .compile(gen, &ctx.to_nnf())
                .unwrap_or_else(|| {
                    elle_error!(self.location.borrow().error(
                        "Unexpected error when trying to compile the `condition` of a ternary"
                    ))
                });

        ctx.func
            .borrow_mut()
            .add_instruction(Instruction::JumpNonZero(
                cond_val,
                true_label.clone(),
                false_label.clone(),
            ));

        ctx.func.borrow_mut().add_block(true_label.clone());

        let (mut if_true_ty, mut if_true_val) =
            self.if_true.compile(gen, ctx).unwrap_or_else(|| {
                elle_error!(self
                    .location
                    .borrow()
                    .error("Unexpected error when trying to compile the `true` path of a ternary"))
            });

        ctx.func.borrow_mut().add_block(format!("{true_label}.jmp"));

        ctx.func
            .borrow_mut()
            .add_instruction(Instruction::Jump(conv_label.clone()));

        ctx.func.borrow_mut().add_block(false_label.clone());

        let (mut if_false_ty, mut if_false_val) =
            self.if_false.compile(gen, ctx).unwrap_or_else(|| {
                elle_error!(self
                    .location
                    .borrow()
                    .error("Unexpected error when trying to compile the `false` path of a ternary"))
            });

        ctx.func
            .borrow_mut()
            .add_block(format!("{false_label}.jmp"));

        ctx.func
            .borrow_mut()
            .add_instruction(Instruction::Jump(conv_label.clone()));

        ctx.func.borrow_mut().add_block(conv_label);

        let phi_tmp = gen.new_temporary(None, false);

        ctx.func.borrow_mut().assign_instruction(
            &phi_tmp,
            &Type::Boolean,
            Instruction::Phi(vec![
                (format!("{true_label}.jmp"), Value::Const(String::new(), 1)),
                (format!("{false_label}.jmp"), Value::Const(String::new(), 0)),
            ]),
        );

        handle_weighted_cast(
            gen,
            ctx.func,
            &mut if_true_ty,
            &mut if_true_val,
            &mut if_false_ty,
            &mut if_false_val,
            &self.location,
        );

        ctx.func
            .borrow_mut()
            .add_instruction(Instruction::JumpNonZero(
                phi_tmp,
                matches_true_label.clone(),
                matches_false_label.clone(),
            ));

        ctx.func.borrow_mut().add_block(matches_true_label.clone());

        let if_true_tmp = gen.new_temporary(None, false);

        ctx.func.borrow_mut().assign_instruction(
            &if_true_tmp,
            &if_true_ty,
            Instruction::Copy(if_true_val),
        );

        ctx.func
            .borrow_mut()
            .add_instruction(Instruction::Jump(end_label.clone()));

        ctx.func.borrow_mut().add_block(matches_false_label.clone());

        let if_false_tmp = gen.new_temporary(None, false);

        ctx.func.borrow_mut().assign_instruction(
            &if_false_tmp,
            &if_false_ty,
            Instruction::Copy(if_false_val),
        );

        ctx.func
            .borrow_mut()
            .add_instruction(Instruction::Jump(end_label.clone()));

        ctx.func.borrow_mut().add_block(end_label);

        let res_tmp = gen.new_temporary(None, false);

        ctx.func.borrow_mut().assign_instruction(
            &res_tmp,
            &if_true_ty,
            Instruction::Phi(vec![
                (matches_true_label, if_true_tmp),
                (matches_false_label, if_false_tmp),
            ]),
        );

        Some((if_true_ty, res_tmp))
    }
}
