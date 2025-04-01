use std::{cell::RefCell, rc::Rc};

use crate::{
    compiler::{
        compiler::{CodegenContext, Compiler},
        enums::{Function, Module, Type, Value},
        qbe::{comparison::Comparison, instruction::Instruction},
    },
    elle_error,
    lexer::enums::{Location, TokenKind},
    parser::enums::AstNode,
};

pub fn handle_short_circuiting_operation(
    gen: &mut Compiler,
    left: Box<AstNode>,
    right: Box<AstNode>,
    func: &RefCell<Function>,
    module: &RefCell<Module>,
    ty: Option<Type>,
    is_return: bool,
    location: Rc<Location>,
    kind: TokenKind,
) -> (Type, Value) {
    gen.tmp_counter += 1;

    let left_label = format!("{}.left.{}", kind, gen.tmp_counter);
    let right_label = format!("{}.right.{}", kind, gen.tmp_counter);
    let left_matches_label = format!("{}.left.match.{}", kind, gen.tmp_counter);
    let right_matches_label = format!("{}.right.match.{}", kind, gen.tmp_counter);
    let end_label = format!("{}.end.{}", kind, gen.tmp_counter);

    let result_tmp = gen.new_temporary(Some(&kind.to_string()), true);

    let (left_ty, left_val) = left
        .compile(
            gen,
            &CodegenContext {
                func,
                module,
                ty: ty.clone(),
                value: None,
                is_return,
            },
        )
        .expect(
            &location.error(
                "Unexpected error when trying to parse left side of an arithmetic operation",
            ),
        );

    func.borrow_mut().assign_instruction(
        &result_tmp,
        &left_ty,
        Instruction::Copy(Value::Const(
            if left_ty.clone() == Type::Double {
                "d_"
            } else if left_ty.clone() == Type::Single {
                "s_"
            } else {
                ""
            }
            .into(),
            0,
        )),
    );

    func.borrow_mut().add_block(left_label);

    let left_tmp = gen.new_temporary(Some(&format!("{}.left", kind)), true);

    func.borrow_mut().assign_instruction(
        &left_tmp,
        &Type::Boolean,
        Instruction::Compare(
            Type::Boolean,
            Comparison::Equal,
            left_val.clone(),
            Value::Const("".into(), 0),
        ),
    );

    match kind {
        TokenKind::And => {
            func.borrow_mut().add_instruction(Instruction::JumpNonZero(
                left_tmp,
                end_label.clone(),
                right_label.clone(),
            ));
        }
        TokenKind::Or => {
            func.borrow_mut().add_instruction(Instruction::JumpNonZero(
                left_tmp,
                right_label.clone(),
                left_matches_label.clone(),
            ));
        }
        other => elle_error!(location.error(format!(
            "Invalid operator token for conditional short circuiting '{}'",
            other
        ))),
    }

    func.borrow_mut().add_block(right_label);

    let (_, right_val) =
        (*right)
            .compile(
                gen,
                &CodegenContext {
                    func,
                    module,
                    ty,
                    value: None,
                    is_return,
                },
            )
            .expect(&location.error(
                "Unexpected error when trying to parse right side of an arithmetic operation",
            ));

    let right_tmp = gen.new_temporary(Some(&format!("{}.right", kind)), true);

    func.borrow_mut().assign_instruction(
        &right_tmp,
        &Type::Boolean,
        Instruction::Compare(
            Type::Boolean,
            Comparison::Equal,
            right_val.clone(),
            Value::Const("".into(), 0),
        ),
    );

    // This is the same for AND and OR
    func.borrow_mut().add_instruction(Instruction::JumpNonZero(
        right_tmp,
        end_label.clone(),
        right_matches_label.clone(),
    ));

    func.borrow_mut().add_block(left_matches_label);

    func.borrow_mut()
        .assign_instruction(&result_tmp, &left_ty, Instruction::Copy(left_val));

    func.borrow_mut()
        .add_instruction(Instruction::Jump(end_label.clone()));

    func.borrow_mut().add_block(right_matches_label);

    func.borrow_mut()
        .assign_instruction(&result_tmp, &left_ty, Instruction::Copy(right_val));

    func.borrow_mut()
        .add_instruction(Instruction::Jump(end_label.clone()));

    func.borrow_mut().add_block(end_label);
    return (left_ty, result_tmp);
}
