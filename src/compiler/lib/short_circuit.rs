use std::cell::RefCell;

use crate::{
    compiler::{
        compiler::{CodegenContext, Compiler},
        qbe::{
            comparison::Comparison, function::Function, instruction::Instruction, module::Module,
            r#type::Type, value::Value,
        },
    },
    elle_error,
    lexer::enums::{Location, MutRc, TokenKind},
    parser::enums::AstNode,
};

use super::weighted_cast::handle_weighted_cast;

pub fn handle_short_circuiting_operation(
    gen: &mut Compiler,
    left: AstNode,
    right: AstNode,
    func: &RefCell<Function>,
    module: &RefCell<Module>,
    ty: Option<Type>,
    is_return: bool,
    location: &MutRc<Location>,
    kind: TokenKind,
) -> (Type, Value) {
    gen.tmp_counter += 1;

    let left_label = format!("{}.left.{}", kind, gen.tmp_counter);
    let right_label = format!("{}.right.{}", kind, gen.tmp_counter);
    let conv_label = format!("{}.match.{}", kind, gen.tmp_counter);
    let left_matches_label = format!("{}.left.match.{}", kind, gen.tmp_counter);
    let right_matches_label = format!("{}.right.match.{}", kind, gen.tmp_counter);
    let end_label = format!("{}.end.{}", kind, gen.tmp_counter);

    let (mut left_ty, left_val) = left
        .compile(
            gen,
            &CodegenContext {
                func,
                module,
                ty: ty.clone(),
                value: None,
                is_return,
                is_generic: false,
            },
        )
        .unwrap_or_else(|| {
            elle_error!(location.borrow().error(
                "Unexpected error when trying to parse left side of an arithmetic operation",
            ))
        });

    func.borrow_mut().add_block(left_label.clone());

    let left_tmp = gen.new_temporary(Some(&format!("{kind}.left")), true);

    func.borrow_mut().assign_instruction(
        &left_tmp,
        &Type::Boolean,
        Instruction::Compare(
            left_ty.clone(),
            Comparison::Equal,
            left_val.clone(),
            Value::Const(String::new(), 0),
        ),
    );

    func.borrow_mut().add_block(format!("{left_label}.jmp"));

    let mut left_tmp_jmp = gen.new_temporary(Some(&kind.to_string()), true);

    func.borrow_mut().assign_instruction(
        &left_tmp_jmp,
        &left_ty,
        Instruction::Copy(left_val.clone()),
    );

    match kind {
        TokenKind::And => {
            func.borrow_mut().add_instruction(Instruction::JumpNonZero(
                left_tmp,
                end_label.clone(),
                conv_label.clone(),
            ));
        }
        TokenKind::Or => {
            func.borrow_mut().add_instruction(Instruction::JumpNonZero(
                left_tmp,
                right_label.clone(),
                conv_label.clone(),
            ));
        }
        other => elle_error!(location.borrow().error(format!(
            "Invalid operator token for conditional short circuiting '{other}'",
        ))),
    }

    func.borrow_mut().add_block(right_label.clone());

    let (mut right_ty, right_val) = right
        .compile(
            gen,
            &CodegenContext {
                func,
                module,
                ty,
                value: None,
                is_return,
                is_generic: false,
            },
        )
        .unwrap_or_else(|| {
            elle_error!(location.borrow().error(
                "Unexpected error when trying to parse right side of an arithmetic operation",
            ))
        });

    let right_tmp = gen.new_temporary(Some(&format!("{kind}.right")), true);

    func.borrow_mut().assign_instruction(
        &right_tmp,
        &Type::Boolean,
        Instruction::Compare(
            right_ty.clone(),
            Comparison::Equal,
            right_val.clone(),
            Value::Const(String::new(), 0),
        ),
    );

    func.borrow_mut().add_block(format!("{right_label}.jmp"));

    let mut right_tmp_jmp = gen.new_temporary(Some(&kind.to_string()), true);

    func.borrow_mut().assign_instruction(
        &right_tmp_jmp,
        &right_ty,
        Instruction::Copy(right_val.clone()),
    );

    // This is the same for AND and OR
    func.borrow_mut().add_instruction(Instruction::JumpNonZero(
        right_tmp,
        end_label.clone(),
        conv_label.clone(),
    ));

    func.borrow_mut().add_block(conv_label);

    let phi_tmp = gen.new_temporary(None, false);

    func.borrow_mut().assign_instruction(
        &phi_tmp,
        &Type::Boolean,
        Instruction::Phi(vec![
            (format!("{left_label}.jmp"), Value::Const(String::new(), 0)),
            (format!("{right_label}.jmp"), Value::Const(String::new(), 1)),
        ]),
    );

    handle_weighted_cast(
        gen,
        func,
        &mut left_ty,
        &mut left_tmp_jmp,
        &mut right_ty,
        &mut right_tmp_jmp,
        &location,
    );

    func.borrow_mut().add_instruction(Instruction::JumpNonZero(
        phi_tmp,
        right_matches_label.clone(),
        match kind {
            TokenKind::And => right_label.clone(),
            TokenKind::Or => left_matches_label.clone(),
            other => elle_error!(location.borrow().error(format!(
                "Invalid operator token for conditional short circuiting '{other}'",
            ))),
        },
    ));

    func.borrow_mut().add_block(left_matches_label.clone());

    let left_tmp_match = gen.new_temporary(Some(&kind.to_string()), true);

    func.borrow_mut().assign_instruction(
        &left_tmp_match,
        &left_ty,
        Instruction::Copy(left_tmp_jmp.clone()),
    );

    func.borrow_mut()
        .add_instruction(Instruction::Jump(end_label.clone()));

    func.borrow_mut().add_block(right_matches_label.clone());

    let right_tmp_match = gen.new_temporary(Some(&kind.to_string()), true);

    func.borrow_mut().assign_instruction(
        &right_tmp_match,
        &left_ty,
        Instruction::Copy(right_tmp_jmp),
    );

    func.borrow_mut()
        .add_instruction(Instruction::Jump(end_label.clone()));

    func.borrow_mut().add_block(end_label);

    let res_tmp = gen.new_temporary(None, false);
    let prefix = match left_ty {
        Type::Double => "d_",
        Type::Single => "s_",
        _ => "",
    }
    .to_string();

    let mut predecessors = vec![
        (
            format!("{right_label}.jmp"),
            Value::Const(prefix.clone(), 0),
        ),
        (left_matches_label, left_tmp_match.clone()),
        (right_matches_label, right_tmp_match.clone()),
    ];

    if kind == TokenKind::And {
        predecessors.insert(0, (format!("{left_label}.jmp"), Value::Const(prefix, 0)));
    }

    func.borrow_mut()
        .assign_instruction(&res_tmp, &left_ty, Instruction::Phi(predecessors));

    (left_ty, res_tmp)
}
