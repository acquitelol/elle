use std::{cell::RefCell, rc::Rc};

use crate::{
    compiler::{
        compiler::Compiler,
        qbe::{
            function::Function, instruction::Instruction, module::Module, r#type::Type,
            value::Value,
        },
    },
    elle_error,
    lexer::enums::{Location, TokenKind},
    parser::enums::{AstNode, FieldAccess, Literal},
    unknown_field,
};

pub fn member_to_offset(
    gen: &Compiler,
    module: &RefCell<Module>,
    struct_name: &String,
    member_name: &String,
) -> Option<(Option<Type>, u64)> {
    match gen.struct_pool.get(struct_name) {
        Some((_, members, ..)) => {
            if !members.iter().any(|member| &member.name == member_name) {
                return None;
            }

            let mut offset = 0_u64;
            let mut ty = None;

            for member in members.iter() {
                if &member.name == member_name {
                    ty = Some(member.r#type.clone());
                    break;
                }

                offset += member.r#type.size(module)
            }

            Some((ty, offset))
        }
        _ => None,
    }
}

pub fn process_field_access(
    gen: &mut Compiler,
    func: &RefCell<Function>,
    module: &RefCell<Module>,
    mut ty: Type,
    mut left: Value,
    mut right: AstNode,
    load: bool,
    location: &Rc<Location>,
) -> (Type, Value) {
    loop {
        match right.clone() {
            AstNode::Literal(Literal {
                kind,
                value,
                location,
            }) if kind == TokenKind::Identifier => {
                let field = value.get_string_inner().unwrap();

                if !ty.is_struct() {
                    // Automatically deref 'Foo *' into 'Foo' when processing
                    if ty.is_pointer() && ty.get_pointer_inner().unwrap().is_struct() {
                        ty = ty.get_pointer_inner().unwrap();
                    } else {
                        elle_error!(&location.error(format!(
                            "Cannot access fields on a non-struct type '{}' (field '{}')",
                            ty.display(),
                            field
                        )));
                    }
                }

                let struct_name = ty.get_struct_inner().unwrap();

                let (member_ty, offset) = member_to_offset(gen, module, &struct_name, &field)
                    .unwrap_or_else(|| {
                        elle_error!(unknown_field!(
                            gen.struct_pool.get(&struct_name).unwrap(),
                            ty,
                            field,
                            location
                        ))
                    });

                let offset_tmp = gen.new_temporary(Some("offset"), true);

                func.borrow_mut().assign_instruction(
                    &offset_tmp,
                    &Type::Long,
                    Instruction::Add(left, Value::Const("".into(), offset as i128)),
                );

                if load && !member_ty.clone().unwrap().is_struct() {
                    let tmp = gen.new_temporary(Some("load"), true);

                    func.borrow_mut().assign_instruction(
                        &tmp,
                        &Type::Long,
                        Instruction::Load(member_ty.clone().unwrap(), offset_tmp),
                    );

                    return (member_ty.unwrap(), tmp);
                } else {
                    return (member_ty.unwrap(), offset_tmp);
                }
            }
            AstNode::FieldAccess(FieldAccess {
                left: nested_left,
                right: nested_right,
                ..
            }) => {
                let (nested_ty, nested_left_value) =
                    process_field_access(gen, func, module, ty, left, *nested_left, true, location);

                ty = nested_ty;
                left = nested_left_value;
                right = *nested_right;
            }
            _ => elle_error!(location.error(format!(
                "Unexpected AST node type for field access: {:?}",
                right
            ))),
        }
    }
}
