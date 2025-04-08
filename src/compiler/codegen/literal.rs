use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        qbe::{
            data::Data, data_item::DataItem, instruction::Instruction, linkage::Linkage,
            r#type::Type, value::Value,
        },
    },
    elle_error,
    lexer::enums::{TokenKind, ValueKind},
    parser::enums::Literal,
};

impl Codegen<'_> for Literal {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        match self.kind {
            TokenKind::Identifier => match self.value {
                ValueKind::String(name) => gen.get_variable_lazy(
                    &name,
                    Some(ctx.func),
                    Some(ctx.module),
                    self.location.clone(),
                ),
                _ => None,
            },
            TokenKind::Break => {
                if let Some(label) = &gen.loop_labels.last() {
                    ctx.func
                        .borrow_mut()
                        .add_instruction(Instruction::Jump(format!("{}.end", label)));
                } else {
                    elle_error!(self.location.error("Break can only be used in a loop"));
                }

                None
            }
            TokenKind::Continue => {
                if let Some(label) = &gen.loop_labels.last() {
                    ctx.func
                        .borrow_mut()
                        .add_instruction(Instruction::Jump(format!("{}.step", label)));
                } else {
                    elle_error!(self.location.error("Continue can only be used in a loop"));
                }

                None
            }
            _ => match self.value {
                ValueKind::Number(val) => {
                    let num_ty = match self.kind {
                        TokenKind::BoolLiteral => Type::Boolean,
                        TokenKind::IntegerLiteral => Type::Word,
                        TokenKind::FloatLiteral => Type::Single,
                        TokenKind::LongLiteral => Type::Long,
                        _ => Type::Word,
                    };

                    let mut final_ty = if ctx.ty.clone().is_some_and(|ty| !ty.is_pointer()) {
                        ctx.ty.clone().unwrap_or(num_ty)
                    } else {
                        num_ty
                    };

                    if ctx.is_return {
                        final_ty = ctx
                            .func
                            .borrow_mut()
                            .return_type
                            .clone()
                            .unwrap_or(final_ty);
                    }

                    Some((
                        final_ty.clone(),
                        Value::Const(
                            if final_ty.clone() == Type::Double {
                                "d_"
                            } else if final_ty.clone() == Type::Single {
                                "s_"
                            } else {
                                ""
                            }
                            .into(),
                            val,
                        ),
                    ))
                }
                ValueKind::String(val) => {
                    gen.tmp_counter += 1;
                    let name = gen
                        .tmp_name_with_debug_assertions(&ctx.func.borrow_mut().name.clone(), true);

                    gen.data_sections.push(Data::new(
                        Linkage::private(),
                        name.clone(),
                        None,
                        vec![
                            (Type::Byte, DataItem::String(val.replace("\n", "\\n"))),
                            (Type::Byte, DataItem::Const(0)),
                        ],
                    ));

                    Some((Type::Pointer(Box::new(Type::Char)), Value::Global(name)))
                }
                ValueKind::Character(val) => {
                    Some((Type::Char, Value::Const("".into(), val as i128)))
                }
                ValueKind::Nil => {
                    gen.tmp_counter += 1;
                    let name = gen
                        .tmp_name_with_debug_assertions(&ctx.func.borrow_mut().name.clone(), true);

                    gen.data_sections.push(Data::new(
                        Linkage::private(),
                        name.clone(),
                        None,
                        vec![(Type::Byte, DataItem::Const(0))],
                    ));

                    Some((Type::Long, Value::Global(name)))
                }
            },
        }
    }
}
