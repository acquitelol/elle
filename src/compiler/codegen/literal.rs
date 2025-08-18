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
            TokenKind::Identifier | TokenKind::ExactLiteral => match self.value {
                ValueKind::String(name) => {
                    let mut res = gen.get_variable_lazy(
                        &name,
                        Some(ctx.func),
                        Some(ctx.module),
                        &self.location,
                    );

                    if self.tagged && res.as_ref().is_some_and(|(ty, _)| !ty.has_generic_type()) {
                        if res.clone().unwrap().0.is_function() {
                            elle_error!(format!(
                                "hover\n{}\n{}\n{}",
                                self.location.borrow().display_plain(false),
                                self.location.borrow().display_plain(true),
                                res.unwrap().0.display()
                            ));
                        }

                        let is_constant = ctx
                            .module
                            .borrow()
                            .functions
                            .get(&name)
                            .is_some_and(|function| function.constant);

                        elle_error!(format!(
                            "hover\n{}\n{}\n{} {}: {}",
                            self.location.borrow().display_plain(false),
                            self.location.borrow().display_plain(true),
                            if is_constant { "const" } else { "let" },
                            name.replace('.', "::"),
                            res.unwrap().0.display()
                        ));
                    }

                    // unwrap aliases: math::floor -> floor
                    if let Some((ty, _)) = res.as_mut() && ty.is_function() {
                        let Type::Function(inner) = ty else { unreachable!() };

                        if let Some(mut func) = *inner.clone() {
                            if let Some(ref unaliased) = func.unaliased {
                                func.name = unaliased.clone();
                            }

                            *inner = Box::new(Some(func));
                        }
                    }

                    res
                }
                _ => None,
            },
            TokenKind::Break => {
                if let Some(label) = &gen.loop_labels.last() {
                    ctx.func
                        .borrow_mut()
                        .add_instruction(Instruction::Jump(format!("{label}.end")));
                } else {
                    elle_error!(self
                        .location
                        .borrow()
                        .error("Break can only be used in a loop"));
                }

                None
            }
            TokenKind::Continue => {
                if let Some(label) = &gen.loop_labels.last() {
                    ctx.func
                        .borrow_mut()
                        .add_instruction(Instruction::Jump(format!("{label}.step")));
                } else {
                    elle_error!(self
                        .location
                        .borrow()
                        .error("Continue can only be used in a loop"));
                }

                None
            }
            _ => match self.value {
                ValueKind::Number(val) => {
                    #[allow(clippy::match_same_arms)]
                    let num_ty = match self.kind {
                        // prevents -1 or 65535 from being interpreted as bools
                        TokenKind::BoolLiteral if [0, 1].contains(&val) => Type::Boolean,
                        TokenKind::IntegerLiteral => Type::Word,
                        TokenKind::FloatLiteral => Type::Single,
                        TokenKind::LongLiteral => Type::Long,
                        _ => Type::Word,
                    };

                    let mut final_ty = if ctx.ty.clone().is_some_and(|ty| {
                        !ty.is_pointer()
                            && !ty.is_unknown()
                            && ty.is_strictly_number()
                            // prevents -1 or 65535 from being interpreted as bools
                            && (!ty.is_bool() || [0, 1].contains(&val))
                    }) {
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

                    let res = (
                        final_ty.clone(),
                        Value::Const(
                            if final_ty == Type::Double {
                                "d_"
                            } else if final_ty == Type::Single {
                                "s_"
                            } else {
                                ""
                            }
                            .into(),
                            val,
                        ),
                    );

                    if self.tagged {
                        elle_error!(format!(
                            "hover\n{}\n{}\n{}: {}",
                            self.location.borrow().display_plain(false),
                            self.location.borrow().display_plain(true),
                            val,
                            res.0.display()
                        ));
                    }

                    Some(res)
                }
                ValueKind::String(val) => {
                    gen.tmp_counter += 1;
                    let name = gen
                        .tmp_name_with_debug_assertions(&ctx.func.borrow_mut().name.clone(), true);
                    let escaped = val.replace('\n', "\\n");

                    let data = gen
                        .data_sections
                        .entry(escaped.clone())
                        .or_insert(Data::new(
                            Linkage::private(),
                            name.clone(),
                            None,
                            vec![
                                (Type::Byte, DataItem::String(escaped)),
                                (Type::Byte, DataItem::Const(0)),
                            ],
                        ));

                    let res = (
                        Type::Pointer(Box::new(Type::Char)),
                        Value::Global(data.name.clone()),
                    );

                    if self.tagged {
                        elle_error!(format!(
                            "hover\n{}\n{}\n\"{val}\": {}",
                            self.location.borrow().display_plain(false),
                            self.location.borrow().display_plain(true),
                            res.0.display()
                        ));
                    }

                    Some(res)
                }
                ValueKind::Character(val) => {
                    let res = (Type::Char, Value::Const(String::new(), val as i128));

                    if self.tagged {
                        elle_error!(format!(
                            "hover\n{}\n{}\n'{val}': {}",
                            self.location.borrow().display_plain(false),
                            self.location.borrow().display_plain(true),
                            res.0.display()
                        ));
                    }

                    Some(res)
                }
                ValueKind::Nil => {
                    gen.tmp_counter += 1;
                    let name = gen
                        .tmp_name_with_debug_assertions(&ctx.func.borrow_mut().name.clone(), true);

                    gen.data_sections.insert(
                        name.clone(),
                        Data::new(
                            Linkage::private(),
                            name.clone(),
                            None,
                            vec![(Type::Byte, DataItem::Const(0))],
                        ),
                    );

                    Some((Type::Long, Value::Global(name)))
                }
            },
        }
    }
}
