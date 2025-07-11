use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler, VariableInfo},
        lib::convert::convert_to_type,
        qbe::{instruction::Instruction, r#type::Type, value::Value},
    },
    elle_error,
    lexer::enums::{Token, TokenKind, ValueKind},
    parser::enums::{AstNode, Declare, Literal, StructLiteral},
    GC_NOOP,
};

impl Codegen<'_> for Declare {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let plain_name = self.name.value.get_string_inner().unwrap();

        let existing = match gen.get_variable(
            plain_name.as_str(),
            Some(ctx.func),
            Some(ctx.module),
            &VariableInfo::default(),
        ) {
            Ok((ty, _)) => ty,
            Err(_) => Type::Word,
        };

        if self.r#type.is_none()
            && gen
                .get_variable(
                    plain_name.as_str(),
                    Some(ctx.func),
                    Some(ctx.module),
                    &VariableInfo::default(),
                )
                .is_err()
        {
            elle_error!(
                self.location.borrow().error(
                    format!("Variable named '{plain_name}' hasn't been declared yet.\nPlease declare it before trying to re-declare it.")));
        }

        if self.r#type.clone().is_some_and(|ty| ty == Type::Infer) && self.value.is_none() {
            elle_error!(self.location.borrow().error(format!(
                "Failed to determine a type for '{plain_name}'.\nPlease give this variable a type or a value."
            )));
        }

        let res = gen.get_variable(
            &format!("{plain_name}.addr"),
            Some(ctx.func),
            Some(ctx.module),
            &VariableInfo::default(),
        );

        let mut local_ty = self.r#type.clone().unwrap_or(existing);
        let mut temp = if local_ty == Type::Infer {
            None
        } else {
            Some(gen.new_variable(&local_ty, &plain_name, Some(ctx.func), true, false))
        };

        let node = *self.value.unwrap_or_else(|| {
            Box::new(if self.r#type.clone().is_some_and(|ty| ty.is_struct()) {
                AstNode::StructLiteral(StructLiteral {
                    name: Token::from_ident(
                        &self.r#type.clone().unwrap().get_struct_inner().unwrap(),
                    ),
                    values: vec![],
                    location: self.location.clone(),
                })
            } else {
                AstNode::Literal(Literal {
                    kind: TokenKind::IntegerLiteral,
                    value: ValueKind::Number(0),
                    location: self.location.clone(),
                    tagged: false,
                })
            })
        });

        let parsed = node.compile(
            gen,
            &CodegenContext {
                ty: if local_ty == Type::Infer {
                    None
                } else {
                    Some(local_ty.clone())
                },
                value: temp.clone(),
                ..ctx.clone()
            },
        );

        if let Some((ret_ty, value)) = parsed {
            if local_ty == Type::Infer {
                local_ty = ret_ty.clone();

                temp = Some(gen.new_variable(&local_ty, &plain_name, Some(ctx.func), true, false));

                let scope = gen.scopes.last_mut().expect("Expected last scope to exist");
                scope.insert(
                    plain_name.clone(),
                    (local_ty.clone(), temp.clone().unwrap()),
                );
            }
            // in `fn *a = fn() -> 5;`
            // - fn *a has type Pointer(Fn)
            // - fn() -> 5 has type Function(...)
            // essentially the below sets the former
            // to the latter if necessary
            if ret_ty.is_function()
                && local_ty
                    .get_pointer_inner()
                    .is_some_and(|ptr| ptr.get_unknown_inner().is_some_and(|inner| inner == "fn"))
            {
                local_ty = ret_ty.clone();
                temp = Some(gen.new_variable(&local_ty, &plain_name, Some(ctx.func), false, false));
            }

            let (final_ty, final_val) = if ret_ty == local_ty {
                (local_ty.clone(), value)
            } else {
                convert_to_type(
                    gen,
                    ctx.func,
                    ret_ty,
                    local_ty.clone(),
                    value,
                    &self.location,
                    &self.value_location,
                    false,
                )
            };

            if res.is_ok() && self.r#type.is_none() {
                let (addr_ty, addr_val) = res.unwrap();

                if addr_ty != final_ty
                    && !(addr_ty.is_pointer()
                        && final_ty.is_pointer()
                        && final_ty.get_pointer_inner().unwrap().is_void())
                {
                    elle_error!(self.location.borrow().error(format!(
                        "Cannot redeclare '{}' which has type {} to type {}",
                        plain_name,
                        addr_ty.display(),
                        final_ty.display()
                    )))
                }

                ctx.func.borrow_mut().add_instruction(Instruction::Store(
                    addr_ty.clone(),
                    addr_val.clone(),
                    final_val.clone(),
                ));

                if addr_ty.is_pointer() && !gen.no_gc {
                    ctx.func.borrow_mut().add_instruction(Instruction::Call(
                        Value::Global(GC_NOOP.into()),
                        vec![(addr_ty.clone(), addr_val.clone())],
                    ));
                }

                gen.address_pool.insert(temp.unwrap(), addr_val);
                let res = (addr_ty, final_val);

                if self.name.tagged {
                    elle_error!(format!(
                        "hover\n{}\n{}\nlet {plain_name}: {}",
                        self.name.location.borrow().display_plain(false),
                        self.name.location.borrow().display_plain(true),
                        res.0.display()
                    ));
                }

                return Some(res);
            }

            let addr_val = gen.new_variable(
                &local_ty,
                &format!("{plain_name}.addr"),
                Some(ctx.func),
                true,
                false,
            );

            ctx.func.borrow_mut().assign_instruction_front(
                &addr_val,
                &Type::Pointer(Box::new(final_ty.clone())),
                Instruction::Alloc8(Value::Const(
                    String::new(),
                    i128::from(if final_ty.is_struct() {
                        Type::Pointer(Box::new(Type::Void)).size(ctx.module)
                    } else {
                        final_ty.size(ctx.module)
                    }),
                )),
            );

            ctx.func.borrow_mut().add_instruction(Instruction::Store(
                final_ty.clone(),
                addr_val.clone(),
                final_val.clone(),
            ));

            if final_ty.is_pointer() && !gen.no_gc {
                ctx.func.borrow_mut().add_instruction(Instruction::Call(
                    Value::Global(GC_NOOP.into()),
                    vec![(final_ty.clone(), addr_val.clone())],
                ));
            }

            gen.address_pool.insert(temp.unwrap(), addr_val);
            let res = (final_ty, final_val);

            if self.name.tagged {
                elle_error!(format!(
                    "hover\n{}\n{}\nlet {plain_name}: {}",
                    self.name.location.borrow().display_plain(false),
                    self.name.location.borrow().display_plain(true),
                    res.0.display()
                ));
            }

            Some(res)
        } else {
            elle_error!(self
                .location
                .borrow()
                .with_extra_info("This variable might be assigned to a statement")
                .error(format!(
                    "Unexpected error when declaring variable named '{plain_name}'\nCould not generate a valid value for this variable.\nMaybe '{plain_name}' is being incorrectly assigned to a statement?"
                )));
        }
    }
}
