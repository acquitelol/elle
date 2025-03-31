use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        enums::{Instruction, Type, Value},
    },
    elle_error,
    lexer::enums::{TokenKind, ValueKind},
    parser::enums::{AstNode, Declare, Literal, StructLiteral},
    GC_NOOP,
};

impl Codegen<'_> for Declare {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let existing = match gen.get_variable(self.name.as_str(), Some(ctx.func), Some(ctx.module))
        {
            Ok((ty, _)) => ty,
            Err(_) => Type::Word,
        };

        if self.r#type.is_none()
            && gen
                .get_variable(self.name.as_str(), Some(ctx.func), Some(ctx.module))
                .is_err()
        {
            elle_error!(
                self.location.error(
                    format!("Variable named '{}' hasn't been declared yet.\nPlease declare it before trying to re-declare it.", self.name)));
        }

        if self.r#type.clone().is_some_and(|ty| ty == Type::Infer) && self.value.is_none() {
            elle_error!(self.location.error(format!(
                "Failed to determine a type for '{}'.\nPlease give this variable a type or a value.",
                self.name
            )));
        }

        let res = gen.get_variable(
            &format!("{}.addr", self.name),
            Some(ctx.func),
            Some(ctx.module),
        );

        let mut local_ty = self.r#type.clone().unwrap_or(existing);
        let mut temp = if local_ty == Type::Infer {
            None
        } else {
            Some(gen.new_variable(&local_ty, &self.name, Some(ctx.func), true, false))
        };

        let node = *self.value.unwrap_or(Box::new(
            if self.r#type.clone().is_some_and(|ty| ty.is_struct()) {
                AstNode::StructLiteral(StructLiteral {
                    name: self.r#type.clone().unwrap().get_struct_inner().unwrap(),
                    values: vec![],
                    location: self.location.clone(),
                })
            } else {
                AstNode::Literal(Literal {
                    kind: TokenKind::IntegerLiteral,
                    value: ValueKind::Number(0),
                    location: self.location.clone(),
                })
            },
        ));

        let parsed = node.compile(
            gen,
            &CodegenContext {
                ty: if local_ty == Type::Infer {
                    None
                } else {
                    Some(local_ty.clone())
                },
                value: temp.clone(),
                is_return: false,
                ..ctx.clone()
            },
        );

        if let Some((ret_ty, value)) = parsed {
            if local_ty == Type::Infer {
                local_ty = ret_ty.clone();

                temp = Some(gen.new_variable(&local_ty, &self.name, Some(ctx.func), true, false));

                let scope = gen.scopes.last_mut().expect("Expected last scope to exist");
                scope.insert(
                    self.name.to_owned(),
                    (local_ty.clone(), temp.clone().unwrap()),
                );
            }
            // in `fn *a = fn() -> 5;`
            // - fn *a has type Pointer(Fn)
            // - fn() -> 5 has type Function(...)
            // essentially the below sets the former
            // to the latter if necessary
            if ret_ty.is_function()
                && local_ty.get_pointer_inner().is_some_and(|ptr| {
                    ptr.get_unknown_inner()
                        .is_some_and(|inner| inner == "fn".to_string())
                })
            {
                local_ty = ret_ty.clone();
                temp = Some(gen.new_variable(&local_ty, &self.name, Some(ctx.func), false, false))
            }

            let (final_ty, final_val) = if ret_ty != local_ty {
                gen.convert_to_type(
                    ctx.func,
                    ret_ty,
                    local_ty.clone(),
                    value.clone(),
                    &self.location,
                    &self.value_location,
                    false,
                )
            } else {
                (local_ty.clone(), value.clone())
            };

            if res.is_ok() && self.r#type.is_none() {
                let (addr_ty, addr_val) = res.unwrap();

                if addr_ty != final_ty
                    && !(addr_ty.is_pointer()
                        && final_ty.is_pointer()
                        && final_ty.get_pointer_inner().unwrap().is_void())
                {
                    elle_error!(self.location.error(format!(
                        "Cannot redeclare '{}' which has type {} to type {}",
                        self.name,
                        addr_ty.display(),
                        final_ty.display()
                    )))
                }

                ctx.func.borrow_mut().add_instruction(Instruction::Store(
                    addr_ty.clone(),
                    addr_val.clone(),
                    final_val.clone(),
                ));

                if addr_ty.is_pointer() {
                    ctx.func.borrow_mut().add_instruction(Instruction::Call(
                        Value::Global(GC_NOOP.into()),
                        vec![(addr_ty.clone(), addr_val.clone())],
                    ));
                }

                gen.address_pool
                    .insert(temp.unwrap().clone(), addr_val.clone());
                return Some((addr_ty, final_val));
            }

            let addr_val = gen.new_variable(
                &local_ty,
                &format!("{}.addr", self.name),
                Some(ctx.func),
                true,
                false,
            );

            ctx.func.borrow_mut().assign_instruction_front(
                &addr_val,
                &Type::Pointer(Box::new(final_ty.clone())),
                Instruction::Alloc8(Value::Const(
                    "".into(),
                    if final_ty.is_struct() {
                        Type::Pointer(Box::new(Type::Void))
                    } else {
                        final_ty.clone()
                    }
                    .size(ctx.module) as i128,
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

            gen.address_pool
                .insert(temp.clone().unwrap(), addr_val.clone());
            return Some((final_ty, final_val));
        }

        None
    }
}
