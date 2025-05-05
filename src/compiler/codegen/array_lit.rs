use std::cell::RefCell;

use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        qbe::{instruction::Instruction, r#type::Type, value::Value},
    },
    elle_error,
    lexer::enums::Token,
    parser::enums::{ArrayLiteral, AstNode, Conversion, FunctionCall},
};

impl Codegen<'_> for ArrayLiteral {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let inner_ty = ctx.ty.clone().and_then(|ty| ty.get_pointer_inner());

        if self.dynamic {
            let new_func = ctx.func.borrow_mut().to_owned();
            let inner_ty = if let Some(ref ty) = self.explicit_inner {
                Some(ty.clone())
            } else if !self.values.is_empty() {
                let (ty, _) = self.values[0]
                    .clone()
                    .1
                    .compile(
                        gen,
                        &CodegenContext {
                            func: &RefCell::new(new_func),
                            ..ctx.clone()
                        },
                    )
                    .unwrap_or_else(|| {
                        elle_error!(self.location.borrow().error(
                            "Unexpected error when trying to compile the first item in an array"
                        ))
                    });

                Some(ty)
            } else if !self.known_generics.is_empty() {
                Some(self.known_generics.first().unwrap().clone())
            } else {
                ctx.ty.clone()
            };

            let node = AstNode::FunctionCall(FunctionCall {
                namespace_token: Token::from_ident("Array"),
                name_token: Token::from_ident("new"),
                name: "Array.new".into(),
                generics: inner_ty
                    .as_ref()
                    .map_or_else(Vec::new, |ty| vec![ty.clone()]),
                parameters: if let Some(ty) = inner_ty {
                    self.values
                        .into_iter()
                        .map(|(loc, node)| {
                            (
                                loc.clone(),
                                AstNode::Conversion(Conversion {
                                    r#type: Some(ty.clone()),
                                    value: Box::new(node),
                                    location: loc,
                                    explicit: false,
                                }),
                            )
                        })
                        .collect()
                } else {
                    self.values
                },
                type_method: false,
                ignore_no_def: false,
                location: self.location.clone(),
            });

            let (ty, val) = node.compile(gen, ctx).unwrap_or_else(|| {
                elle_error!(self
                    .location
                    .borrow()
                    .error("Unexpected error when trying to compile a dynamic array"))
            });

            return Some((ty, val));
        }

        let mut first_type: Option<Type> = None;
        let mut results: Vec<Value> = vec![];

        // value.is_some() because we don't want to do this to
        // arrays that aren't assigned to a variable
        if ctx.value.is_some() && ctx.ty.is_some() && !ctx.ty.clone().unwrap().is_pointer() {
            elle_error!(
                self.location.borrow().error(
                    format!("The type of array '{:?}' must be a pointer to the inner type of the array (it is {})",
                        self.values, ctx.ty.clone().unwrap().display()
                    )
                )
            );
        }

        for (i, (location, value)) in self.values.iter().enumerate() {
            let (ty, val) = value
                .clone()
                .compile(
                    gen,
                    &CodegenContext {
                        ty: if inner_ty.is_some() {
                            inner_ty.clone()
                        } else {
                            first_type.clone()
                        },
                        ..ctx.clone()
                    },
                )
                .unwrap_or_else(|| {
                    elle_error!(location.borrow().error(format!(
                        "Unexpected error when trying to compile an item in an array with index {i}"
                    )))
                });

            results.push(val);

            if let Some(first_type) = first_type.clone() {
                if ty != first_type {
                    elle_error!(location.borrow().error(format!(
                        "Inconsistent array types '{}' and '{}' (possibly more)",
                        first_type.display(),
                        ty.display()
                    )));
                }

                if inner_ty.is_some() && inner_ty.clone().unwrap() != first_type {
                    elle_error!(location.borrow().error(format!(
                        "Invalid type of element in array '{}' when the array type is '{}'",
                        ty.display(),
                        inner_ty.unwrap().display(),
                    )))
                }
            } else {
                if inner_ty.is_some() && inner_ty.clone().unwrap() != ty {
                    elle_error!(location.borrow().error(format!(
                        "Invalid type of element in array '{}' when the array type is '{}'",
                        ty.display(),
                        inner_ty.unwrap().display(),
                    )))
                }

                first_type = Some(ty);
            }
        }

        let buf_ty = Type::Pointer(Box::new(first_type.clone().unwrap_or(Type::Void)));
        let array_size = if let Some(ref ty) = first_type {
            self.values.len() as u64 * ty.size(ctx.module)
        } else {
            0
        };
        let array_size_val = Value::Const(
            String::new(),
            i128::from(array_size + Type::Word.size_base()),
        );
        let tmp_full = gen.new_temporary(Some("array.full"), true);

        ctx.func.borrow_mut().assign_instruction_front(
            &tmp_full,
            &buf_ty,
            Instruction::Alloc8(array_size_val.clone()),
        );

        ctx.func.borrow_mut().add_instruction(Instruction::Store(
            Type::Word,
            tmp_full.clone(),
            Value::Const(String::new(), results.len() as i128),
        ));

        let tmp = gen.new_temporary(Some("array"), true);

        ctx.func.borrow_mut().assign_instruction(
            &tmp,
            &buf_ty,
            Instruction::Add(
                tmp_full,
                Value::Const(String::new(), i128::from(Type::Word.size(ctx.module))),
            ),
        );

        gen.buf_metadata.insert(
            ctx.value.clone().unwrap_or_else(|| tmp.clone()),
            (buf_ty.get_pointer_inner().unwrap(), array_size_val),
        );

        for (i, value) in results.iter().enumerate() {
            let value_ptr = gen.new_temporary(Some("array.offset"), true);

            ctx.func.borrow_mut().assign_instruction(
                &value_ptr,
                &Type::Long,
                Instruction::Add(
                    tmp.clone(),
                    Value::Const(
                        String::new(),
                        i as i128 * i128::from(first_type.as_ref().unwrap().size(ctx.module)),
                    ),
                ),
            );

            let ty = first_type.as_ref().unwrap().clone();

            if ty.is_struct() {
                ctx.func.borrow_mut().add_instruction(Instruction::Blit(
                    value.clone(),
                    value_ptr,
                    ty.size(ctx.module),
                ));
            } else {
                ctx.func.borrow_mut().add_instruction(Instruction::Store(
                    ty,
                    value_ptr,
                    value.clone(),
                ));
            }
        }

        Some((buf_ty, tmp))
    }
}
