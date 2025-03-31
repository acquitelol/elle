use std::cell::RefCell;

use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        enums::{Instruction, Type, Value},
    },
    elle_error,
    parser::enums::{ArrayLiteral, AstNode, Conversion, FunctionCall},
};

impl Codegen<'_> for ArrayLiteral {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let inner_ty = if let Some(ty) = ctx.ty.clone() {
            ty.get_pointer_inner()
        } else {
            None
        };

        if self.dynamic {
            let new_func = ctx.func.borrow_mut().to_owned();
            let inner_ty = if let Some(ref ty) = self.explicit_inner {
                Some(ty.clone())
            } else if self.values.len() > 0 {
                let (ty, _) = self.values[0]
                    .clone()
                    .1
                    .compile(
                        gen,
                        &CodegenContext {
                            func: &RefCell::new(new_func),
                            ty: None,
                            value: None,
                            is_return: false,
                            ..ctx.clone()
                        },
                    )
                    .expect(&self.location.error(format!(
                        "Unexpected error when trying to compile the first item in an array"
                    )));

                Some(ty.clone())
            } else if !self.known_generics.is_empty() {
                Some(self.known_generics.get(0).unwrap().clone())
            } else if let Some(ref ty) = ctx.ty {
                Some(ty.clone())
            // } else if is_return {
            //     None
            } else {
                // panic!(
                //     "{}",
                //     location.with_extra_info("Try specifying a type here").error(format!("Could not determine any type for this array.\nPlease specify a type explicitly with the {GREEN}[T;]{RESET} syntax."))
                // )
                None
            };

            let node = AstNode::FunctionCall(FunctionCall {
                name: "Array.new".into(),
                generics: if let Some(ref ty) = inner_ty {
                    vec![ty.clone()]
                } else {
                    vec![]
                },
                parameters: if let Some(ty) = inner_ty {
                    self.values
                        .into_iter()
                        .map(|(loc, node)| {
                            (
                                loc.clone(),
                                AstNode::Conversion(Conversion {
                                    r#type: Some(ty.clone()),
                                    value: Box::new(node),
                                    location: loc.clone(),
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

            let (ty, val) = node.compile(gen, ctx).expect(&self.location.error(format!(
                "Unexpected error when trying to compile a dynamic array"
            )));

            return Some((ty, val));
        }

        let mut first_type: Option<Type> = None;
        let mut results: Vec<Value> = vec![];

        // value.is_some() because we don't want to do this to
        // arrays that aren't assigned to a variable
        if ctx.value.is_some() && ctx.ty.is_some() && !ctx.ty.clone().unwrap().is_pointer() {
            elle_error!(
                self.location.error(
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
                        value: None,
                        is_return: false,
                        ..ctx.clone()
                    },
                )
                .expect(&location.error(format!(
                    "Unexpected error when trying to compile an item in an array with index {}",
                    i
                )));

            results.push(val);

            if let Some(first_type) = first_type.clone() {
                if ty != first_type {
                    elle_error!(location.error(format!(
                        "Inconsistent array types '{}' and '{}' (possibly more)",
                        first_type.display(),
                        ty.display()
                    )));
                }

                if inner_ty.is_some() && inner_ty.clone().unwrap() != first_type {
                    elle_error!(location.error(format!(
                        "Invalid type of element in array '{}' when the array type is '{}'",
                        ty.display(),
                        inner_ty.unwrap().display(),
                    )))
                }
            } else {
                if inner_ty.is_some() && inner_ty.clone().unwrap() != ty {
                    elle_error!(location.error(format!(
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
        let array_size_val = Value::Const("".into(), (array_size + Type::Word.size_base()) as i128);
        let tmp_full = gen.new_temporary(Some("array.full"), true);

        ctx.func.borrow_mut().assign_instruction_front(
            &tmp_full,
            &buf_ty,
            Instruction::Alloc8(array_size_val.clone()),
        );

        ctx.func.borrow_mut().add_instruction(Instruction::Store(
            Type::Word,
            tmp_full.clone(),
            Value::Const("".into(), results.len() as i128),
        ));

        let tmp = gen.new_temporary(Some("array"), true);

        ctx.func.borrow_mut().assign_instruction(
            &tmp,
            &buf_ty,
            Instruction::Add(
                tmp_full,
                Value::Const("".into(), Type::Word.size(ctx.module) as i128),
            ),
        );

        gen.buf_metadata.insert(
            ctx.value.clone().unwrap_or(tmp.clone()),
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
                        "".into(),
                        i as i128 * first_type.as_ref().unwrap().size(ctx.module) as i128,
                    ),
                ),
            );

            ctx.func.borrow_mut().add_instruction(Instruction::Store(
                first_type.as_ref().unwrap().clone(),
                value_ptr,
                value.clone(),
            ));
        }

        Some((buf_ty, tmp))
    }
}
