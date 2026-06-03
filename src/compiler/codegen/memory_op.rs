use std::cell::RefCell;

use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        lib::convert::convert_to_type,
        qbe::{instruction::Instruction, r#type::Type, value::Value},
    },
    elle_error, is_generic,
    lexer::enums::{Token, TokenKind, ValueKind},
    misc::constants::{DEREF_LOAD_CONSTANT, DEREF_STORE_CONSTANT, LOAD_REF_CONSTANT},
    parser::enums::{AstNode, BinaryOperation, FunctionCall, Literal, MemoryOperation},
    LOAD_CONSTANT, STORE_CONSTANT,
};

impl Codegen<'_> for MemoryOperation {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let mut tmp_func = ctx.func.borrow().clone();
        tmp_func.add_block("start");

        let tmp_ctx = CodegenContext {
            func: &RefCell::new(tmp_func.clone()),
            ..ctx.clone()
        };

        let (left_ty, _) = self.left.clone().compile(gen, &tmp_ctx).unwrap_or_else(|| {
            elle_error!(self.left_location.borrow().error(format!(
                "Unexpected error when trying to compile the left side of a {} statement",
                if self.value.is_some() {
                    "store"
                } else {
                    "load"
                }
            )))
        });

        macro_rules! exists {
            ($struct_name:expr, $constant:expr) => {
                ctx.module
                    .borrow()
                    .functions
                    .get(&format!("{}.{}", $struct_name, $constant))
                    .is_some()
                    || (is_generic!($struct_name)
                        && gen
                            .generic_functions
                            .get(&format!(
                                "{}.{}",
                                Type::from_internal_id(&$struct_name).0,
                                $constant
                            ))
                            .is_some())
                    || gen
                        .generic_functions
                        .get(&format!("{}.{}", $struct_name, $constant))
                        .is_some() // The struct isn't generic but the function is
            };
        }

        if !self.is_deref
            && (left_ty.is_struct()
                || left_ty.is_pointer() && left_ty.get_pointer_inner().unwrap().is_struct())
        {
            let struct_name = if left_ty.is_struct() {
                left_ty.get_struct_inner().unwrap()
            } else {
                left_ty
                    .get_pointer_inner()
                    .unwrap()
                    .get_struct_inner()
                    .unwrap()
            };

            if self.value.is_some() && exists!(struct_name, STORE_CONSTANT)
                || (self.value.is_none()
                    && (self.addr_only || ctx.is_field_access)
                    && exists!(struct_name, LOAD_REF_CONSTANT))
                || (self.value.is_none() && exists!(struct_name, LOAD_CONSTANT))
            {
                let mut parameters = vec![
                    (self.left_location.clone(), *self.left),
                    (self.right_location, *self.right),
                ];

                if self.value.is_some() {
                    parameters.push((self.value_location, *self.value.clone().unwrap()));
                }

                let constant = if self.value.is_some() {
                    STORE_CONSTANT
                } else if self.addr_only || ctx.is_field_access {
                    LOAD_REF_CONSTANT
                } else {
                    LOAD_CONSTANT
                };

                let node = AstNode::FunctionCall(FunctionCall {
                    namespace_token: Token::from_ident(""),
                    name_token: Token::from_ident(constant),
                    name: constant.into(),
                    generics: vec![],
                    parameters,
                    type_method: true,
                    ignore_no_def: false,
                    location: self.left_location.clone(),
                });

                let (ty, val) = node.compile(gen, ctx)?;

                if ctx.is_field_access
                    && ty.is_pointer()
                    && ty
                        .get_pointer_inner()
                        .is_some_and(|inner| inner.is_pointer())
                {
                    let tmp = gen.new_temporary(None, false);
                    let res_ty = ty.get_pointer_inner().unwrap();

                    ctx.func.borrow_mut().assign_instruction(
                        &tmp,
                        &res_ty.clone(),
                        Instruction::Load(res_ty.clone(), val.clone()),
                    );

                    return Some((res_ty, tmp));
                }

                return Some((ty, val));
            }
        }

        let (right_ty, _) = self
            .right
            .clone()
            .compile(gen, &tmp_ctx)
            .unwrap_or_else(|| {
                elle_error!(self.right_location.borrow().error(format!(
                    "Unexpected error when trying to compile the right side of a {} statement",
                    if self.value.is_some() {
                        "store"
                    } else {
                        "load"
                    }
                )))
            });

        if let Some(struct_name) = left_ty.get_struct_inner()
            && if self.value.is_some() {
                exists!(struct_name, DEREF_STORE_CONSTANT)
            } else {
                exists!(struct_name, DEREF_LOAD_CONSTANT)
            }
        {
            let mut parameters = vec![(self.left_location.clone(), *self.left)];

            if let Some(value) = self.value.clone() {
                parameters.push((self.value_location, *value));
            }

            let constant = if self.value.is_some() {
                DEREF_STORE_CONSTANT
            } else {
                DEREF_LOAD_CONSTANT
            };

            let node = AstNode::FunctionCall(FunctionCall {
                namespace_token: Token::from_ident(""),
                name_token: Token::from_ident(constant),
                name: constant.into(),
                generics: vec![],
                parameters,
                type_method: true,
                ignore_no_def: false,
                location: self.left_location,
            });

            return node.compile(gen, ctx);
        }

        if !(left_ty.is_pointer_like() || right_ty.is_pointer_like()) {
            elle_error!(self.left_location.borrow().error(format!(
                "Cannot {} data {} non-pointer types ({} and {})",
                if self.value.is_some() {
                    "store"
                } else {
                    "load"
                },
                if self.value.is_some() { "to" } else { "from" },
                left_ty.display(),
                right_ty.display()
            )));
        }

        let inner = if left_ty.is_pointer() {
            left_ty.get_pointer_inner()
        } else if left_ty.is_static_array() {
            left_ty.get_static_array_inner()
        } else if right_ty.is_pointer() {
            right_ty.get_pointer_inner()
        } else {
            right_ty.get_static_array_inner()
        }
        .unwrap();

        let node = AstNode::BinaryOperation(BinaryOperation {
            left: if left_ty.is_pointer_like() {
                self.left.clone()
            } else {
                self.right.clone()
            },
            right: Box::new(AstNode::BinaryOperation(BinaryOperation {
                left: Box::new(AstNode::Literal(Literal {
                    kind: TokenKind::LongLiteral,
                    value: ValueKind::Number(i128::from(inner.size(ctx.module))),
                    location: self.right_location.clone(),
                    tagged: false,
                })),
                right: if left_ty.is_pointer_like() {
                    self.right
                } else {
                    self.left
                },
                operator: TokenKind::Multiply,
                treat_as_string: false,
                dunder_methods: true,
                location: self.right_location.clone(),
            })),
            operator: TokenKind::Add,
            treat_as_string: false,
            dunder_methods: true,
            location: self.right_location.clone(),
        });

        let (_, compiled_location) = node.compile(gen, &ctx.to_nnf()).unwrap_or_else(|| {
            elle_error!(self.right_location.borrow().error(format!(
                "Unexpected error when trying to compile the offset of a {} statement",
                if self.value.is_some() {
                    "store"
                } else {
                    "load"
                }
            )))
        });

        if let Some(ref val) = self.value {
            let (val_ty, compiled) = val
                .clone()
                .compile(
                    gen,
                    &CodegenContext {
                        ty: Some(inner.clone()),
                        ..ctx.clone()
                    },
                )
                .unwrap_or_else(|| {
                    elle_error!(self.value_location.borrow().error(format!(
                        "Unexpected error when trying to compile the value of a {} statement",
                        if self.value.is_some() {
                            "store"
                        } else {
                            "load"
                        }
                    )))
                });

            let (final_ty, final_val) = convert_to_type(
                gen,
                ctx.func,
                val_ty,
                inner,
                compiled,
                &self.left_location,
                &self.right_location,
                false,
            );

            if final_ty.is_struct() || final_ty.is_static_array() {
                ctx.func.borrow_mut().add_instruction(Instruction::Blit(
                    final_val.clone(),
                    compiled_location,
                    final_ty.size(ctx.module),
                ));
            } else {
                ctx.func.borrow_mut().add_instruction(Instruction::Store(
                    final_ty.clone(),
                    compiled_location,
                    final_val.clone(),
                ));
            }

            return Some((final_ty, final_val));
        }

        let res = if self.addr_only {
            (Type::Pointer(Box::new(inner)), compiled_location)
        } else if inner.is_struct() || inner.is_static_array() {
            (inner, compiled_location)
        } else {
            let temp = gen.new_temporary(Some("load"), true);

            ctx.func.borrow_mut().assign_instruction(
                &temp,
                &inner,
                Instruction::Load(inner.clone(), compiled_location),
            );

            (inner, temp)
        };

        Some(res)
    }
}
