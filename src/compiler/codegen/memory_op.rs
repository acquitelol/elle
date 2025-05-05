use std::cell::RefCell;

use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        qbe::{instruction::Instruction, r#type::Type, value::Value},
    },
    elle_error, is_generic,
    lexer::enums::{Token, TokenKind, ValueKind},
    misc::constants::LOAD_REF_CONSTANT,
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

            macro_rules! exists {
                ($constant:expr) => {
                    ctx.module
                        .borrow()
                        .functions
                        .get(&format!("{struct_name}.{}", $constant))
                        .is_some()
                        || (is_generic!(struct_name)
                            && gen
                                .generic_functions
                                .get(&format!(
                                    "{}.{}",
                                    Type::from_internal_id(&struct_name).0,
                                    $constant
                                ))
                                .is_some())
                        || gen
                            .generic_functions
                            .get(&format!("{struct_name}.{}", $constant))
                            .is_some() // The struct isn't generic but the function is
                };
            }

            if (self.value.is_some() && exists!(STORE_CONSTANT))
                || (self.addr_only && exists!(LOAD_REF_CONSTANT))
                || exists!(LOAD_CONSTANT)
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
                } else if self.addr_only {
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
                    location: self.left_location,
                });

                return node.compile(gen, ctx);
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

        if !(matches!(left_ty, Type::Pointer(_)) || matches!(right_ty, Type::Pointer(_))) {
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
            left_ty.get_pointer_inner().unwrap()
        } else {
            right_ty.get_pointer_inner().unwrap()
        };

        let node = AstNode::BinaryOperation(BinaryOperation {
            left: if left_ty.is_pointer() {
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
                right: if left_ty.is_pointer() {
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
            let (_, compiled) = val
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

            ctx.func.borrow_mut().add_instruction(Instruction::Store(
                inner.clone(),
                compiled_location,
                compiled.clone(),
            ));

            return Some((inner, compiled));
        }

        let res = if self.addr_only {
            (Type::Pointer(Box::new(inner)), compiled_location)
        } else if inner.is_struct() {
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
