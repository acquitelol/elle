use std::cell::RefCell;

use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        qbe::{function::Function, r#type::Type, value::Value},
    },
    elle_error, get_GREEN, get_RED, get_RESET,
    lexer::enums::{TokenKind, ValueKind},
    parser::enums::{AstNode, Environment, FieldAccess, Literal, SetAllocator},
    Warning, ARBITRARY_ALLOCATOR_NAME, GREEN, RED, RESET,
};

impl Codegen<'_> for SetAllocator {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let mut tmp_func = Function::default();
        tmp_func.add_block("start");

        let (ty, _) = self
            .value
            .clone()
            .compile(
                gen,
                &CodegenContext {
                    func: &RefCell::new(tmp_func),
                    ..ctx.clone()
                },
            )
            .unwrap_or_else(|| {
                elle_error!(self
                    .location
                    .error("Unexpected error when compiling a `set allocator` expresssion"))
            });

        if !ty.is_struct() && !(ty.is_pointer() && ty.get_pointer_inner().unwrap().is_struct()) {
            elle_error!(self
                .location
                .with_extra_info(format!("This has the type {}", ty.display()))
                .error("Cannot set an allocator to a non-allocator expression"))
        }

        let allocator_name = if ty.is_struct() {
            ty.get_struct_inner().unwrap()
        } else {
            ty.get_pointer_inner().unwrap().get_struct_inner().unwrap()
        };

        macro_rules! method_or_noop {
            ($name:literal) => {{
                let method_name = format!("{allocator_name}.{}", $name);

                AstNode::Literal(Literal {
                    kind: TokenKind::Identifier,
                    value: ValueKind::String(if ctx.module.borrow().functions.iter().find(|f| f.name == method_name).is_some() {
                        method_name
                    } else {
                        if gen.warnings.has_warning(Warning::AllocatorMethodsMissing) {
                            eprintln!(
                                "{}",
                                self.location.basic_warning(format!(
                                    "The allocator '{GREEN}{}{RESET}' has no method named '{GREEN}{}{RESET}'.\nIt will be set to a function which returns {RED}nil{RESET} instead.",
                                    allocator_name,
                                    method_name.replace(".", "::"),
                                    GREEN = get_GREEN!(),
                                    RED = get_RED!(),
                                    RESET = get_RESET!(),
                                ))
                            );
                        }

                        format!("{ARBITRARY_ALLOCATOR_NAME}.noop")
                    }),
                    location: self.location.clone(),
                    tagged: false
                })
            }};
        }

        let parts = vec![
            ("inner", *self.value),
            (
                "kind",
                AstNode::Literal(Literal {
                    kind: TokenKind::StringLiteral,
                    value: ValueKind::String(allocator_name.clone()),
                    location: self.location.clone(),
                    tagged: false,
                }),
            ),
            ("alloc", method_or_noop!("alloc")),
            ("realloc", method_or_noop!("realloc")),
            ("free", method_or_noop!("free")),
            ("free_self", method_or_noop!("free_self")),
        ];

        for (field, expr) in parts {
            let node = AstNode::FieldAccess(FieldAccess {
                left: Box::new(AstNode::Environment(Environment {
                    value: None,
                    location: self.location.clone(),
                })),
                right: Box::new(AstNode::FieldAccess(FieldAccess {
                    left: Box::new(AstNode::Literal(Literal {
                        kind: TokenKind::Identifier,
                        value: ValueKind::String("allocator".into()),
                        location: self.location.clone(),
                        tagged: false,
                    })),
                    right: Box::new(AstNode::Literal(Literal {
                        kind: TokenKind::Identifier,
                        value: ValueKind::String(field.into()),
                        location: self.location.clone(),
                        tagged: false,
                    })),
                    value: None,
                    location: self.location.clone(),
                })),
                value: Some(Box::new(expr)),
                location: self.location.clone(),
            });

            node.compile(gen, ctx).unwrap_or_else(|| {
                elle_error!(self
                    .location
                    .error("Unexpected error when compiling a `set allocator` expression"))
            });
        }

        None
    }
}
