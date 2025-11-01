use std::collections::HashSet;

use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        lib::{
            convert::convert_to_type, field_utils::member_to_offset,
            mono_struct::create_monomorphized_struct,
        },
        qbe::{instruction::Instruction, r#type::Type, value::Value},
    },
    elle_error, is_generic,
    parser::enums::StructLiteral,
    struct_hover, Warning,
};

impl Codegen<'_> for StructLiteral {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let mut plain_name = self.name.value.get_string_inner().unwrap();
        let inner = ctx.ty.clone().unwrap_or_else(|| {
            ctx.func
                .borrow_mut()
                .return_type
                .clone()
                .unwrap_or(Type::Void)
        });

        if inner.is_struct()
            && is_generic!(inner.get_struct_inner().unwrap())
            && !is_generic!(plain_name)
        {
            let (generic_name, ..) = Type::from_internal_id(&inner.get_struct_inner().unwrap());

            if plain_name == generic_name {
                plain_name = inner.get_struct_inner().unwrap();
            }
        }

        if !gen.struct_pool.contains_key(&plain_name) {
            if is_generic!(plain_name) {
                create_monomorphized_struct(gen, ctx.module, &plain_name);
            } else {
                elle_error!(
                    self.location.borrow().error(format!(
                        "Could not find struct named '{}'. Did you spell it correctly?\nThis struct may be generic but missing generic parameters.",
                        Type::Struct(plain_name).display()
                    ))
                );
            }
        }

        let td = ctx
            .module
            .borrow()
            .types
            .clone()
            .into_iter()
            .find(|td| td.name == plain_name)
            .unwrap_or_else(|| {
                elle_error!(self
                    .location
                    .borrow()
                    .error(format!("Unable to find struct named '{plain_name}'")))
            });

        if !td.usable && !ctx.func.borrow_mut().imported {
            elle_error!(self.location.borrow().error(format!(
                "Struct named '{}' was not imported and can't be used",
                Type::Struct(plain_name.clone()).display()
            )))
        }

        let struct_pool = gen.struct_pool.clone();
        let struct_def = struct_pool.get(&plain_name).unwrap();
        let members = struct_def.1.clone();
        let member_names = members
            .iter()
            .map(|member| member.name.clone())
            .collect::<Vec<String>>();

        let member_set: HashSet<_> = member_names.iter().cloned().collect();
        let value_set: HashSet<_> = self.values.iter().map(|value| value.0.clone()).collect();
        let diff: Vec<_> = member_set.difference(&value_set).collect();

        // we're assuming that if you are spreading something,
        // all fields will automatically be set to some value
        if gen.warnings.has_warning(Warning::StructFieldsMissing) && self.spreads.is_empty() && !self.allow_empty {
            for member in diff {
                if members
                    .iter()
                    .find(|x| x.name == *member)
                    .is_some_and(|member| !member.is_unused)
                {
                    eprintln!(
                        "{}",
                        self.location.borrow().warning(format!(
                            "Declaring struct '{}' without field '{member}'",
                            Type::Struct(plain_name.clone()).display(),
                        ))
                    );
                }
            }
        }

        let ty = Type::Struct(plain_name.clone());
        let size = ty.size(ctx.module);

        let alloc_tmp = gen.new_temporary(Some(&format!("struct.{plain_name}")), true);

        #[cfg(debug_assertions)]
        ctx.func
            .borrow_mut()
            .add_instruction(Instruction::Comment(format!("size of :{plain_name}")));

        ctx.func.borrow_mut().assign_instruction_front(
            &alloc_tmp,
            &Type::Long,
            Instruction::Alloc8(Value::Const(String::new(), i128::from(size))),
        );

        for (location, spread) in self.spreads.iter().cloned() {
            let (spread_ty, value) =
                spread.compile(gen, &CodegenContext {
                    ty: Some(ty.clone()),
                    ..ctx.to_nnf()
                })
                .unwrap_or_else(||
                    elle_error!(self.location.borrow().error(
                        format!(
                            "Unexpected error when trying to compile the spread in struct '{}'",
                            Type::Struct(plain_name.clone()).display()
                        )
                    )
                ));

            let (final_ty, final_val) = if ty == spread_ty {
                (spread_ty.clone(), value)
            } else {
                convert_to_type(
                    gen,
                    ctx.func,
                    spread_ty.clone(),
                    ty.clone(),
                    value,
                    &location,
                    &location,
                    false,
                )
            };

            ctx.func.borrow_mut().add_instruction(Instruction::Blit(
                final_val,
                alloc_tmp.clone(),
                final_ty.size(ctx.module),
            ));
        }

        for (member_name, value) in self.values.iter().cloned() {
            if !member_names.contains(&member_name) {
                elle_error!(self.location.borrow().error(format!(
                    "Struct named '{}' has no field named '{member_name}'. Did you spell it correctly?",
                    Type::Struct(plain_name.clone()).display()
                )));
            }

            let (member_ty, offset) =
                member_to_offset(gen, ctx.module, &plain_name, &member_name).unwrap();

            let (mut ty, mut val) =
                value.compile(gen, &CodegenContext {
                    ty: members
                        .iter()
                        .find(|member| member.name == member_name)
                        .map(|arg| arg.r#type.clone()),
                    is_return: false,
                    ..ctx.clone()
                })
                .unwrap_or_else(||
                    elle_error!(self.location.borrow().error(
                        format!(
                            "Unexpected error when trying to compile the value of a field '{member_name}' in struct '{}'",
                            Type::Struct(plain_name.clone()).display()
                        )
                    )
                ));

            if let Some(member_ty) = member_ty {
                let (new_ty, new_val) = convert_to_type(
                    gen,
                    ctx.func,
                    ty.clone(),
                    member_ty.clone(),
                    val,
                    &self.location,
                    &self.location,
                    false,
                );

                ty = new_ty;
                val = new_val;
            }

            let offset_tmp = gen.new_temporary(Some("offset"), true);

            ctx.func.borrow_mut().assign_instruction(
                &offset_tmp,
                &Type::Long,
                Instruction::Add(
                    alloc_tmp.clone(),
                    Value::Const(String::new(), i128::from(offset)),
                ),
            );

            if ty.is_struct() {
                ctx.func.borrow_mut().add_instruction(Instruction::Blit(
                    val,
                    offset_tmp,
                    ty.size(ctx.module),
                ));
            } else {
                ctx.func
                    .borrow_mut()
                    .add_instruction(Instruction::Store(ty, offset_tmp, val));
            }
        }

        struct_hover!(self.name, members.is_empty(), struct_def.0, members);
        Some((ty, alloc_tmp))
    }
}
