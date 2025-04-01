use std::collections::HashSet;

use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        enums::{Instruction, Type, Value},
        lib::field_utils::member_to_offset,
    },
    elle_error, is_generic,
    parser::enums::StructLiteral,
    Warning,
};

impl Codegen<'_> for StructLiteral {
    fn compile(mut self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let inner = ctx.ty.clone().unwrap_or(
            ctx.func
                .borrow_mut()
                .return_type
                .clone()
                .unwrap_or(Type::Void),
        );

        if inner.is_struct()
            && is_generic!(inner.get_struct_inner().unwrap())
            && !is_generic!(self.name)
        {
            let generic_name = Type::from_internal_id(inner.get_struct_inner().unwrap()).0;

            if self.name == generic_name {
                self.name = inner.get_struct_inner().unwrap();
            }
        }

        if gen.struct_pool.get(&self.name).is_none() {
            if is_generic!(self.name) {
                gen.create_monomorphized_struct(ctx.module, self.name.clone())
            } else {
                elle_error!(
                    self.location.error(format!(
                        "Could not find struct named '{}'. Did you spell it correctly?\nThis struct may be generic but missing generic parameters.",
                        Type::Struct(self.name).display()
                    ))
                )
            }
        }

        let td = ctx
            .module
            .borrow()
            .types
            .clone()
            .into_iter()
            .find(|td| td.name == self.name)
            .expect(&format!("Unable to find struct named '{}'", self.name));

        if !td.usable && !ctx.func.borrow_mut().imported {
            elle_error!(self.location.error(format!(
                "Struct named '{}' was not imported and can't be used",
                Type::Struct(self.name.clone()).display()
            )))
        }

        let struct_pool = gen.struct_pool.clone();
        let members = struct_pool.get(&self.name).unwrap().1.clone();
        let member_names = members
            .iter()
            .map(|member| member.name.clone())
            .collect::<Vec<String>>();

        let member_set: HashSet<_> = member_names.iter().cloned().collect();
        let value_set: HashSet<_> = self.values.iter().map(|value| value.0.clone()).collect();

        let diff: Vec<_> = member_set.difference(&value_set).collect();

        if gen.warnings.has_warning(Warning::StructFieldsMissing) {
            for member in diff.iter().cloned() {
                println!(
                    "{}",
                    self.location.warning(format!(
                        "Declaring struct '{}' without field '{}'",
                        Type::Struct(self.name.clone()).display(),
                        member
                    ))
                );
            }
        }

        let ty = Type::Struct(self.name.clone());
        let size = ty.size(ctx.module);

        let alloc_tmp = gen.new_temporary(Some(&format!("struct.{}", self.name)), true);

        #[cfg(debug_assertions)]
        ctx.func
            .borrow_mut()
            .add_instruction(Instruction::Comment(format!("size of :{}", self.name)));

        ctx.func.borrow_mut().assign_instruction_front(
            &alloc_tmp,
            &Type::Long,
            Instruction::Alloc8(Value::Const("".into(), size as i128)),
        );

        for (member_name, value) in self.values.iter().cloned() {
            if !member_names.contains(&member_name) {
                elle_error!(self.location.error(format!(
                    "Struct named '{}' has no field named '{}'. Did you spell it correctly?",
                    self.name, member_name
                )));
            }

            let (member_ty, offset) =
                member_to_offset(gen, ctx.module, &self.name, &member_name).unwrap();

            let (mut ty, mut val) =
                value.compile(gen, &CodegenContext {
                    ty: members
                        .iter()
                        .find(|member| member.name == member_name)
                        .map(|arg| arg.r#type.clone()),
                    ..ctx.clone()
                })
                .expect(
                    &self.location.error(
                        format!("Unexpected error when trying to compile the value of a field '{}' in struct '{}'", member_name, self.name)
                    ),
                );

            if let Some(member_ty) = member_ty {
                if ty.weight() > member_ty.weight() || ty.weight() < member_ty.weight() {
                    let (new_ty, new_val) = gen.convert_to_type(
                        ctx.func,
                        ty.clone(),
                        member_ty.clone(),
                        val,
                        &self.location,
                        &self.location,
                        false,
                    );

                    ty = new_ty;
                    val = new_val
                }
            }

            let offset_tmp = gen.new_temporary(Some("offset"), true);

            ctx.func.borrow_mut().assign_instruction(
                &offset_tmp,
                &Type::Long,
                Instruction::Add(alloc_tmp.clone(), Value::Const("".into(), offset as i128)),
            );

            if ty.is_struct() {
                ctx.func.borrow_mut().add_instruction(Instruction::Call(
                    Value::Global("memcpy".into()),
                    // The structs must have their pointers diminished
                    // to just a `Long` instead of a `Struct(name)`
                    vec![
                        (Type::Long, offset_tmp),
                        (Type::Long, val),
                        (
                            Type::Word,
                            Value::Const("".into(), ty.size(ctx.module) as i128),
                        ),
                    ],
                ))
            } else {
                ctx.func
                    .borrow_mut()
                    .add_instruction(Instruction::Store(ty, offset_tmp, val))
            }
        }

        Some((ty, alloc_tmp))
    }
}
