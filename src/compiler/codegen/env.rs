use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        qbe::{
            data::Data, data_item::DataItem, instruction::Instruction, linkage::Linkage,
            r#type::Type, value::Value,
        },
    },
    elle_error,
    parser::enums::Environment,
    ENV_ID, ENV_STRUCT_NAME,
};

impl Codegen<'_> for Environment {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        if let Some(value) = self.value {
            if !gen
                .data_sections
                .iter()
                .find(|data| data.name == ENV_ID)
                .is_some()
            {
                gen.data_sections.push(Data {
                    linkage: Linkage::public(),
                    name: ENV_ID.into(),
                    align: None,
                    items: vec![(Type::Long, DataItem::Const(0))],
                })
            }

            let (ty, val) = value.compile(gen, ctx).unwrap_or_else(|| {
                elle_error!(&self
                    .location
                    .borrow()
                    .error("Unexpected error when compiling the value to assign to environment"))
            });

            ctx.func.borrow_mut().add_instruction(Instruction::Store(
                ty.clone(),
                Value::Global(ENV_ID.into()),
                val.clone(),
            ));

            Some((ty, val))
        } else {
            let ty = Type::Pointer(Box::new(Type::Struct(ENV_STRUCT_NAME.into())));
            let val = gen.new_temporary(None, false);

            ctx.func.borrow_mut().assign_instruction(
                &val,
                &ty,
                Instruction::Load(ty.clone(), Value::Global(ENV_ID.into())),
            );

            Some((ty, val))
        }
    }
}
