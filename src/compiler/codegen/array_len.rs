/// ! EXCLUSIVELY FOR STATIC ARRAYS !
/// THIS DOESNT WORK FOR DYNAMIC ARRAYS
use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        qbe::{r#type::Type, value::Value},
    },
    parser::enums::ArrayLength,
};

impl Codegen<'_> for ArrayLength {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let (ty, _) = self.value.clone().compile(gen, ctx).unwrap();

        if let Type::StaticArray(_, size) = ty {
            return Some((
                Type::UnsignedLong,
                Value::Const(String::new(), size as i128),
            ));
        }

        return None;
    }
}
