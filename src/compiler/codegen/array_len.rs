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
        let (mut ty, _) = self.value.clone().compile(gen, ctx).unwrap();

        loop {
            if let Type::StaticArray(inner, _) = ty.clone()
                && let Type::StaticArray(..) = *inner
            {
                ty = *inner;
            } else {
                break;
            }
        }

        if let Type::StaticArray(_, size) = ty
            && let Type::Size(size) = *size
        {
            return Some((
                Type::UnsignedLong,
                Value::Const(String::new(), size as i128),
            ));
        }

        return None;
    }
}
