use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        enums::{Type, Value},
    },
    parser::enums::Conversion,
};

impl Codegen<'_> for Conversion {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let (first, val) = self
            .value
            .compile(
                gen,
                &CodegenContext {
                    value: None,
                    is_return: false,
                    ..ctx.clone()
                },
            )
            .expect(&self.location.error(
                "Unexpected error when trying to compile the value of a conversion statement",
            ));

        Some(gen.convert_to_type(
            ctx.func,
            first,
            self.r#type.unwrap(),
            val,
            &self.location,
            &self.location,
            self.explicit,
        ))
    }
}
