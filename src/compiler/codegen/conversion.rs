use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        lib::convert::convert_to_type,
        qbe::{r#type::Type, value::Value},
    },
    elle_error,
    parser::enums::Conversion,
};

impl Codegen<'_> for Conversion {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let (first, val) = self.value.compile(gen, &ctx.to_nnf()).unwrap_or_else(|| {
            elle_error!(self.location.borrow().error(
                "Unexpected error when trying to compile the value of a conversion statement",
            ))
        });

        Some(convert_to_type(
            gen,
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
