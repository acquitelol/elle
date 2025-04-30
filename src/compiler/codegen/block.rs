use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        qbe::{r#type::Type, value::Value},
    },
    hashmap,
    parser::enums::BlockStatement,
};

impl Codegen<'_> for BlockStatement {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        gen.scopes.push(hashmap![]);
        gen.tmp_counter += 1;

        let body_label = format!("block.start.{}", gen.tmp_counter);
        let end_label = format!("block.end.{}", gen.tmp_counter);
        ctx.func.borrow_mut().add_block(body_label);

        for statement in self.body {
            statement.clone().compile(gen, ctx);
        }

        ctx.func.borrow_mut().add_block(end_label);
        gen.scopes.pop();
        None
    }
}
