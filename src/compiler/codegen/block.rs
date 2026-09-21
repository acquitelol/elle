use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        qbe::{r#type::Type, value::Value},
    },
    hashmap,
    parser::enums::BlockStatement,
};

impl Codegen<'_> for BlockStatement {
    fn compile(self, compiler: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        compiler.scopes.push(hashmap![]);
        compiler.tmp_counter += 1;

        let body_label = format!("block.start.{}", compiler.tmp_counter);
        let end_label = format!("block.end.{}", compiler.tmp_counter);
        ctx.func.borrow_mut().add_block(body_label);

        for statement in self.body {
            statement.clone().compile(compiler, ctx);
        }

        ctx.func.borrow_mut().add_block(end_label);
        compiler.scopes.pop();
        None
    }
}
