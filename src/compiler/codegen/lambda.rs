use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        enums::{Type, Value},
    },
    hashmap,
    parser::enums::Lambda,
};

impl Codegen<'_> for Lambda {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        gen.tmp_counter += 1;
        let lambda_name = format!("lambda.{}", gen.tmp_counter);

        let scopes = gen.scopes.clone();
        gen.scopes = vec![hashmap![]];

        let mut args = vec![];

        for argument in self.arguments.clone() {
            let ty = argument.r#type.clone();
            let tmp = if argument.manual {
                gen.new_manual_argument(&ty, &argument.name)
            } else {
                gen.new_variable(&ty, &argument.name, None, false, false)
            };

            args.push((ty.into_abi(), tmp));
        }

        let lambda_func = gen.generate_function(
            lambda_name.clone(),
            false,
            false,
            false,
            false,
            false,
            false,
            false,
            true,
            None,
            true,
            false,
            vec![],
            hashmap![],
            &self.arguments,
            None,
            self.value,
            ctx.module,
            self.location.clone(),
            self.location,
        );

        gen.deferred_functions.push(lambda_func.clone());
        gen.scopes = scopes;

        Some((
            Type::Function(Box::new(Some(lambda_func))),
            Value::Global(lambda_name),
        ))
    }
}
