use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        primitive::function::generate_function,
        qbe::{r#type::Type, value::Value},
    },
    hashmap,
    lexer::enums::Token,
    parser::enums::{FunctionSource, Lambda},
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
            let tmp = gen.new_variable(&ty, &argument.name, None, false, false);

            args.push((ty.into_abi(), tmp));
        }

        let lambda_func = generate_function(
            FunctionSource {
                namespace_token: Token::from_ident(""),
                name_token: Token::from_ident(&lambda_name),
                name: lambda_name.clone(),
                public: false,
                variadic: false,
                external: false,
                builtin: false,
                volatile: false,
                format: false,
                unaliased: None,
                usable: true,
                imported: false,
                generics: vec![],
                arguments: self.arguments,
                r#return: self.return_ty,
                body: self.value,
                location: self.location.clone(),
                return_location: self.location,
            },
            gen,
            true,
            false,
            hashmap![],
            ctx.module,
        );

        gen.deferred_functions.push(lambda_func.clone());
        gen.scopes = scopes;

        Some((
            Type::Function(Box::new(Some(lambda_func))),
            Value::Global(lambda_name),
        ))
    }
}
