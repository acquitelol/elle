use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        primitive::function::generate_function,
        qbe::{r#type::Type, value::Value},
    },
    elle_error, hashmap,
    lexer::enums::Token,
    misc::colors::{get_GREEN, get_RESET, GREEN, RESET},
    parser::enums::{Argument, FunctionSource, Lambda},
};

impl Codegen<'_> for Lambda {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        gen.tmp_counter += 1;
        let lambda_name = format!("lambda.{}", gen.tmp_counter);

        let scopes = gen.scopes.clone();
        gen.scopes = vec![hashmap![]];

        let patched_arguments = self.arguments.into_iter().enumerate().map(|(i, arg)| match arg {
            Ok(name) => {
                let normal_name = name.value.get_string_inner().unwrap();

                if let Some(ty) = &ctx.ty && let Some(inner) = ty.get_function_inner() && inner.arguments.len() > i {
                    if name.tagged && !inner.arguments[i].0.0.has_generic_type() {
                        elle_error!(format!(
                            "hover\n{}\n{}\nlet {}: {}",
                            self.location.borrow().display_plain(false),
                            self.location.borrow().display_plain(true),
                            normal_name.replace('.', "::"),
                            inner.arguments[i].0.0.clone().display()
                        ));
                    }

                    Argument {
                        name: normal_name,
                        r#type: inner.arguments[i].0.0.clone(),
                        is_unused: false,
                        no_fmt: false
                    }
                } else {
                    elle_error!(name
                        .location
                        .borrow()
                        .error(format!("Could not automatically infer a type for {normal_name}.\nPlease explicitly declare it: `{GREEN}T {normal_name}{RESET}`",
                            GREEN = get_GREEN!(),
                            RESET = get_RESET!())))
                }
            }
            Err(arg) => arg,
        }).collect::<Vec<_>>();

        let mut lambda_func = generate_function(
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
                arguments: patched_arguments,
                r#return: self.return_ty,
                body: if ctx.is_generic { vec![] } else { self.value },
                location: self.location.clone(),
                return_location: self.location,
            },
            gen,
            true,
            false,
            hashmap![],
            ctx.module,
        );

        if ctx.is_generic
            && let Some(ty) = &ctx.ty
            && let Some(inner) = ty.get_function_inner()
        {
            lambda_func.return_type = inner.return_type;
        }

        gen.deferred_functions.push(lambda_func.clone());
        gen.scopes = scopes;

        Some((
            Type::Function(Box::new(Some(lambda_func))),
            Value::Global(lambda_name),
        ))
    }
}
