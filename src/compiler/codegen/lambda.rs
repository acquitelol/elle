use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        primitive::function::generate_function,
        qbe::{r#type::Type, value::Value},
    },
    elle_error, hashmap,
    lexer::enums::{Token, TokenKind, ValueKind},
    misc::colors::{get_GREEN, get_RESET, GREEN, RESET},
    parser::enums::{Argument, AstNode, Conversion, Declare, FunctionSource, Lambda, Literal},
    LAMBDA_SHORTHAND_SCHEME,
};

impl Codegen<'_> for Lambda {
    fn compile(mut self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        gen.tmp_counter += 1;
        let lambda_name = format!("lambda.{}", gen.tmp_counter);

        let scopes = gen.scopes.clone();
        gen.scopes = vec![hashmap![]];
        let mut is_shorthand = false;

        if self.arguments.is_none() {
            is_shorthand = true;
            if let Some(ty) = ctx.ty.clone()
                && ty.is_function()
                && let Some(func) = ty.get_function_inner()
            {
                self.arguments = Some(
                    (0..func.arguments.len())
                        .map(|i| Ok(Token::from_ident(&format!(LAMBDA_SHORTHAND_SCHEME!(), i))))
                        .collect(),
                );
            } else {
                elle_error!(self.location.borrow().error("Failed to infer the number of arguments of this lambda.\nThis lambda cannot be created using the shorthand.\nPlease create it explicitly."))
            }
        }

        let patched_arguments = self.arguments.unwrap().into_iter().enumerate().map(|(i, arg)| match arg {
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

        if is_shorthand && patched_arguments.len() > 0 {
            let shorthand_name = format!(LAMBDA_SHORTHAND_SCHEME!(), "0");
            let shorthand_ty = patched_arguments[0].clone().r#type;

            self.value.insert(
                0,
                AstNode::Declare(Declare {
                    name: Token::from_ident("it"),
                    r#type: Some(Type::Infer),
                    value: Some(Box::new(AstNode::Conversion(Conversion {
                        r#type: Some(shorthand_ty.clone()),
                        // yes, Struct -> void * -> Struct is INTENTIONAL
                        // it is not intended to be Struct * -> void * -> Struct *
                        // it ensures that no shallow copy of the struct is made.
                        // elle allows turning a struct into its direct pointer
                        // representing it.
                        //
                        // as of 26/08/2026, version 0.91.1 of elle core,
                        // direct struct reassignments, i.e x := y where y
                        // is a struct, already doesn't copy. this is proofing
                        // for the future, because this is incorrect behaviour.
                        value: Box::new(if shorthand_ty.is_struct() {
                            AstNode::Conversion(Conversion {
                                r#type: Some(Type::Pointer(Box::new(Type::Void))),
                                value: Box::new(AstNode::Literal(Literal {
                                    kind: TokenKind::Identifier,
                                    value: ValueKind::String(shorthand_name),
                                    location: self.location.clone(),
                                    tagged: false,
                                })),
                                location: self.location.clone(),
                                explicit: true,
                            })
                        } else {
                            AstNode::Literal(Literal {
                                kind: TokenKind::Identifier,
                                value: ValueKind::String(shorthand_name),
                                location: self.location.clone(),
                                tagged: false,
                            })
                        }),
                        location: self.location.clone(),
                        explicit: true,
                    }))),
                    location: self.location.clone(),
                    value_location: self.location.clone(),
                }),
            );
        }

        let mut lambda_func = generate_function(
            FunctionSource {
                namespace_token: Token::from_ident(""),
                name_token: Token::from_ident(&lambda_name),
                name: lambda_name.clone(),
                public: false,
                variadic: self.variadic,
                external: false,
                builtin: false,
                volatile: false,
                format: false,
                unaliased: None,
                usable: true,
                imported: false,
                generics: vec![],
                arguments: patched_arguments,
                r#return: self
                    .return_ty
                    .or(ctx
                        .ty
                        .clone()
                        .and_then(|ty| ty.get_function_inner())
                        .and_then(|func| func.return_type))
                    .and_then(|ty| (!ty.has_generic_type()).then(|| ty)),
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
