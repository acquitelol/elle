use std::{cell::RefCell, rc::Rc};

use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        enums::{Function, Instruction, Linkage, Type, Value},
        lib::{convert::convert_to_type, meta_struct::generate_meta_struct},
    },
    elle_error, get_GREEN, get_POINTER_ID, get_RESET, hashmap, is_generic,
    lexer::enums::{TokenKind, ValueKind},
    parser::enums::{Address, AstNode, FunctionCall, Literal},
    unknown_function, DUNDER_CONSTANTS, FORMAT_CONSTANT, GREEN, META_STRUCT_NAME, POINTER_ID,
    PTR_PRIORITY_CONSTANTS, RESET, VOID_POINTER_ID,
};

impl Codegen<'_> for FunctionCall {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let FunctionCall {
            mut name,
            // Generics passed by the caller
            // ie foo<i32>()
            // !! these ones can be indexed !!
            generics: base_known_generics,
            parameters,
            type_method,
            ignore_no_def,
            location: call_location,
        } = self;

        let mut call_location = (*call_location).clone();
        let declarative_ty = ctx.ty.clone().unwrap_or(Type::Void);
        let mut should_get_address = false; // Gets address if first arg's ty is the same as ty predicate
        let mut first_param = None;
        let mut known_generics = hashmap![String, Type];

        if type_method {
            let parameter = parameters.get(0).expect(
                &call_location
                    .error("Tried to get the 0th parameter to parse struct call but failed"),
            );

            let (mut ty, val) = parameter.1.clone().compile(gen, ctx)
                .expect(&parameter.0.error(
                    format!(
                        "Unexpected error when trying to generate a statement for a parameter in a function called '{}'",
                        name
                    ))
                );

            // The first param needs to be compiled to get its type
            // however if the first param is mutating (ie `yield()`)
            // then there will be a double yield causing many issues.
            // Therefore, we store the first param here to be
            // used later when compiling all of the params instead
            // of compiling the first parameter again
            first_param = Some((ty.clone(), val));

            if ty.is_pointer() {
                let inner = ty.get_pointer_inner().unwrap();

                if inner.is_struct() && is_generic!(inner.get_struct_inner().unwrap()) {
                    ty = Type::Pointer(Box::new(Type::Struct(
                        Type::from_internal_id(inner.get_struct_inner().unwrap()).0,
                    )));
                }
            } else if ty.is_struct() && is_generic!(ty.get_struct_inner().unwrap()) {
                ty = Type::Struct(Type::from_internal_id(ty.get_struct_inner().unwrap()).0);
            }

            // struct access
            if ty.is_struct() {
                name = format!("{}.{}", ty.get_struct_inner().unwrap(), name)
            // string access
            } else if ty.is_string() {
                name = format!("string.{}", name)
            // string* access
            } else if ty.is_pointer() && ty.get_pointer_inner().unwrap().is_string() {
                name = format!("string.{}", name)
            // void* access
            } else if ty.is_void_pointer() {
                name = format!("{}.{}", VOID_POINTER_ID, name)
            // dunder access
            } else if ty.is_pointer()
                && PTR_PRIORITY_CONSTANTS.contains(&name.as_str())
                && type_method
            {
                name = format!("{}.{}", get_POINTER_ID!(), name)
            // struct* access
            } else if ty.is_pointer() && ty.get_pointer_inner().unwrap().is_struct() {
                name = format!(
                    "{}.{}",
                    ty.get_pointer_inner().unwrap().get_struct_inner().unwrap(),
                    name
                )
            // void* access
            } else if ty.is_void_pointer() {
                name = format!("{}.{}", VOID_POINTER_ID, name)
            // dunder access
            } else if ty.is_pointer() && DUNDER_CONSTANTS.contains(&name.as_str()) && type_method {
                name = format!("{}.{}", get_POINTER_ID!(), name)
            } else {
                name = format!("{}.{}", ty.id(), name)
            }
        }

        let tmp_function_option = ctx
            .module
            .borrow()
            .functions
            .iter()
            .find(|function| function.name == name)
            .cloned();
        let mut is_callback = false;

        let mut tmp_function = if let Some(func) = tmp_function_option {
            func
        } else {
            // Function could be a callback pointer
            let callback = gen.get_variable(name.as_str(), Some(ctx.func), Some(ctx.module));

            let fallback = Function {
                linkage: Linkage::public(),
                name: name.clone(),
                variadic: false,
                manual: false,
                external: false,
                builtin: false,
                volatile: false,
                format: false,
                lambda: true,
                unaliased: None,
                usable: true,
                imported: false,
                generics: vec![],
                known_generics: hashmap![],
                // TODO: Allow the function declaration to specify a real signature instead of just `fn *`
                arguments: parameters
                    .iter()
                    .map(|param| {
                        let mut tmp_func = Function::default();
                        tmp_func.add_block("start");

                        (
                            param.1.clone().compile(
                                gen,
                                &CodegenContext {
                                    func: &RefCell::new(tmp_func),
                                    ..ctx.clone()
                                },
                            ).expect(&param.0.error(
                                format!(
                                    "Unexpected error when trying to generate a statement for a parameter in a function called '{}'",
                                    name
                                ))
                            ),
                            false,
                        )
                    })
                    .collect(),
                return_type: Some(declarative_ty.clone()),
                blocks: vec![],
            };

            if let Ok((ty, _)) = callback {
                if (ty.is_pointer()
                    && ty.get_pointer_inner().unwrap().is_unknown()
                    && ty.get_pointer_inner().unwrap().get_unknown_inner().unwrap() == "fn")
                    || ignore_no_def
                    || gen.generic_functions.contains_key(&name)
                {
                    is_callback = true;
                    fallback
                } else if ty.is_function() {
                    is_callback = true;

                    // We know the function exists
                    ty.get_function_inner().unwrap().unwrap()
                } else {
                    unknown_function!(call_location, name, ctx.module)
                }
            } else if ignore_no_def {
                is_callback = true;
                fallback
            } else {
                unknown_function!(call_location, name, ctx.module)
            }
        };

        if let Some(unaliased_name) = tmp_function.unaliased.clone() {
            name = unaliased_name;
        };

        if !tmp_function.usable && !ctx.func.borrow_mut().imported && !ignore_no_def {
            elle_error!(call_location.error(format!(
                "Function named '{}' was not imported and can't be used",
                name
            )))
        }

        let mut params: Vec<((Type, Value), bool)> = vec![];
        let mut add_meta = false;

        if let Some(inner) = tmp_function.arguments.get(0) {
            if inner.0 .0.is_struct() {
                let name = inner.0 .0.get_struct_inner().unwrap();

                if name == META_STRUCT_NAME {
                    add_meta = true;
                }
            }
        }

        if gen.generic_functions.contains_key(&name) {
            gen.create_monomorphized_function(
                &mut name,
                &mut add_meta,
                base_known_generics,
                &mut known_generics,
                parameters.clone(),
                ctx.module,
                ctx.func,
                &mut call_location,
                &mut tmp_function,
                ctx.ty.clone(),
            )
        }

        if type_method {
            if let Some((ty, _)) = first_param.clone() {
                let parsed_ty = if ty.is_struct() && is_generic!(ty.get_struct_inner().unwrap()) {
                    Type::Struct(Type::from_internal_id(ty.get_struct_inner().unwrap()).0)
                } else {
                    ty.clone()
                };

                // struct access
                if parsed_ty.is_struct() {
                    should_get_address = true;
                // string access
                } else if parsed_ty.is_string() {
                    should_get_address = true;
                }
            }
        }

        for (i, mut parameter) in parameters.iter().cloned().enumerate() {
            let param_ty = {
                let tmp = tmp_function.arguments.get(i + add_meta as usize);

                if tmp.is_some() {
                    tmp.map(|item| item.0 .0.clone())
                } else {
                    None
                }
            };

            let first_arg = tmp_function.arguments.get(0 + add_meta as usize);
            let mut got_address = false;

            if let Some(first_arg) = first_arg {
                if i == 0
                    && type_method
                    && should_get_address
                    && first_param.is_some()
                    && first_arg.0 .0.is_pointer()
                    && (first_arg.0 .0.get_pointer_inner().unwrap()
                        == first_param.clone().unwrap().0)
                {
                    got_address = true;

                    parameter.1 = AstNode::Address(Address {
                        value: Box::new(parameter.1),
                        location: Rc::new(call_location.clone()),
                    })
                }
            }

            let (ty, val) = if i == 0 && first_param.is_some() && !got_address {
                first_param.clone().unwrap()
            } else {
                parameter.1.compile(gen, &CodegenContext {
                    ty: param_ty.clone(),
                    ..ctx.to_nnf()
                })
                .expect(&parameter.0.error(
                    format!(
                        "Unexpected error when trying to generate a statement for a parameter in a function called '{}'",
                        name
                    ))
                )
            };

            let no_fmt = tmp_function
                .arguments
                .get(i + add_meta as usize)
                .map(|x| x.1)
                .unwrap_or(false);

            params.push(if param_ty.is_none() || ty == param_ty.clone().unwrap() {
                ((ty, val), no_fmt)
            } else {
                (
                    convert_to_type(
                        gen,
                        ctx.func,
                        ty.into_abi(),
                        param_ty.unwrap(),
                        val,
                        &parameter.0,
                        &parameter.0,
                        false,
                    ),
                    no_fmt,
                )
            });
        }

        let meta_struct = generate_meta_struct(
            ctx.func,
            &params,
            parameters.clone(),
            Rc::new(call_location.clone()),
        );

        if tmp_function.format {
            for (i, ((ty, val), no_fmt)) in params.iter_mut().enumerate() {
                if *no_fmt {
                    continue;
                }

                let fmt_tmp;
                let fmt_ty;
                let mut func_name;
                let mut tmp_function;

                if ty.is_struct() {
                    let struct_name = ty.get_struct_inner().unwrap();
                    func_name = format!("{struct_name}.{FORMAT_CONSTANT}");

                    fmt_tmp = gen.new_temporary(Some(&format!("{struct_name}.fmt")), false);
                    fmt_ty = Type::Pointer(Box::new(Type::Char));
                    tmp_function = ctx
                        .module
                        .borrow()
                        .functions
                        .iter()
                        .find(|func| func.name == func_name)
                        .cloned()
                        .unwrap_or(Function::default());

                    if is_generic!(struct_name) {
                        let (real_struct_name, _) = Type::from_internal_id(struct_name);
                        func_name = format!("{real_struct_name}.{FORMAT_CONSTANT}");

                        if gen.generic_functions.contains_key(&func_name) {
                            gen.create_monomorphized_function(
                                &mut func_name,
                                &mut false,
                                vec![],
                                &mut hashmap!(),
                                vec![
                                    parameters[i].clone(),
                                    (
                                        Rc::new(call_location.clone()),
                                        AstNode::Literal(Literal {
                                            kind: TokenKind::IntegerLiteral,
                                            value: ValueKind::Number(0),
                                            location: Rc::new(call_location.clone()),
                                        }),
                                    ),
                                ],
                                ctx.module,
                                ctx.func,
                                &mut call_location,
                                &mut tmp_function,
                                None,
                            )
                        }
                    }
                } else {
                    func_name = format!("{}.{FORMAT_CONSTANT}", ty.strict_id());

                    fmt_tmp = gen.new_temporary(Some(&format!("{}.fmt", ty.strict_id())), false);
                    fmt_ty = Type::Pointer(Box::new(Type::Char));
                    tmp_function = ctx
                        .module
                        .borrow()
                        .functions
                        .iter()
                        .find(|func| func.name == func_name)
                        .cloned()
                        .unwrap_or(Function::default());

                    if gen.generic_functions.contains_key(&func_name) {
                        gen.create_monomorphized_function(
                            &mut func_name,
                            &mut false,
                            vec![],
                            &mut hashmap!(),
                            vec![
                                parameters[i].clone(),
                                (
                                    Rc::new(call_location.clone()),
                                    AstNode::Literal(Literal {
                                        kind: TokenKind::IntegerLiteral,
                                        value: ValueKind::Number(0),
                                        location: Rc::new(call_location.clone()),
                                    }),
                                ),
                            ],
                            ctx.module,
                            ctx.func,
                            &mut call_location,
                            &mut tmp_function,
                            None,
                        )
                    }
                }

                if tmp_function
                    .return_type
                    .as_ref()
                    .is_none_or(|ty| !ty.is_string())
                {
                    elle_error!(
                        call_location.error(format!(
                            "The method \"{}\" returns {}{RESET} but it should return {GREEN}string{RESET}.\nThis method's implementation must be changed to return {GREEN}string{RESET}.",
                            func_name,
                            tmp_function.return_type.unwrap_or(Type::Unknown("_".into())).display(),
                            GREEN = get_GREEN!(),
                            RESET = get_RESET!(),
                        ))
                    )
                }

                ctx.func.borrow_mut().assign_instruction(
                    &fmt_tmp,
                    &fmt_ty,
                    Instruction::Call(
                        Value::Global(func_name),
                        vec![
                            (ty.clone(), val.clone()),
                            (Type::Word, Value::Const("".into(), 0)),
                        ],
                    ),
                );

                *ty = fmt_ty;
                *val = fmt_tmp;
            }
        }

        let ty = tmp_function.return_type.clone().unwrap_or(declarative_ty);

        if add_meta {
            let res = meta_struct
                .compile(
                    gen,
                    &CodegenContext {
                        ty: Some(ty.clone()),
                        value: None,
                        is_return: false,
                        ..ctx.clone()
                    },
                )
                .expect(
                    &call_location
                        .error("Unexpected error when trying to compile the Elle metadata struct"),
                );

            params.insert(0, (res, false));
        }

        if tmp_function.variadic {
            let node = AstNode::Literal(Literal {
                kind: TokenKind::ExactLiteral,
                value: ValueKind::String("...".into()),
                location: Rc::new(call_location.clone()),
            });

            let res = node
                .compile(
                    gen,
                    &CodegenContext {
                        ty: Some(ty.clone()),
                        value: None,
                        is_return: false,
                        ..ctx.clone()
                    },
                )
                .expect(
                    &call_location.error(
                        "Unexpected error when trying to compile the variadic literal '...'",
                    ),
                );

            params.insert(tmp_function.arguments.len(), (res, false));
        }

        if !tmp_function.variadic {
            if tmp_function.arguments.len() != params.len() {
                let only = if tmp_function.arguments.len() > params.len() && params.len() != 0 {
                    "only "
                } else {
                    ""
                };

                let name = if is_generic!(tmp_function.name) {
                    let mut parts = tmp_function.name.split(".").map(|x| x.to_string());
                    let mut name = parts.next().unwrap();

                    if type_method {
                        name.push_str(&format!("::{}", parts.next().unwrap()));
                    }

                    name.push_str(&format!(
                        "<{}>",
                        known_generics
                            .iter()
                            .map(|generic| generic.1.display())
                            .collect::<Vec<String>>()
                            .join(", ")
                    ));

                    name
                } else {
                    tmp_function.name
                };

                let arg_len = tmp_function
                    .arguments
                    .len()
                    .saturating_sub(add_meta as usize)
                    .saturating_sub(type_method as usize);
                let param_len = params.len().saturating_sub(add_meta as usize);

                elle_error!(call_location
                    .with_extra_info(if tmp_function.arguments.is_empty() && type_method {
                        format!(
                            "Use `{}({})` instead here",
                            name.replace(".", "::"),
                            if arg_len > 0 { "..." } else { "" }
                        )
                    } else {
                        "".into()
                    })
                    .error(format!(
                        "Function named `{}({})` takes {} argument{}, but you {}passed {}\n{}",
                        name.replace(".", "::"),
                        if arg_len > 0 { "..." } else { "" },
                        arg_len,
                        if arg_len == 1 { "" } else { "s" },
                        only,
                        param_len,
                        if tmp_function.arguments.is_empty() && type_method {
                            format!(
                                "This function does't accept a `{} self` parameter.",
                                first_param
                                    .expect("This function is a type method")
                                    .0
                                    .display()
                            )
                        } else {
                            tmp_function
                                .arguments
                                .iter()
                                .skip(params.len())
                                .map(|((ty, val), _)| {
                                    format!(
                                        "Missing argument named \"{}\" (of type \"{}\")",
                                        val.get_string_inner()
                                            .replace("%", "")
                                            .split(".")
                                            .nth(0)
                                            .unwrap(),
                                        ty.display()
                                    )
                                })
                                .collect::<Vec<String>>()
                                .join("\n")
                        }
                    )))
            }
        }

        let temp = gen.new_temporary(None, true);
        let val = if is_callback {
            let tmp = gen.new_temporary(None, true);
            let res = gen.get_variable(&format!("{}.addr", name), Some(ctx.func), Some(ctx.module));

            if let Ok((_, addr_val)) = res {
                ctx.func.borrow_mut().assign_instruction(
                    &tmp,
                    &Type::Long,
                    Instruction::Load(Type::Long, addr_val),
                );

                tmp
            } else {
                gen.get_variable(&name, Some(ctx.func), Some(ctx.module))
                    .unwrap_or((Type::Long, Value::Global(name)))
                    .1
            }
        } else {
            gen.get_variable(&name, Some(ctx.func), Some(ctx.module))
                .unwrap_or((Type::Long, Value::Global(name)))
                .1
        };

        ctx.func.borrow_mut().assign_instruction(
            &temp,
            &ty,
            Instruction::Call(val, params.into_iter().map(|x| x.0).collect()),
        );

        Some((ty, temp))
    }
}
