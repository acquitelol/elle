use std::cell::RefCell;
use std::fmt::Write;

use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler, VariableInfo},
        lib::{
            convert::convert_to_type, meta_struct::generate_meta_struct,
            mono_function::create_monomorphized_function,
        },
        qbe::{
            function::Function, instruction::Instruction, linkage::Linkage, r#type::Type,
            value::Value,
        },
    },
    elle_error, get_GREEN, get_POINTER_ID, get_RESET, hashmap, is_generic,
    lexer::enums::{TokenKind, ValueKind},
    parser::enums::{Address, AstNode, FunctionCall, Literal},
    struct_hover, unknown_function, DUNDER_CONSTANTS, FORMAT_CONSTANT, GREEN, META_STRUCT_NAME,
    POINTER_ID, PTR_PRIORITY_CONSTANTS, RESET, VOID_POINTER_ID,
};

impl Codegen<'_> for FunctionCall {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let Self {
            namespace_token,
            name_token,
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

        let declarative_ty = ctx.ty.clone().unwrap_or(Type::Void);
        let mut should_get_address = false; // Gets address if first arg's ty is the same as ty predicate
        let mut first_param = None;
        let mut known_generics = hashmap![String, Type];

        if type_method {
            let parameter = parameters.first().unwrap_or_else(|| {
                elle_error!(call_location
                    .borrow()
                    .error("Tried to get the 0th parameter to parse struct call but failed"))
            });

            let (mut ty, val) = parameter.1.clone().compile(gen, ctx)
                .unwrap_or_else(|| {
                    elle_error!(parameter
                        .0
                        .borrow()
                        .error(format!(
                            "Unexpected error when trying to generate a statement for a parameter in a function called '{name}'"
                        )))
                });

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
                        Type::from_internal_id(&inner.get_struct_inner().unwrap()).0,
                    )));
                }
            } else if ty.is_struct() && is_generic!(ty.get_struct_inner().unwrap()) {
                ty = Type::Struct(Type::from_internal_id(&ty.get_struct_inner().unwrap()).0);
            }

            // struct access
            if ty.is_struct() {
                name = format!("{}.{name}", ty.get_struct_inner().unwrap());
            // enum access
            } else if ty.is_enum() {
                name = format!("{}.{name}", ty.get_enum_inner().unwrap());
            // string access
            } else if ty.is_string() {
                name = format!("string.{name}");
            // void* access
            } else if ty.is_void_pointer() {
                name = format!("{VOID_POINTER_ID}.{name}");
            // dunder access
            } else if ty.is_pointer()
                && PTR_PRIORITY_CONSTANTS.contains(&name.as_str())
                && type_method
            {
                name = format!("{}.{name}", get_POINTER_ID!());
            // string* access
            } else if ty.is_pointer() && ty.get_pointer_inner().unwrap().is_string() {
                name = format!("string.{name}");
            // struct* access
            } else if ty.is_pointer() && ty.get_pointer_inner().unwrap().is_struct() {
                name = format!(
                    "{}.{name}",
                    ty.get_pointer_inner().unwrap().get_struct_inner().unwrap()
                );
            // enum* access
            } else if ty.is_pointer() && ty.get_pointer_inner().unwrap().is_enum() {
                name = format!(
                    "{}.{name}",
                    ty.get_pointer_inner().unwrap().get_enum_inner().unwrap()
                );
            // dunder access
            } else if ty.is_pointer() && DUNDER_CONSTANTS.contains(&name.as_str()) && type_method {
                name = format!("{}.{name}", get_POINTER_ID!());
            } else {
                name = format!("{}.{name}", ty.id());
            }
        }

        let tmp_function_option = ctx
            .module
            .borrow()
            .functions
            .get(&name)
            .filter(|function| !function.constant)
            .cloned();
        let mut is_callback = false;

        let mut tmp_function = if let Some(func) = tmp_function_option {
            func
        } else {
            // Function could be a callback pointer
            let callback = gen
                .get_variable(
                    &name,
                    Some(ctx.func),
                    Some(ctx.module),
                    &VariableInfo {
                        dont_call_constants: true,
                    },
                )
                .map(|(ty, val)| {
                    let ty = match ty {
                        Type::Function(inner) if inner.is_some() => {
                            let mut dup = inner.unwrap();
                            dup.name.clone_from(&name);
                            Type::Function(Box::new(Some(dup)))
                        }
                        other => other,
                    };

                    (ty, val)
                });

            let fallback = Function {
                linkage: Linkage::public(),
                name: name.clone(),
                constant: false,
                variadic: false,
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
                            ).unwrap_or_else(|| elle_error!(param.0.borrow().error(
                                format!(
                                    "Unexpected error when trying to generate a statement for a parameter in a function called '{name}'"
                                )))),
                            false,
                        )
                    })
                    .collect(),
                return_type: Some(declarative_ty.clone()),
                blocks: vec![],
            };

            match callback {
                Ok((ty, _)) => {
                    if (ty.is_pointer() && ty.is_function())
                        || ignore_no_def
                        || gen.generic_functions.contains_key(&name)
                    {
                        is_callback = true;
                        fallback
                    } else if ty.is_function() {
                        is_callback = true;
                        ty.get_function_inner().unwrap().unwrap()
                    } else {
                        unknown_function!(call_location, name, ctx.module)
                    }
                }
                Err(_) if ignore_no_def => {
                    is_callback = true;
                    fallback
                }
                _ => unknown_function!(call_location, name, ctx.module),
            }
        };

        if let Some(unaliased_name) = tmp_function.unaliased.clone() {
            name = unaliased_name;
        }

        if !tmp_function.usable && !ctx.func.borrow_mut().imported && !ignore_no_def {
            elle_error!(call_location.borrow().error(format!(
                "Function named '{name}' was not imported and can't be used"
            )))
        }

        let mut params: Vec<((Type, Value), bool)> = vec![];
        let mut add_meta = false;

        if let Some(inner) = tmp_function.arguments.first() {
            if inner.0 .0.is_struct() {
                let name = inner.0 .0.get_struct_inner().unwrap();

                if name == META_STRUCT_NAME {
                    add_meta = true;
                }
            }
        }

        if gen.generic_functions.contains_key(&name) {
            create_monomorphized_function(
                gen,
                &mut name,
                &mut add_meta,
                &base_known_generics,
                &mut known_generics,
                &parameters,
                ctx.module,
                ctx.func,
                &call_location,
                &mut tmp_function,
                ctx.ty.clone(),
            );
        }

        if namespace_token.tagged {
            let plain_name = namespace_token.value.get_string_inner().unwrap();
            let (_, members, _) = gen.struct_pool.get(&plain_name).unwrap();
            struct_hover!(namespace_token, members.is_empty(), members);
        }

        if name_token.tagged {
            elle_error!(format!(
                "hover\n{}\n{}\n{}",
                name_token.location.borrow().display_plain(false),
                name_token.location.borrow().display_plain(true),
                Type::Function(Box::new(Some(tmp_function))).display()
            ));
        }

        if type_method {
            if let Some((ty, _)) = first_param.clone() {
                let parsed_ty = if ty.is_struct() && is_generic!(ty.get_struct_inner().unwrap()) {
                    Type::Struct(Type::from_internal_id(&ty.get_struct_inner().unwrap()).0)
                } else {
                    ty
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
                let tmp = tmp_function.arguments.get(i + usize::from(add_meta));
                tmp.map(|item| item.0 .0.clone())
            };

            let first_arg = tmp_function.arguments.get(usize::from(add_meta));
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
                        location: call_location.clone(),
                    });
                }
            }

            let (ty, val) = if i == 0 && first_param.is_some() && !got_address {
                first_param.clone().unwrap()
            } else {
                parameter.1.compile(gen, &CodegenContext {
                    ty: param_ty.clone(),
                    ..ctx.to_nnf()
                })
                .unwrap_or_else(|| elle_error!(parameter.0.borrow().error(
                    format!(
                        "Unexpected error when trying to generate a statement for a parameter in a function called '{name}'"
                    ))))
            };

            let no_fmt = tmp_function
                .arguments
                .get(i + usize::from(add_meta))
                .is_some_and(|x| x.1);

            params.push(
                if let Some(param_ty) = param_ty
                    && param_ty != ty
                {
                    (
                        convert_to_type(
                            gen,
                            ctx.func,
                            ty,
                            param_ty,
                            val,
                            &parameter.0,
                            &parameter.0,
                            false,
                        ),
                        no_fmt,
                    )
                } else {
                    ((ty, val), no_fmt)
                },
            );
        }

        let meta_struct =
            generate_meta_struct(ctx.func, &params, &parameters, call_location.clone());

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
                        .get(&func_name)
                        .cloned()
                        .unwrap_or_else(Function::default);

                    if is_generic!(struct_name) {
                        let (real_struct_name, _) = Type::from_internal_id(&struct_name);
                        func_name = format!("{real_struct_name}.{FORMAT_CONSTANT}");

                        if gen.generic_functions.contains_key(&func_name) {
                            create_monomorphized_function(
                                gen,
                                &mut func_name,
                                &mut false,
                                &[],
                                &mut hashmap!(),
                                &[
                                    parameters[i].clone(),
                                    (
                                        call_location.clone(),
                                        AstNode::Literal(Literal {
                                            kind: TokenKind::IntegerLiteral,
                                            value: ValueKind::Number(0),
                                            location: call_location.clone(),
                                            tagged: false,
                                        }),
                                    ),
                                ],
                                ctx.module,
                                ctx.func,
                                &call_location,
                                &mut tmp_function,
                                None,
                            );
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
                        .get(&func_name)
                        .cloned()
                        .unwrap_or_else(Function::default);

                    if gen.generic_functions.contains_key(&func_name) {
                        create_monomorphized_function(
                            gen,
                            &mut func_name,
                            &mut false,
                            &[],
                            &mut hashmap!(),
                            &[
                                parameters[i].clone(),
                                (
                                    call_location.clone(),
                                    AstNode::Literal(Literal {
                                        kind: TokenKind::IntegerLiteral,
                                        value: ValueKind::Number(0),
                                        location: call_location.clone(),
                                        tagged: false,
                                    }),
                                ),
                            ],
                            ctx.module,
                            ctx.func,
                            &call_location,
                            &mut tmp_function,
                            None,
                        );
                    }
                }

                if tmp_function
                    .return_type
                    .as_ref()
                    .is_none_or(|ty| !ty.is_string())
                {
                    elle_error!(
                        call_location.borrow().error(format!(
                            "The method \"{}\" returns {}{RESET} but it should return {GREEN}string{RESET}.\nThis method's implementation must be changed to return {GREEN}string{RESET}.",
                            func_name,
                            tmp_function.return_type.unwrap_or_else(|| Type::Unknown("_".into())).display(),
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
                            (Type::Word, Value::Const(String::new(), 0)),
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
                .unwrap_or_else(|| {
                    elle_error!(call_location
                        .borrow()
                        .error("Unexpected error when trying to compile the Elle metadata struct"))
                });

            params.insert(0, (res, false));
        }

        if tmp_function.variadic {
            params.insert(
                tmp_function.arguments.len(),
                ((Type::Null, Value::Literal("...".into())), false),
            );

            // ensure structs are not passed as abi structs but rather just the pure
            // address so that it can be reconstructed accordingly with vaarg
            for arg in &mut params[tmp_function.arguments.len()..] {
                arg.0 .0 = arg.0 .0.clone().into_base();
            }
        }

        if !tmp_function.variadic && tmp_function.arguments.len() != params.len() {
            let only = if tmp_function.arguments.len() > params.len() && !params.is_empty() {
                "only "
            } else {
                ""
            };

            let name = if is_generic!(tmp_function.name) {
                let mut parts = tmp_function.name.split('.').map(ToString::to_string);
                let mut name = parts.next().unwrap();

                if type_method {
                    write!(name, "::{}", parts.next().unwrap()).unwrap();
                }

                write!(
                    name,
                    "<{}>",
                    known_generics
                        .iter()
                        .map(|generic| generic.1.display())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
                .unwrap();

                name
            } else {
                tmp_function.name
            };

            let arg_len = tmp_function
                .arguments
                .len()
                .saturating_sub(usize::from(add_meta))
                .saturating_sub(usize::from(type_method));
            let param_len = params.len().saturating_sub(usize::from(add_meta));

            elle_error!(call_location
                .borrow()
                .with_extra_info(if tmp_function.arguments.is_empty() && type_method {
                    format!(
                        "Use `{}({})` instead here",
                        name.replace('.', "::"),
                        if arg_len > 0 { "..." } else { "" }
                    )
                } else {
                    String::new()
                })
                .error(format!(
                    "Function named `{}({})` takes {} argument{}, but you {}passed {}\n{}",
                    name.replace('.', "::"),
                    if arg_len > 0 { "..." } else { "" },
                    arg_len,
                    if arg_len == 1 { "" } else { "s" },
                    only,
                    param_len,
                    if tmp_function.arguments.is_empty() && type_method {
                        format!(
                            "This function doesn't accept a `{} self` parameter.",
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
                                        .replace('%', "")
                                        .split('.')
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

        let tmp = gen.new_temporary(None, true);
        let val = if is_callback {
            let tmp = gen.new_temporary(None, true);
            let res = gen.get_variable(
                &format!("{name}.addr"),
                Some(ctx.func),
                Some(ctx.module),
                &VariableInfo::default(),
            );

            if let Ok((_, addr_val)) = res {
                ctx.func.borrow_mut().assign_instruction(
                    &tmp,
                    &Type::Long,
                    Instruction::Load(Type::Long, addr_val),
                );

                tmp
            } else {
                gen.get_variable(
                    &name,
                    Some(ctx.func),
                    Some(ctx.module),
                    &VariableInfo::default(),
                )
                .unwrap_or((Type::Long, Value::Global(name)))
                .1
            }
        } else {
            gen.get_variable(
                &name,
                Some(ctx.func),
                Some(ctx.module),
                &VariableInfo {
                    dont_call_constants: true,
                },
            )
            .unwrap_or((Type::Long, Value::Global(name)))
            .1
        };

        ctx.func.borrow_mut().assign_instruction(
            &tmp,
            &ty,
            Instruction::Call(val, params.into_iter().map(|x| x.0).collect()),
        );

        Some((ty, tmp))
    }
}
