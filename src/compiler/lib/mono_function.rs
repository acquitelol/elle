use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
};

use crate::{
    compiler::{
        compiler::{CodegenContext, Compiler},
        primitive::{function::generate_function, r#struct::generate_struct},
        qbe::{function::Function, module::Module, r#type::Type},
    },
    elle_error, get_GREEN, get_RED, get_RESET, get_STATIC_ARRAY_ID, hashmap,
    lexer::enums::{Location, MutRc},
    parser::enums::{modify_type_in_ast, Argument, AstNode, FunctionSource, Primitive},
    GENERIC_END, GENERIC_IDENTIFIER, GREEN, META_STRUCT_NAME, RED, RESET, STATIC_ARRAY_ID,
};

use super::can_convert::can_convert_to_type;

macro_rules! insert_known_generics {
    ($known_generics:expr, $inner:expr, $gen:expr, $name:expr, $this:expr, $call_location:expr, $throw:literal) => {
        for (key, ty) in $inner.iter().map(|(x, y)| (x.clone(), y.clone())) {
            match $known_generics.get(&key) {
                Some(existing_ty)
                    if !can_convert_to_type($gen, existing_ty, &ty, false) && $throw && !ty.is_unknown() =>
                {
                    let name = $name.replace('.', "::");

                    elle_error!(
                        $call_location.borrow().with_extra_info(format!("{key} = `{}`, but got `{}`", existing_ty.display(), ty.display())).error(
                            format!(
                                "Mismatched type for generic {key} in {}<{}>({}):\n{key} is defined with both type \"{GREEN}{}{RESET}\" and \"{RED}{}{RESET}\"",
                                if name.starts_with(&format!("{}::", get_STATIC_ARRAY_ID!())) && !$this.arguments.is_empty() {
                                    name.replacen(get_STATIC_ARRAY_ID!(), &$this.arguments[0].r#type.display(), 1)
                                } else {
                                    name
                                },
                                $this.generics.join(", "),
                                if $this.arguments.is_empty() { "" } else { "..." },
                                existing_ty.display(),
                                ty.display(),
                                GREEN = get_GREEN!(),
                                RED = get_RED!(),
                                RESET = get_RESET!()
                            )
                        )
                    )
                }
                None if !ty.is_unknown() => {
                    $known_generics.insert(key, ty);
                }
                Some(_) => {} // Found but can convert implicitly
                None => {} // Has an unknown type, shouldn't be added
            }
        }
    };
}

pub fn create_monomorphized_function(
    gen: &mut Compiler,
    name: &mut String,
    add_meta: &mut bool,
    base_known_generics: &[Type],
    known_generics: &mut HashMap<String, Type>,
    parameters: &[(MutRc<Location>, AstNode)],
    module: &RefCell<Module>,
    func: &RefCell<Function>,
    call_location: &MutRc<Location>,
    tmp_function: &mut Function,
    ty: Option<Type>,
    // if this call being monomorphized is at the return step
    // consider:
    //
    // fn foo() -> Option<i32> {
    //     return Option::None();
    // }
    //
    // We want to infer that T = i32 here
    is_return: bool,
) {
    // the aliasing could be multiple levels deep
    loop {
        match gen.generic_functions.get(&name.clone()).unwrap().clone() {
            Primitive::Function(FunctionSource { unaliased, .. }) => {
                if unaliased.is_none() {
                    break;
                }

                *name = unaliased.clone().unwrap_or_else(|| (*name).to_string());
            }
            _ => {}
        }
    }

    match &gen.generic_functions.get(&name.clone()).unwrap().clone() {
        Primitive::Function(this) => {
            // Reassign it if the function is generic
            // as the function won't have been found last time
            if let Some(inner) = this.arguments.first() {
                if inner.r#type.is_struct() {
                    let name = inner.r#type.get_struct_inner().unwrap();

                    if name == META_STRUCT_NAME {
                        *add_meta = true;
                    }
                }
            }

            // Add base known generics
            // If the function takes <T, U, V>
            // and the caller does foo<i32>()
            // it will know T and try to infer U and V
            if base_known_generics.len() <= this.generics.len() {
                known_generics.extend(
                    base_known_generics
                        .iter()
                        .enumerate()
                        .map(|(i, known)| (this.generics[i].clone(), known.clone())),
                );
            }

            let mut tmp_known_generics = known_generics.clone();

            if let Some(other) = this.r#return.clone() {
                if let Some(ty) = ty
                    && other.has_generic_type()
                    && tmp_known_generics.len() < this.generics.len()
                    && let Some(inner) = ty.deduce_generic_type(&other, &call_location)
                {
                    insert_known_generics!(
                        tmp_known_generics,
                        inner,
                        gen,
                        name,
                        this,
                        call_location,
                        false
                    );
                }
            }

            let mut deferred_generics = vec![];
            let struct_pool = RefCell::new(gen.struct_pool.clone());
            let tree = RefCell::new(vec![]);
            struct_pool.borrow().clone_into(&mut gen.struct_pool);

            for (i, parameter) in parameters.iter().cloned().enumerate() {
                let param_ty = {
                    let tmp = this.arguments.get(i + usize::from(*add_meta));
                    tmp.map(|item| {
                        item.r#type.clone().unknown_to_known(
                            Some(&struct_pool),
                            Some(&tree),
                            &this.generics,
                            &tmp_known_generics,
                        )
                    })
                };

                // Use an empty func as to not cause duplicate codegen and/or side effects
                let deferred_functions = gen.deferred_functions.clone();
                let mut tmp_func = func.borrow().clone();
                tmp_func.add_block("start");

                let (ty, _) = parameter.1.clone().compile(
                        gen,
                        &CodegenContext {
                            func: &RefCell::new(tmp_func.clone()),
                            module,
                            ty: param_ty.clone(),
                            value: None,
                            is_return: false,
                            is_generic: true,
                            is_field_access: false,
                        }
                    )
                    .unwrap_or_else(|| elle_error!(parameter.0.borrow().error(
                        format!(
                            "Unexpected error when trying to generate a statement for a parameter in a function called '{name}'",
                        ))));

                // offload lambda monomorphization until after everything else is monomorphized
                // because the lambda's return type may depend on generics which arent known yet
                //
                // consider:
                //
                // fn foo<T, U>(fn(T) -> U x, T b) {
                //     $dbg(x(b));
                // }
                //
                // foo(fn(x) "{}".format(x), 5);
                //
                // in this, the return type requires that we know `x`'s type, but this is deduced after
                // the lambda definition (T = i32 is deduced from `T b`[5])
                //
                // if we try to generate the body without knowing T, we will call `T::__fmt__`, which
                // we can't guarantee will have a definition, so the compiler will throw an error
                //
                // instead, we don't do this right away, and just defer it.
                //
                // after all other generics are deduced, we now know T, so we can now generate the body
                // as this is monomorphized with T = i32, and we know that `i32::__fmt__` exists so
                // we can confidently generate that U = string, monomorphizing everything as a result.
                if ty.is_function() {
                    gen.deferred_functions = deferred_functions;
                    deferred_generics.push((i, param_ty.clone(), parameter.1));
                }

                let other = {
                    let tmp = this.arguments.get(i + usize::from(*add_meta));
                    tmp.map(|item| item.r#type.clone())
                }
                .unwrap_or(Type::Void);

                if other.has_generic_type()
                    && let Some(inner) = ty.deduce_generic_type(&other, &call_location)
                {
                    insert_known_generics!(
                        known_generics,
                        inner,
                        gen,
                        name,
                        this,
                        call_location,
                        true
                    );
                }
            }

            if known_generics.len() < this.generics.len() {
                insert_known_generics!(
                    known_generics,
                    tmp_known_generics,
                    gen,
                    name,
                    this,
                    call_location,
                    true
                );

                // see function signature for explanation on why this is here
                if is_return
                    && let Some(other) = this.r#return.clone()
                    && let Some(ty) = func.borrow().return_type.clone()
                    && other.has_generic_type()
                    && let Some(inner) = ty.deduce_generic_type(&other, &call_location)
                {
                    insert_known_generics!(
                        known_generics,
                        inner,
                        gen,
                        name,
                        this,
                        call_location,
                        false
                    );
                }
            }

            for (i, mut param_ty, parameter) in deferred_generics {
                let struct_pool = RefCell::new(gen.struct_pool.clone());
                let tree = RefCell::new(vec![]);
                struct_pool.borrow().clone_into(&mut gen.struct_pool);

                let mut tmp_func = func.borrow().clone();
                tmp_func.add_block("start");

                param_ty = param_ty.map(|ty| {
                    ty.unknown_to_known(
                        Some(&struct_pool),
                        Some(&tree),
                        &this.generics,
                        known_generics,
                    )
                });

                for primitive in tree.borrow().to_owned() {
                    match primitive {
                        Primitive::Struct(this) => {
                            let td = generate_struct(this, gen);
                            module.borrow_mut().add_type(td);
                        }
                        _ => {}
                    }
                }

                let ty = parameter
                    .compile(
                        gen,
                        &CodegenContext {
                            func: &RefCell::new(tmp_func),
                            module,
                            ty: param_ty.clone(),
                            value: None,
                            is_return: false,
                            is_generic: param_ty.as_ref().is_none_or(|ty| {
                                ty.get_function_inner().is_some_and(|inner| {
                                    inner
                                        .arguments
                                        .iter()
                                        .any(|((ty, _), _)| ty.has_generic_type())
                                })
                            }),
                            is_field_access: false,
                        },
                    )
                    .unwrap()
                    .0;

                let other = {
                    let tmp = this.arguments.get(i + usize::from(*add_meta));
                    tmp.map(|item| item.r#type.clone())
                }
                .unwrap_or(Type::Void);

                if let Some(inner) = ty.deduce_generic_type(&other, &call_location) {
                    // as this is a fn def, its deductions are carefully cherry-picked
                    // in the deduce_generic_type fn, so this should be safe to do
                    // without the macro which is used above
                    known_generics.extend(inner);
                }
            }

            if this.generics.len() != known_generics.len() {
                if this.generics.len() < known_generics.len() {
                    elle_error!(call_location.borrow().with_extra_info(format!("When monomorphizing {name}")).error(
                        format!(
                            "Attempted to monomorphize with too many generics.\nExpected {GREEN}{}{RESET} generic{} but got {RED}{}{RESET} instead",
                            this.generics.len(),
                            if this.generics.len() != 1 { "s" } else { "" },
                            known_generics.len(),
                            GREEN = get_GREEN!(),
                            RED = get_RED!(),
                            RESET = get_RESET!(),
                        )
                    ))
                }

                let a: HashSet<_> = this.generics.iter().cloned().collect();
                let b: HashSet<_> = known_generics.keys().cloned().collect();

                let diff: Vec<_> = a.difference(&b).cloned().collect();

                elle_error!(
                    call_location.borrow().error(format!(
                        "Mismatched number of generics in function {}<{}>({}).\nCould not find generic{} {} where the function specifies <{}>.",
                        name.replace('.', "::"),
                        this.generics.join(", "),
                        if this.arguments.is_empty() { "" } else { "..." },
                        if diff.len() == 1 { "" } else { "s" },
                        diff.join(", "),
                        this.generics.join(", ")
                    ))
                )
            }

            let generic_name = format!(
                "{name}.{GENERIC_IDENTIFIER}.{}.{GENERIC_END}",
                this.generics
                    .iter()
                    .map(|generic| known_generics.get(generic).unwrap().to_internal_id())
                    .collect::<Vec<_>>()
                    .join(".")
            );

            let existing = module.borrow().functions.get(&generic_name).cloned();
            name.clone_from(&generic_name);

            if existing.is_none() {
                // Temporarily empty the scopes
                let scopes = gen.scopes.clone();
                gen.scopes = vec![hashmap![]];

                let struct_pool = RefCell::new(gen.struct_pool.clone());
                let tree = RefCell::new(vec![]);

                let parsed_arguments = &this
                    .arguments
                    .iter()
                    .cloned()
                    // .enumerate()
                    .map(|/*(i, */ arg /*)*/| {
                        // if arg.r#type.is_function() {
                        //     // bring back the scopes while generating
                        //     // this function
                        //     gen.scopes = scopes.clone();

                        //     let parameter = parameters[i].clone();
                        //     let param_ty = arg.r#type.unknown_to_known(
                        //         Some(&struct_pool),
                        //         Some(&tree),
                        //         &this.generics,
                        //         known_generics,
                        //     );

                        //     parameter
                        //         .1
                        //         .compile(
                        //             gen,
                        //             &CodegenContext {
                        //                 func,
                        //                 module,
                        //                 ty: Some(param_ty.clone()),
                        //                 value: None,
                        //                 is_return: false,
                        //                 is_generic: false,
                        //             },
                        //         )
                        //         .unwrap();

                        //     gen.scopes = vec![hashmap![]];

                        //     Argument {
                        //         name: arg.name,
                        //         r#type: param_ty,
                        //         no_fmt: arg.no_fmt,
                        //         is_unused: arg.is_unused,
                        //     }
                        // } else {
                        Argument {
                            name: arg.name,
                            r#type: arg.r#type.unknown_to_known(
                                Some(&struct_pool),
                                Some(&tree),
                                &this.generics,
                                known_generics,
                            ),
                            no_fmt: arg.no_fmt,
                            is_unused: arg.is_unused,
                        }
                        // }
                    })
                    .collect::<Vec<Argument>>();

                let parsed_return = if this.r#return.is_some() {
                    Some(this.r#return.clone().unwrap().unknown_to_known(
                        Some(&struct_pool),
                        Some(&tree),
                        &this.generics,
                        known_generics,
                    ))
                } else {
                    this.r#return.clone()
                };

                let parsed_body = modify_type_in_ast(
                    this.body.clone(),
                    &this.generics,
                    known_generics,
                    Some(&struct_pool),
                    Some(&tree),
                );

                struct_pool.borrow().clone_into(&mut gen.struct_pool);

                for primitive in tree.borrow().to_owned() {
                    match primitive {
                        Primitive::Struct(this) => {
                            let td = generate_struct(this, gen);
                            module.borrow_mut().add_type(td);
                        }
                        _ => {}
                    }
                }

                let function = generate_function(
                    FunctionSource {
                        name: generic_name.clone(),
                        generics: vec![],
                        arguments: parsed_arguments.clone(),
                        r#return: parsed_return,
                        body: parsed_body,
                        ..this.clone()
                    },
                    gen,
                    false,
                    false,
                    known_generics.clone(),
                    module,
                );

                module.borrow_mut().add_function(function.clone());
                *tmp_function = function;

                // Bring them back
                gen.scopes = scopes;
            } else {
                *tmp_function = existing.unwrap();
            }
        }
        _ => {}
    }
}
