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
    elle_error, get_GREEN, get_RED, get_RESET, hashmap,
    lexer::enums::{Location, MutRc},
    parser::enums::{modify_type_in_ast, Argument, AstNode, FunctionSource, Primitive},
    GENERIC_END, GENERIC_IDENTIFIER, GREEN, META_STRUCT_NAME, RED, RESET,
};

use super::can_convert::can_convert_to_type;

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

            for (i, parameter) in parameters.iter().cloned().enumerate() {
                let param_ty = {
                    let tmp = this.arguments.get(i + usize::from(*add_meta));

                    if tmp.is_some() && !Type::Void.has_generic_type(&tmp.unwrap().r#type) {
                        tmp.map(|item| item.r#type.clone())
                    } else {
                        None
                    }
                };

                // Use an empty func as to not cause duplicate codegen and/or side effects
                let mut tmp_func = func.borrow().to_owned();
                tmp_func.add_block("start");

                let (ty, _) = parameter.1.compile(
                        gen,
                        &CodegenContext {
                            func: &RefCell::new(tmp_func),
                            module,
                            ty: param_ty.clone(),
                            value: None,
                            is_return: false
                        }
                    )
                    .unwrap_or_else(|| elle_error!(parameter.0.borrow().error(
                        format!(
                            "Unexpected error when trying to generate a statement for a parameter in a function called '{name}'",
                        ))));

                let other = {
                    let tmp = this.arguments.get(i + usize::from(*add_meta));

                    if tmp.is_some() {
                        tmp.map(|item| item.r#type.clone())
                    } else {
                        None
                    }
                }
                .unwrap_or(Type::Void);

                if ty.has_generic_type(&other) {
                    // Possibly Option.generic.8 and Option
                    if let Some(inner) = ty.deduce_generic_type(&other) {
                        for (key, ty) in inner.iter().map(|(x, y)| (x.clone(), y.clone())) {
                            match known_generics.get(&key) {
                                Some(existing_ty)
                                    if !can_convert_to_type(gen, existing_ty, &ty, false) =>
                                {
                                    elle_error!(
                                        call_location.borrow().with_extra_info(format!("{key} = `{}`, but got `{}`", existing_ty.display(), ty.display())).error(
                                            format!(
                                                "Mismatched type for generic {key} in {}<{}>({}):\n{key} is defined with both type \"{GREEN}{}{RESET}\" and \"{RED}{}{RESET}\"",
                                                name.replace('.', "::"),
                                                this.generics.join(", "),
                                                if this.arguments.is_empty() { "" } else { "..." },
                                                existing_ty.display(),
                                                ty.display(),
                                                GREEN = get_GREEN!(),
                                                RED = get_RED!(),
                                                RESET = get_RESET!()
                                            )
                                        )
                                    )
                                }
                                Some(_) => {} // Found but can convert implicitly
                                None => {
                                    known_generics.insert(key, ty);
                                }
                            }
                        }
                    } else if other.is_unknown() && other.get_unknown_inner().unwrap() == "fn" {
                        eprintln!(
                            "{}",
                            this.location.borrow().warning(format!(
                                "Failed to deduce a generic type from {} and {}",
                                ty.display(),
                                other.display()
                            ),)
                        );
                    }
                }
            }

            if let Some(other) = this.r#return.clone() {
                if let Some(ty) = ty {
                    if ty.has_generic_type(&other) && known_generics.len() < this.generics.len() {
                        // Possibly Option.generic.8 and Option
                        if let Some(inner) = ty.deduce_generic_type(&other) {
                            known_generics.extend(inner);
                        } else if other.is_unknown() && other.get_unknown_inner().unwrap() == "fn" {
                            eprintln!(
                                "{}",
                                this.location.borrow().warning(format!(
                                    "Failed to deduce a generic type from {} and {}",
                                    ty.display(),
                                    other.display()
                                ))
                            );
                        }
                    }
                }

                if let Some(ty) = func.borrow().return_type.clone() {
                    if ty.has_generic_type(&other) && known_generics.len() < this.generics.len() {
                        // Possibly Option.generic.8 and Option
                        if let Some(inner) = ty.deduce_generic_type(&other) {
                            known_generics.extend(inner);
                        } else if other.is_unknown() && other.get_unknown_inner().unwrap() == "fn" {
                            eprintln!(
                                "{}",
                                this.location.borrow().warning(format!(
                                    "Failed to deduce a generic type from {} and {}",
                                    ty.display(),
                                    other.display()
                                ),)
                            );
                        }
                    }
                }
            }

            if this.generics.len() != known_generics.len() {
                if this.generics.len() < known_generics.len() {
                    todo!("the user passed too many generics");
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
                    .map(|generic| { known_generics.get(generic).unwrap().to_internal_id() })
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
                    .map(|arg| Argument {
                        name: arg.name,
                        r#type: arg.r#type.unknown_to_known(
                            Some(&struct_pool),
                            Some(&tree),
                            &this.generics,
                            known_generics,
                        ),
                        no_fmt: arg.no_fmt,
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
