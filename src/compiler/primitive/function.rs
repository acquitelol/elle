use std::{cell::RefCell, collections::HashMap};

use crate::{
    compiler::{
        compiler::{CodegenContext, Compiler},
        enums::{Linkage, Module},
        qbe::{
            function::Function, instruction::Instruction, r#type::Type, statement::Statement,
            value::Value,
        },
    },
    elle_error, hashmap, is_generic,
    lexer::enums::TokenKind,
    parser::enums::{AstNode, FunctionSource, Literal},
};

pub fn generate_function(
    this: FunctionSource,
    gen: &mut Compiler,
    lambda: bool,
    known_generics: HashMap<String, Type>,
    module: &RefCell<Module>,
) -> Function {
    gen.scopes.push(hashmap![]);

    let mut args = vec![];

    for argument in this.arguments {
        let ty = argument.r#type.clone();

        let tmp = if argument.manual {
            gen.new_manual_argument(&ty, &argument.name)
        } else {
            gen.new_variable(&ty, &argument.name, None, false, false)
        };

        args.push(((ty.into_abi(), tmp), argument.no_fmt));
    }

    let mut func = Function {
        linkage: if this.public || &this.name == "main" {
            Linkage::public()
        } else {
            Linkage::private()
        },
        name: this.name.clone(),
        variadic: this.variadic,
        manual: this.manual,
        external: this.external,
        builtin: this.builtin,
        volatile: this.volatile,
        format: this.format,
        lambda,
        unaliased: this.unaliased,
        usable: this.usable,
        imported: this.imported,
        generics: this.generics,
        known_generics,
        arguments: args,
        return_type: this.r#return,
        blocks: vec![],
    };

    if this.external {
        gen.scopes.pop();
        return func;
    }

    func.add_block("start");

    let func_ref = RefCell::new(func.clone());

    // Could be a tail call recursion
    //
    // The compiler is single pass which means that
    // we need to forward-declare the function with an empty body
    //
    // TODO: Forward declare *all* functions without their bodies
    if !func_ref.borrow().lambda {
        module.borrow_mut().add_function(func.clone());
    }

    for statement in this.body.iter() {
        // Ignore plain literals that aren't assigned to anything
        // exact literals should not be ignored
        let ctx = CodegenContext {
            func: &func_ref,
            module,
            ty: None,
            value: None,
            is_return: false,
        };

        match statement {
            AstNode::Literal(Literal { kind, .. }) => match kind {
                TokenKind::ExactLiteral => {
                    if let Some((_, value)) = statement.clone().compile(gen, &ctx) {
                        func_ref
                            .borrow_mut()
                            .add_instruction(Instruction::Literal(value));
                    }
                }
                TokenKind::Break | TokenKind::Continue => {
                    statement.clone().compile(gen, &ctx);
                }
                _ => {}
            },
            _ => {
                statement.clone().compile(gen, &ctx);
            }
        }
    }

    let mut first_ty: Option<Type> = None;

    macro_rules! ty_err_message {
        ($first:expr, $second:expr, $location:expr, $extra:expr $(,)?) => {{
            $location.error(format!(
                "Inconsistent return types in function '{}': {} and {}.{}",
                if is_generic!(func.name) {
                    let mut parts = func.name.split(".").map(|x| x.to_string()).peekable();
                    let mut name = parts.next().unwrap();

                    if let Some(next) = parts.peek() {
                        if next != "0" {
                            name.push_str(&format!("::{}", parts.next().unwrap()));
                        }
                    }

                    name.push_str(&format!(
                        "<{}>",
                        func.known_generics
                            .iter()
                            .map(|(_, ty)| ty.display())
                            .collect::<Vec<String>>()
                            .join(", ")
                    ));
                    name
                } else {
                    func.name
                }
                .replace(".", "::"),
                $first,
                $second,
                if $extra.is_some() {
                    format!("\n{}", $extra.unwrap())
                } else {
                    "".into()
                }
            ))
        }};
    }

    macro_rules! maybe_void_pointer {
        ($first:expr, $second:expr $(,)?) => {
            $first.is_pointer()
                && $second.is_pointer()
                && ($first.get_pointer_inner().unwrap().is_void()
                    || $second.get_pointer_inner().unwrap().is_void())
        };
    }

    macro_rules! maybe_generic {
        ($first:expr, $second:expr $(,)?) => {
            $first.is_struct()
                && $second.is_struct()
                && is_generic!($first.get_struct_inner().unwrap())
                && is_generic!($second.get_struct_inner().unwrap())
        };
    }

    macro_rules! handle_inconsistent_types {
        ($return_type:expr, $first_type:expr, $location:expr $(,)?) => {
            if $return_type != $first_type && !(maybe_void_pointer!($return_type, $first_type)) {
                if maybe_generic!($return_type, $first_type) {
                    let (a, a_parts) =
                        Type::from_internal_id($return_type.get_struct_inner().unwrap());

                    let (b, b_parts) =
                        Type::from_internal_id($first_type.get_struct_inner().unwrap());

                    if a != b || a_parts != b_parts {
                        elle_error!(
                            ty_err_message!(
                                $return_type.display(),
                                $first_type.display(),
                                $location.with_extra_info(format!(
                                    "This has the type '{}'",
                                    $first_type.display()
                                )),
                                Some(
                                    format!("This function's return type is {} but this statement returns {}",
                                        $return_type.display(), $first_type.display()
                                    )
                                )
                            )
                        )
                    }
                } else {
                    elle_error!(
                        ty_err_message!(
                            $return_type.display(),
                            $first_type.display(),
                            $location.with_extra_info(format!(
                                "This has the type '{}'",
                                $first_type.display()
                            )),
                            Some(
                                format!("This error was caused because the return type is {} but this statement returns {}",
                                    $return_type.display(), $first_type.display()
                                )
                            )
                        )
                    )
                }
            }
        };
    }

    for block in func_ref.borrow().blocks.iter() {
        for statement in block.statements.clone() {
            if let Statement::Volatile(Instruction::Return(val)) = statement {
                if let Some((ty, val, location)) = val {
                    if first_ty.is_none() {
                        first_ty = Some(ty.clone());

                        if let Some(real_return_type) = func_ref.borrow().return_type.clone() {
                            handle_inconsistent_types!(real_return_type, ty, location)
                        }
                    } else {
                        let return_type = ty.clone();
                        let first_type = first_ty.clone().unwrap();

                        if let Some(real_return_type) = func_ref.borrow().return_type.clone() {
                            handle_inconsistent_types!(real_return_type, return_type, location)
                        }

                        if return_type != first_type
                            && !matches!(val, Value::Const(_, _))
                            && !(maybe_void_pointer!(return_type, first_type))
                        {
                            if maybe_generic!(return_type, first_type) {
                                let (a, a_parts) =
                                    Type::from_internal_id(return_type.get_struct_inner().unwrap());

                                let (b, b_parts) =
                                    Type::from_internal_id(first_type.get_struct_inner().unwrap());

                                if a != b || a_parts != b_parts {
                                    elle_error!(
                                        ty_err_message!(
                                            return_type.display(),
                                            first_type.display(),
                                            location.with_extra_info(format!(
                                                "This has the type '{}'",
                                                return_type.display()
                                            )),
                                            Some(format!(
                                                "This error was caused because you returned {} elsewhere, but returned {} here.",
                                                first_type.display(), return_type.display()
                                            ))
                                        )
                                    )
                                }
                            } else {
                                elle_error!(
                                    ty_err_message!(
                                        ty.display(),
                                        first_ty.unwrap().display(),
                                        location,
                                        Some(format!("This error was caused because you returned '{}' elsewhere, but not here.", first_type.display()))
                                    )
                                )
                            }
                        }
                    }
                }
            }
        }
    }

    if first_ty.is_some() {
        let return_ty = func_ref.borrow().return_type.clone();

        if return_ty.is_none() {
            func_ref.borrow_mut().return_type = first_ty;
        } else {
            let return_type = return_ty.clone().unwrap();
            let first_type = first_ty.clone().unwrap();

            handle_inconsistent_types!(return_type, first_type, this.return_location)
        }
    }

    if !func_ref.borrow_mut().returns() && !func_ref.borrow_mut().manual {
        func_ref
            .borrow_mut()
            .add_instruction(Instruction::Return(Some((
                Type::Word,
                Value::Const("".into(), 0),
                this.location,
            ))));
    }

    gen.scopes.pop();

    let mut owned_func = func_ref.borrow_mut().to_owned();

    if owned_func.return_type.is_none() {
        owned_func.return_type = Some(Type::Word)
    }

    // Remove the empty function from the module
    // it will be added automatically when this function leaves scope
    if !func_ref.borrow().lambda {
        module
            .borrow_mut()
            .functions
            .retain(|func| func.name != this.name);
    }

    owned_func
}
