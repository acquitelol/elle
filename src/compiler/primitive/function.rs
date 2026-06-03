use std::{cell::RefCell, collections::HashMap, fmt::Write, rc::Rc};

use crate::{
    compiler::{
        compiler::{CodegenContext, Compiler},
        lib::can_convert::can_convert_to_type,
        qbe::{
            function::Function, instruction::Instruction, linkage::Linkage, module::Module,
            r#type::Type, statement::Statement, value::Value,
        },
    },
    elle_error, hashmap, is_generic,
    lexer::enums::{Location, Token, TokenKind, ValueKind},
    parser::enums::{AstNode, Declare, FunctionSource, Literal},
};

pub fn generate_function(
    this: FunctionSource,
    gen: &mut Compiler,
    lambda: bool,
    constant: bool,
    known_generics: HashMap<String, Type>,
    module: &RefCell<Module>,
) -> Function {
    let mut func = Function {
        linkage: if this.public || &this.name == "main" {
            Linkage::public()
        } else {
            Linkage::private()
        },
        name: this.name.clone(),
        constant,
        variadic: this.variadic,
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
        arguments: this
            .arguments
            .clone()
            .into_iter()
            .map(|x| ((x.r#type, Value::Temporary(x.name)), false))
            .collect(),
        return_type: this.r#return,
        blocks: vec![],
    };

    if this.external {
        return func;
    }

    gen.scopes.push(hashmap![]);
    func.add_block("start");

    let func_ref = RefCell::new(func.clone());

    let mut args = vec![];
    let loc = Rc::new(RefCell::new(Location::base()));
    let ctx = CodegenContext {
        func: &func_ref,
        module,
        ty: None,
        value: None,
        is_return: false,
        is_generic: false,
        is_field_access: false,
    };

    for argument in &this.arguments {
        let ty = argument.r#type.clone();
        let tmp = gen.new_variable(&ty, &argument.name, None, false, false);

        let stmt = AstNode::Declare(Declare {
            name: Token::from_ident(&argument.name),
            r#type: Some(Type::Infer),
            value: Some(Box::new(AstNode::Literal(Literal {
                kind: TokenKind::Identifier,
                value: ValueKind::String(argument.name.clone()),
                location: loc.clone(),
                tagged: false,
            }))),
            location: loc.clone(),
            value_location: loc.clone(),
        });

        stmt.compile(gen, &ctx);
        args.push(((ty, tmp), argument.no_fmt));
    }

    func_ref.borrow_mut().arguments = args;

    // Could be a recursive function
    //
    // The compiler is single pass which means that
    // we need to forward-declare the function with an empty body
    //
    // TODO: Forward declare *all* functions without their bodies
    if !func_ref.borrow().lambda {
        module.borrow_mut().add_function(func.clone());
    }

    for statement in &this.body {
        statement.clone().compile(gen, &ctx);
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
                            write!(name, "::{}", parts.next().unwrap()).unwrap();
                        }
                    }

                    write!(
                        name,
                        "<{}>",
                        func.known_generics
                            .iter()
                            .map(|(_, ty)| ty.display())
                            .collect::<Vec<String>>()
                            .join(", ")
                    )
                    .unwrap();

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

    macro_rules! handle_inconsistent_types {
        ($return_type:expr, $first_type:expr, $location:expr $(,)?) => {
            if !can_convert_to_type(gen, $return_type, $first_type, false) {
                elle_error!(
                    ty_err_message!(
                        $return_type.display(),
                        $first_type.display(),
                        $location.borrow().with_extra_info(format!(
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
        };
    }

    for block in &func_ref.borrow().blocks {
        for statement in block.statements.clone() {
            if let Statement::Volatile(Instruction::Return(Some((ty, val, location)))) = statement {
                if first_ty.is_none() {
                    first_ty = Some(ty.clone());

                    if let Some(real_return_type) = func_ref.borrow().return_type.clone() {
                        handle_inconsistent_types!(&real_return_type, &ty, location);
                    }
                } else {
                    let return_type = &ty;
                    let first_type: &Type = Option::as_ref(&first_ty).unwrap();

                    if let Some(real_return_type) = func_ref.borrow().return_type.clone() {
                        handle_inconsistent_types!(&real_return_type, return_type, location);
                    }

                    if return_type != first_type
                        && !matches!(val, Value::Const(_, _))
                        && !return_type.function_eq(first_type, Some(&location))
                    {
                        elle_error!(
                            ty_err_message!(
                                return_type.display(),
                                first_type.display(),
                                location.borrow().with_extra_info(format!(
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
                }
            }
        }
    }

    if first_ty.is_some() {
        let return_ty = func_ref.borrow().return_type.clone();

        if return_ty.is_none() {
            func_ref.borrow_mut().return_type = first_ty;
        } else {
            let return_type = return_ty.unwrap();
            let first_type = first_ty.unwrap();

            handle_inconsistent_types!(&return_type, &first_type, this.return_location);
        }
    }

    if !func_ref.borrow_mut().returns() {
        func_ref
            .borrow_mut()
            .add_instruction(Instruction::Return(Some((
                Type::Word,
                Value::Const(String::new(), 0),
                this.location,
            ))));
    }

    gen.scopes.pop();

    let mut owned_func = func_ref.borrow_mut().to_owned();

    // If there are statements after a return in the very last block
    // QBE will throw an error. This simply gets rid of statements
    // after a return in the last block.
    //
    // This is fine because a return will jump from the function anyways,
    // so any statements that are removed wouldn't be executed anyway.
    if let Some(index) = owned_func
        .blocks
        .last()
        .unwrap()
        .statements
        .iter()
        .position(|x| matches!(x, Statement::Volatile(Instruction::Return(..))))
    {
        owned_func.blocks.last_mut().unwrap().statements =
            owned_func.blocks.last().unwrap().statements[..=index].to_vec();
    }

    // Remove the empty function from the module
    // it will be added automatically when this function leaves scope
    if !func_ref.borrow().lambda {
        module
            .borrow_mut()
            .functions
            .retain(|_, func| func.name != this.name);
    }

    owned_func
}
