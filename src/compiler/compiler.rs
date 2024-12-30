#![allow(unused_assignments)]
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fs::File,
    io::Write,
    rc::Rc,
};

use crate::{
    advance, hashmap, is_generic,
    lexer::enums::{Location, TokenKind, ValueKind},
    misc::colors::*,
    parser::{
        enums::{modify_type_in_ast, Argument, AstNode, Primitive},
        parser::StructPool,
    },
    unknown_field, unknown_function, Warning, Warnings, DUNDER_CONSTANTS, ENV_ID, ENV_STRUCT_NAME,
    EQUALS_CONSTANT, FORMAT_CONSTANT, GENERIC_END, GENERIC_IDENTIFIER, LOAD_CONSTANT, MAIN_ID,
    META_STRUCT_NAME, POINTER_ID, STORE_CONSTANT, VOID_POINTER_ID,
};

use super::enums::{
    Comparison, Data, DataItem, Function, Instruction, Linkage, Module, Statement, Type, TypeDef,
    Value,
};

pub struct Compiler {
    tmp_counter: u32,
    scopes: Vec<HashMap<String, (Type, Value)>>,
    data_sections: Vec<Data>,
    generic_functions: HashMap<String, Primitive>,
    // Struct Name => ((Field Name, Field Type)[], (Known Generic)[])
    struct_pool: StructPool,
    loop_labels: Vec<String>,
    // ret_types: HashMap<String, Type>,
    buf_metadata: HashMap<Value, (Type, Value)>,
    tree: Vec<Primitive>,
    warnings: Warnings,
    // lambda functions that should be added as soon as possible
    deferred_functions: Vec<Function>,
    // Map from temporary to its stack allocated address
    address_pool: HashMap<Value, Value>,
    output_path: String,
}

impl Compiler {
    fn tmp_name_with_debug_assertions(&self, name: &str, minify: bool) -> String {
        if cfg!(debug_assertions) || !minify {
            format!("{}.{}", name, self.tmp_counter)
        } else {
            format!(".{}", self.tmp_counter)
        }
    }

    fn new_temporary(&mut self, name: Option<&str>, _minify: bool) -> Value {
        self.tmp_counter += 1;
        Value::Temporary(self.tmp_name_with_debug_assertions(name.unwrap_or("tmp"), false))
    }

    fn new_variable(
        &mut self,
        ty: &Type,
        name: &str,
        func: Option<&RefCell<Function>>,
        new: bool,
        minify: bool,
    ) -> Value {
        let tmp = if new {
            self.new_temporary(Some(name), minify)
        } else {
            let existing_var = self.get_variable(name, func, None);

            match existing_var {
                Ok((_, val)) => match val {
                    Value::Temporary(_) => val,
                    _ => self.new_temporary(Some(name), minify),
                },
                Err(_) => self.new_temporary(Some(name), minify),
            }
        };

        let scope = self
            .scopes
            .last_mut()
            .expect("Expected last scope to exist");

        scope.insert(name.to_owned(), (ty.to_owned(), tmp.to_owned()));
        tmp
    }

    fn new_manual_argument(&mut self, ty: &Type, name: &str) -> Value {
        let tmp = Value::Temporary(name.into());

        let scope = self
            .scopes
            .last_mut()
            .expect("Expected last scope to exist");

        scope.insert(name.to_owned(), (ty.to_owned(), tmp.to_owned()));
        tmp
    }

    fn get_variable(
        &mut self,
        name: &str,
        func: Option<&RefCell<Function>>,
        module: Option<&RefCell<Module>>,
    ) -> Result<(Type, Value), String> {
        let var = self
            .scopes
            .iter()
            .rev()
            .filter_map(|s| s.get(name))
            .next()
            .ok_or_else(|| {
                format!(
                    "\nUndefined variable '{}'{}",
                    name,
                    if func.is_some_and(|func| func.borrow().lambda) {
                        ". Lambdas do not capture variables in scope."
                    } else {
                        " "
                    }
                )
            });

        if var.is_err() {
            for item in self.tree.iter().cloned() {
                match item {
                    Primitive::Constant {
                        name: const_name,
                        r#type: ty,
                        location,
                        usable,
                        ..
                    } => {
                        if name == const_name && func.is_some() {
                            if !usable && !func.unwrap().borrow_mut().imported {
                                panic!(
                                    "{}",
                                    location.error(format!(
                                        "Constant named '{}' was not imported and can't be used",
                                        name
                                    ))
                                )
                            }

                            let temp = self.new_temporary(Some("constant"), true);

                            func.unwrap().borrow_mut().assign_instruction(
                                &temp,
                                &ty.clone().unwrap(),
                                Instruction::Call(Value::Global(name.into()), vec![]),
                            );

                            return Ok((ty.unwrap(), temp));
                        }
                    }
                    Primitive::Function {
                        name: op_name,
                        usable,
                        location,
                        builtin,
                        ..
                    } => {
                        if name == op_name {
                            if !usable && !func.unwrap().borrow_mut().imported && !builtin {
                                panic!(
                                    "{}",
                                    location.error(format!(
                                        "Function named '{}' was not imported and can't be used",
                                        name.replace(".", "::")
                                    ))
                                )
                            }

                            return Ok((
                                Type::Function(Box::new(if let Some(module) = module {
                                    module
                                        .borrow()
                                        .functions
                                        .iter()
                                        .find(|func| func.name == name)
                                        .cloned()
                                } else {
                                    None
                                })),
                                Value::Global(name.into()),
                            ));
                        }
                    }
                    _ => {}
                }
            }
        }

        var.cloned()
    }

    fn get_variable_lazy(
        &mut self,
        name: &String,
        func: Option<&RefCell<Function>>,
        module: Option<&RefCell<Module>>,
        location: Location,
        // (Ty, Val, Init)
    ) -> Option<(Type, Value)> {
        let var = self.get_variable(&name, func, module);

        match var {
            Ok((ty, val)) => {
                let res = self.get_variable(&format!("{}.addr", name), func, module);

                if res.is_ok() && func.is_some() {
                    let (_, addr_val) = res.unwrap();

                    func.unwrap().borrow_mut().assign_instruction(
                        &val,
                        &ty,
                        Instruction::Load(ty.clone(), addr_val),
                    );

                    return Some((ty, val));
                }

                Some((ty, val))
            }
            Err(msg) => {
                macro_rules! undefined_error {
                    () => {
                        panic!(
                            "{}",
                            location.error(format!(
                                "Unexpected error when trying to get a variable called '{}': {}",
                                name, msg
                            ))
                        )
                    };
                }

                if !module.is_some() {
                    undefined_error!();
                }

                // If it fails to get the variable from the current scope
                // then attempt to get it from a global instead
                let tmp_module = module.unwrap().borrow();
                let global = tmp_module
                    .data
                    .iter()
                    .find(|item| item.name == name.clone());

                if let Some(item) = global {
                    Some((Type::Long, Value::Global(item.name.clone())))
                } else {
                    undefined_error!()
                }
            }
        }
    }

    fn generate_function(
        &mut self,
        name: String,
        public: bool,
        variadic: bool,
        manual: bool,
        external: bool,
        builtin: bool,
        volatile: bool,
        format: bool,
        lambda: bool,
        unaliased: Option<String>,
        usable: bool,
        imported: bool,
        generics: Vec<String>,
        known_generics: HashMap<String, Type>,
        arguments: &Vec<Argument>,
        return_type: Option<Type>,
        body: Vec<AstNode>,
        module: &RefCell<Module>,
        location: Location,
        return_location: Location,
    ) -> Function {
        self.scopes.push(hashmap!());

        let mut args = vec![];

        for argument in arguments {
            let ty = argument.r#type.clone();

            let tmp = if argument.manual {
                self.new_manual_argument(&ty, &argument.name)
            } else {
                self.new_variable(&ty, &argument.name, None, false, false)
            };

            args.push((ty.into_abi(), tmp));
        }

        let mut func = Function {
            linkage: if public || &name == "main" {
                Linkage::public()
            } else {
                Linkage::private()
            },
            name: name.clone(),
            variadic,
            manual,
            external,
            builtin,
            volatile,
            format,
            lambda,
            unaliased,
            usable,
            imported,
            generics,
            known_generics,
            arguments: args,
            return_type,
            blocks: vec![],
        };

        if external {
            self.scopes.pop();
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

        for statement in body.iter() {
            // Ignore plain literals that aren't assigned to anything
            // exact literals should not be ignored
            match statement {
                AstNode::Literal { kind, .. } => match kind {
                    TokenKind::ExactLiteral => match self.generate_statement(
                        &func_ref,
                        module,
                        statement.clone(),
                        None,
                        None,
                        false,
                    ) {
                        Some((_, value)) => func_ref
                            .borrow_mut()
                            .add_instruction(Instruction::Literal(value)),
                        _ => {}
                    },
                    TokenKind::Break | TokenKind::Continue => {
                        self.generate_statement(
                            &func_ref,
                            module,
                            statement.clone(),
                            None,
                            None,
                            false,
                        );
                    }
                    _ => {}
                },
                _ => match self.generate_statement(
                    &func_ref,
                    module,
                    statement.clone(),
                    None,
                    None,
                    false,
                ) {
                    _ => {}
                },
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
                            panic!(
                                "{}",
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
                        panic!(
                            "{}",
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
                                    let (a, a_parts) = Type::from_internal_id(
                                        return_type.get_struct_inner().unwrap(),
                                    );

                                    let (b, b_parts) = Type::from_internal_id(
                                        first_type.get_struct_inner().unwrap(),
                                    );

                                    if a != b || a_parts != b_parts {
                                        panic!(
                                            "{}",
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
                                    panic!(
                                        "{}",
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

                handle_inconsistent_types!(return_type, first_type, return_location)
            }
        }

        if !func_ref.borrow_mut().returns() && !func_ref.borrow_mut().manual {
            func_ref
                .borrow_mut()
                .add_instruction(Instruction::Return(Some((
                    Type::Word,
                    Value::Const("".into(), 0),
                    location,
                ))));
        }

        self.scopes.pop();

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
                .retain(|func| func.name != name);
        }

        owned_func
    }

    fn generate_statement(
        &mut self,
        func: &RefCell<Function>,
        module: &RefCell<Module>,
        stmt: AstNode,
        ty: Option<Type>,
        value: Option<Value>,
        is_return: bool,
    ) -> Option<(Type, Value)> {
        let res = match stmt {
            AstNode::Declare {
                name,
                r#type,
                value,
                location,
                value_location,
            } => {
                let existing = match self.get_variable(name.as_str(), Some(func), Some(module)) {
                    Ok((ty, _)) => ty,
                    Err(_) => Type::Word,
                };

                if r#type.is_none()
                    && self
                        .get_variable(name.as_str(), Some(func), Some(module))
                        .is_err()
                {
                    panic!("{}", location.error(format!("Variable named '{}' hasn't been declared yet.\nPlease declare it before trying to re-declare it.", name)));
                }

                if r#type.clone().is_some_and(|ty| ty == Type::Infer) && value.is_none() {
                    panic!("{}", location.error(format!("Failed to determine a type for '{}'.\nPlease give this variable a type or a value.", name)));
                }

                let res = self.get_variable(&format!("{}.addr", name), Some(func), Some(module));
                let mut local_ty = r#type.clone().unwrap_or(existing);

                let mut temp = if local_ty != Type::Infer {
                    Some(self.new_variable(&local_ty, &name, Some(func), false, false))
                } else {
                    None
                };

                let parsed = self.generate_statement(
                    func,
                    module,
                    *value.unwrap_or(Box::new(
                        if r#type.clone().is_some_and(|ty| ty.is_struct()) {
                            AstNode::StructLiteral {
                                name: r#type.clone().unwrap().get_struct_inner().unwrap(),
                                values: vec![],
                                location: location.clone(),
                            }
                        } else {
                            AstNode::Literal {
                                kind: TokenKind::IntegerLiteral,
                                value: ValueKind::Number(0),
                                location: location.clone(),
                            }
                        },
                    )),
                    if local_ty == Type::Infer {
                        None
                    } else {
                        Some(local_ty.clone())
                    },
                    temp.clone(),
                    false,
                );

                if let Some((ret_ty, value)) = parsed {
                    if local_ty == Type::Infer {
                        local_ty = ret_ty.clone();

                        temp = Some(self.new_variable(&local_ty, &name, Some(func), false, false));

                        let scope = self
                            .scopes
                            .last_mut()
                            .expect("Expected last scope to exist");
                        scope.insert(name.to_owned(), (local_ty.clone(), temp.clone().unwrap()));
                    }
                    // in `fn *a = fn() -> 5;`
                    // - fn *a has type Pointer(Fn)
                    // - fn() -> 5 has type Function(...)
                    // essentially the below sets the former
                    // to the latter if necessary
                    if ret_ty.is_function()
                        && local_ty.get_pointer_inner().is_some_and(|ptr| {
                            ptr.get_unknown_inner()
                                .is_some_and(|inner| inner == "fn".to_string())
                        })
                    {
                        local_ty = ret_ty.clone();
                        temp = Some(self.new_variable(&local_ty, &name, Some(func), false, false))
                    }

                    let (final_ty, final_val) = if ret_ty != local_ty {
                        self.convert_to_type(
                            func,
                            ret_ty,
                            local_ty.clone(),
                            value.clone(),
                            &location,
                            &value_location,
                            false,
                        )
                    } else {
                        (local_ty.clone(), value.clone())
                    };

                    if res.is_ok() && r#type.is_none() {
                        let (addr_ty, addr_val) = res.unwrap();
                        let tmp = self.new_variable(&addr_ty, &name, Some(func), true, false);

                        if addr_ty != final_ty
                            && !(addr_ty.is_pointer()
                                && final_ty.is_pointer()
                                && final_ty.get_pointer_inner().unwrap().is_void())
                        {
                            panic!(
                                "{}",
                                location.error(format!(
                                    "Cannot redeclare '{}' which has type {} to type {}",
                                    name,
                                    addr_ty.display(),
                                    final_ty.display()
                                ))
                            )
                        }

                        func.borrow_mut().add_instruction(Instruction::Store(
                            addr_ty.clone(),
                            addr_val.clone(),
                            final_val.clone(),
                        ));

                        func.borrow_mut().assign_instruction(
                            &tmp,
                            &addr_ty,
                            Instruction::Load(addr_ty.clone(), addr_val.clone()),
                        );

                        self.address_pool
                            .insert(temp.unwrap().clone(), addr_val.clone());
                        return Some((addr_ty, tmp));
                    }

                    let addr_val = self.new_variable(
                        &local_ty,
                        &format!("{}.addr", name),
                        Some(func),
                        true,
                        false,
                    );

                    func.borrow_mut().assign_instruction_front(
                        &addr_val,
                        &Type::Pointer(Box::new(final_ty.clone())),
                        Instruction::Alloc8(Value::Const("".into(), final_ty.size(module) as i128)),
                    );

                    func.borrow_mut().add_instruction(Instruction::Store(
                        final_ty.clone(),
                        addr_val.clone(),
                        final_val.clone(),
                    ));

                    self.address_pool
                        .insert(temp.clone().unwrap(), addr_val.clone());
                    return Some((final_ty, final_val));
                }

                None
            }
            AstNode::Return {
                value, location, ..
            } => {
                match self.generate_statement(func, module, *value, ty, None, true) {
                    Some((ret_ty, value)) => {
                        if !func.borrow_mut().manual {
                            func.borrow_mut().add_instruction(Instruction::Return(Some((
                                ret_ty, value, location,
                            ))))
                        }
                    }
                    None => {
                        if !func.borrow_mut().manual {
                            func.borrow_mut().add_instruction(Instruction::Return(None))
                        }
                    }
                }

                None
            }
            AstNode::BinaryOperation {
                left,
                right,
                operator,
                treat_as_string,
                dunder_methods,
                location,
            } => {
                // Implement conditional short circuiting for logical AND and OR
                if matches!(operator, TokenKind::And | TokenKind::Or) {
                    return Some(self.handle_short_circuiting_operation(
                        left, right, func, module, ty, is_return, location, operator,
                    ));
                }

                if matches!(operator, TokenKind::Range | TokenKind::RangeEqual) {
                    let node = AstNode::FunctionCall {
                        name: "Array.range".into(),
                        generics: vec![],
                        parameters: vec![
                            (location.clone(), *left),
                            (location.clone(), *right),
                            (
                                location.clone(),
                                AstNode::Literal {
                                    kind: TokenKind::IntegerLiteral,
                                    value: ValueKind::Number(
                                        if operator == TokenKind::RangeEqual {
                                            1
                                        } else {
                                            0
                                        },
                                    ),
                                    location: location.clone(),
                                },
                            ),
                        ],
                        type_method: false,
                        ignore_no_def: false,
                        location: location.clone(),
                    };

                    let (ty, val) = self
                        .generate_statement(func, module, node, ty.clone(), None, is_return)
                        .expect(&location.error(
                            "Unexpected error when trying to parse left side of an arithmetic operation"
                        ));

                    return Some((ty, val));
                }

                let (mut left_ty, left_val_unparsed) = self
                    .generate_statement(func, module, *left.clone(), ty.clone(), None, is_return)
                    .expect(&location.error(
                        "Unexpected error when trying to parse left side of an arithmetic operation"
                    ));

                let (mut right_ty, right_val_unparsed) = self
                    .generate_statement(func, module, *right.clone(), ty.clone(), None, is_return)
                    .expect(&location.error(
                        "Unexpected error when trying to parse right side of an arithmetic operation"
                    ));

                let mut left_val = left_val_unparsed.clone();
                let mut right_val = right_val_unparsed.clone();

                if operator != TokenKind::Concat {
                    if left_ty.is_string() && right_ty == Type::Char {
                        let char_tmp = self.new_temporary(None, true);

                        func.borrow_mut().assign_instruction(
                            &char_tmp,
                            &Type::Char,
                            Instruction::Load(Type::Char, left_val),
                        );

                        left_ty = Type::Char;
                        left_val = char_tmp;
                    }

                    if right_ty.is_string() && left_ty == Type::Char {
                        let char_tmp = self.new_temporary(None, true);

                        func.borrow_mut().assign_instruction(
                            &char_tmp,
                            &Type::Char,
                            Instruction::Load(Type::Char, right_val),
                        );

                        right_ty = Type::Char;
                        right_val = char_tmp;
                    }
                }

                if left_ty.weight() > right_ty.weight() {
                    let (_, val) = self.convert_to_type(
                        func,
                        right_ty.clone(),
                        left_ty.clone(),
                        right_val_unparsed,
                        &location,
                        &location,
                        false,
                    );

                    right_val = val;
                } else if left_ty.weight() < right_ty.weight() {
                    let (ty, val) = self.convert_to_type(
                        func,
                        left_ty,
                        right_ty.clone(),
                        left_val_unparsed,
                        &location,
                        &location,
                        false,
                    );

                    left_ty = ty;
                    left_val = val;
                }

                if (!left_ty.is_primitive() || !right_ty.is_primitive())
                    && [TokenKind::EqualTo, TokenKind::NotEqualTo].contains(&operator)
                    && dunder_methods
                {
                    let mut node = AstNode::FunctionCall {
                        name: EQUALS_CONSTANT.into(),
                        generics: vec![],
                        parameters: vec![(location.clone(), *left), (location.clone(), *right)],
                        type_method: true,
                        ignore_no_def: false,
                        location: location.clone(),
                    };

                    if operator == TokenKind::NotEqualTo {
                        node = AstNode::LogicalNot {
                            value: Box::new(node),
                            location: location.clone(),
                        }
                    }

                    let (ty, val) = self
                        .generate_statement(func, module, node, ty.clone(), None, is_return)
                        .expect(&location.error(
                            "Unexpected error when trying to parse an equals arithmetic operation",
                        ));

                    return Some((ty, val));
                }

                if left_ty.is_string() && right_ty.is_string() && treat_as_string {
                    let mut kind = None;

                    match operator {
                        // Token => (Name, HasMeta, Type),
                        TokenKind::Concat => {
                            kind = Some(("concat", true, Type::Pointer(Box::new(Type::Char))))
                        }
                        _ => {}
                    }

                    if let Some((kind, has_meta, ty)) = kind {
                        // TODO: extend this idea to more than just strings?
                        // ideally add a .equals method on any primitive to make it equatable, and implement it for each
                        // same for any struct, define a .equals method to allow it to be ran with == directly
                        let func_name = format!("string.{kind}");
                        let module_ref = module.borrow();

                        let tmp_function_option = module_ref
                            .functions
                            .iter()
                            .find(|func| func.name == func_name);

                        if tmp_function_option.is_none() {
                            panic!("{}", location.error(format!("Cannot use the '{}' operator because the string module is not imported.\nPlease import it with {GREEN}{BOLD}use std/string;{RESET} at the top of this file.", operator)))
                        }

                        let tmp_function = tmp_function_option.unwrap().clone();
                        let mut params = vec![(left_ty, left_val), (right_ty, right_val)];

                        if has_meta {
                            let meta = Compiler::generate_meta_struct(
                                func,
                                &params,
                                vec![(location.clone(), *left), (location.clone(), *right)],
                                location.clone(),
                            );

                            let res = self
                                .generate_statement(func, module, meta, None, None, false)
                                .expect(&location.error(
                                    "Unexpected error when trying to compile the Elle metadata struct",
                                ));

                            params.insert(0, res);
                        }

                        if tmp_function.variadic {
                            let res = self
                                .generate_statement(
                                    func,
                                    module,
                                    AstNode::Literal {
                                        kind: TokenKind::ExactLiteral,
                                        value: ValueKind::String("...".into()),
                                        location: location.clone(),
                                    },
                                    Some(ty.clone()),
                                    None,
                                    false,
                                )
                                .expect(&location.error(
                                    "Unexpected error when trying to compile the variadic literal '...'",
                                ));

                            params.insert(tmp_function.arguments.len(), res);
                        }

                        let instr = Instruction::Call(Value::Global(func_name), params);
                        let op_temp = self.new_temporary(None, true);

                        func.borrow_mut().assign_instruction(&op_temp, &ty, instr);

                        return Some((ty, op_temp));
                    }
                }

                if operator == TokenKind::Concat && treat_as_string {
                    panic!(
                        "{}",
                        location.error(format!(
                            "Cannot use the '<>' operator on non-string types {} and {}",
                            left_ty.display(),
                            right_ty.display()
                        ))
                    )
                }

                if [
                    TokenKind::BitwiseXor,
                    TokenKind::BitwiseOr,
                    TokenKind::BitwiseAnd,
                    TokenKind::ShiftLeft,
                    TokenKind::ShiftRight,
                ]
                .contains(&operator)
                    && (left_ty.is_float() || right_ty.is_float())
                {
                    panic!(
                        "{}",
                        location.error(format!(
                            "Cannot use the '{:?}' operator on non-integer type '{}'.\nYou can cast it to an integer if you need this functionality.",
                            operator,
                            if left_ty.is_float() {
                                left_ty.display()
                            } else {
                                right_ty.display()
                            }
                        ))
                    )
                }

                let instruction_ty = left_ty;
                let cloned_ty = instruction_ty.clone();

                let res = match operator.clone() {
                    TokenKind::Add => Instruction::Add(left_val, right_val),
                    TokenKind::Subtract => Instruction::Subtract(left_val, right_val),
                    TokenKind::Multiply => Instruction::Multiply(left_val, right_val),
                    TokenKind::Divide => Instruction::Divide(left_val, right_val),
                    TokenKind::Modulus => Instruction::Modulus(left_val, right_val),
                    TokenKind::GreaterThan => Instruction::Compare(
                        cloned_ty,
                        Comparison::GreaterThan,
                        left_val,
                        right_val,
                    ),
                    TokenKind::GreaterThanEqual => Instruction::Compare(
                        cloned_ty,
                        Comparison::GreaterThanEqual,
                        left_val,
                        right_val,
                    ),
                    TokenKind::LessThan => {
                        Instruction::Compare(cloned_ty, Comparison::LessThan, left_val, right_val)
                    }
                    TokenKind::LessThanEqual => Instruction::Compare(
                        cloned_ty,
                        Comparison::LessThanEqual,
                        left_val,
                        right_val,
                    ),
                    TokenKind::EqualTo => {
                        Instruction::Compare(cloned_ty, Comparison::Equal, left_val, right_val)
                    }
                    TokenKind::NotEqualTo => {
                        Instruction::Compare(cloned_ty, Comparison::NotEqual, left_val, right_val)
                    }
                    TokenKind::BitwiseAnd => Instruction::BitwiseAnd(left_val, right_val),
                    TokenKind::BitwiseOr => Instruction::BitwiseOr(left_val, right_val),
                    TokenKind::BitwiseXor => Instruction::BitwiseXor(left_val, right_val),
                    TokenKind::ShiftLeft => Instruction::ShiftLeft(left_val, right_val),
                    TokenKind::ShiftRight => Instruction::ArithmeticShiftRight(left_val, right_val),
                    _ => panic!(
                        "{}",
                        location.error(format!("Invalid operator token: {:?}", operator))
                    ),
                };

                let op_temp = self.new_temporary(None, true);

                let final_ty = if operator.is_comparative() {
                    Type::Boolean
                } else {
                    instruction_ty
                };

                func.borrow_mut()
                    .assign_instruction(&op_temp, &final_ty, res);

                Some((final_ty, op_temp))
            }
            AstNode::Literal {
                kind,
                value,
                location,
            } => match kind {
                TokenKind::Identifier => match value {
                    ValueKind::String(name) => {
                        self.get_variable_lazy(&name, Some(func), Some(module), location.clone())
                    }
                    _ => None,
                },
                TokenKind::ExactLiteral => match value {
                    ValueKind::String(val) => Some((Type::Null, Value::Literal(val))),
                    _ => None,
                },
                TokenKind::Break => {
                    if let Some(label) = &self.loop_labels.last() {
                        func.borrow_mut()
                            .add_instruction(Instruction::Jump(format!("{}.end", label)));
                    } else {
                        panic!("{}", location.error("Break can only be used in a loop"));
                    }

                    None
                }
                TokenKind::Continue => {
                    if let Some(label) = &self.loop_labels.last() {
                        func.borrow_mut()
                            .add_instruction(Instruction::Jump(format!("{}.step", label)));
                    } else {
                        panic!("{}", location.error("Continue can only be used in a loop"));
                    }

                    None
                }
                _ => match value {
                    ValueKind::Number(val) => {
                        let num_ty = match kind {
                            TokenKind::BoolLiteral => Type::Boolean,
                            TokenKind::IntegerLiteral => Type::Word,
                            TokenKind::FloatLiteral => Type::Single,
                            TokenKind::LongLiteral => Type::Long,
                            _ => Type::Word,
                        };

                        let mut final_ty = if ty.clone().is_some_and(|ty| !ty.is_string()) {
                            ty.unwrap_or(num_ty)
                        } else {
                            num_ty
                        };

                        if is_return {
                            final_ty = func.borrow_mut().return_type.clone().unwrap_or(final_ty);
                        }

                        Some((
                            final_ty.clone(),
                            Value::Const(
                                if final_ty.clone() == Type::Double {
                                    "d_"
                                } else if final_ty.clone() == Type::Single {
                                    "s_"
                                } else {
                                    ""
                                }
                                .into(),
                                val,
                            ),
                        ))
                    }
                    ValueKind::String(val) => {
                        self.tmp_counter += 1;
                        let name = self
                            .tmp_name_with_debug_assertions(&func.borrow_mut().name.clone(), true);

                        self.data_sections.push(Data::new(
                            Linkage::private(),
                            name.clone(),
                            None,
                            vec![
                                (Type::Byte, DataItem::String(val)),
                                (Type::Byte, DataItem::Const(0)),
                            ],
                        ));

                        Some((Type::Pointer(Box::new(Type::Char)), Value::Global(name)))
                    }
                    ValueKind::Character(val) => {
                        Some((Type::Char, Value::Const("".into(), val as i128)))
                    }
                    ValueKind::Nil => {
                        self.tmp_counter += 1;
                        let name = self
                            .tmp_name_with_debug_assertions(&func.borrow_mut().name.clone(), true);

                        self.data_sections.push(Data::new(
                            Linkage::private(),
                            name.clone(),
                            None,
                            vec![(Type::Byte, DataItem::Const(0))],
                        ));

                        Some((Type::Long, Value::Global(name)))
                    }
                },
            },
            AstNode::FunctionCall {
                mut name,
                // Generics passed by the caller
                // ie foo<i32>()
                // !! these ones can be indexed !!
                generics: base_known_generics,
                parameters,
                type_method,
                ignore_no_def,
                location: mut call_location,
            } => {
                let declarative_ty = ty.clone().unwrap_or(Type::Void);
                let mut should_get_address = false; // Gets address if first arg's ty is the same as ty predicate
                let mut first_param = None;
                let mut known_generics = hashmap![String, Type];

                if type_method {
                    let parameter =
                        parameters.get(0).expect(&call_location.error(
                            "Tried to get the 0th parameter to parse struct call but failed",
                        ));

                    let (mut ty, val) = self.generate_statement(
                        func,
                        module,
                        parameter.1.clone(),
                        None,
                        None,
                        false,
                    )
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
                    // struct* access
                    } else if ty.is_pointer() && ty.get_pointer_inner().unwrap().is_struct() {
                        name = format!(
                            "{}.{}",
                            ty.get_pointer_inner().unwrap().get_struct_inner().unwrap(),
                            name
                        )
                    // string access
                    } else if ty.is_string() {
                        name = format!("string.{}", name)
                    // void* access
                    } else if ty.is_void_pointer() {
                        name = format!("{}.{}", VOID_POINTER_ID, name)
                    // string* access
                    } else if ty.is_pointer() && ty.get_pointer_inner().unwrap().is_string() {
                        name = format!("string.{}", name)
                    // fmt access
                    } else if ty.is_pointer()
                        && DUNDER_CONSTANTS.contains(&name.as_str())
                        && type_method
                    {
                        name = format!("{}.{}", POINTER_ID, name)
                    } else {
                        name = format!("{}.{}", ty.id(), name)
                    }
                }

                let tmp_function_option = module
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
                    let callback = self.get_variable(name.as_str(), Some(func), Some(module));

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
                        arguments: parameters.iter().map(|param| {
                            let mut tmp_func = Function::default();
                            tmp_func.add_block("start");

                            self.generate_statement(
                                &RefCell::new(tmp_func),
                                module,
                                param.1.clone(),
                                None,
                                None,
                                false,
                            )
                            .expect(&param.0.error(
                                format!(
                                    "Unexpected error when trying to generate a statement for a parameter in a function called '{}'",
                                    name
                                ))
                            )
                        }).collect(),
                        return_type: Some(declarative_ty.clone()),
                        blocks: vec![]
                    };

                    if let Ok((ty, _)) = callback {
                        if (ty.is_pointer()
                            && ty.get_pointer_inner().unwrap().is_unknown()
                            && ty.get_pointer_inner().unwrap().get_unknown_inner().unwrap() == "fn")
                            || ignore_no_def
                            || self.generic_functions.contains_key(&name)
                        {
                            is_callback = true;
                            fallback
                        } else if ty.is_function() {
                            is_callback = true;

                            // We know the function exists
                            ty.get_function_inner().unwrap().unwrap()
                        } else {
                            unknown_function!(call_location, name, module)
                        }
                    } else if ignore_no_def {
                        is_callback = true;
                        fallback
                    } else {
                        unknown_function!(call_location, name, module)
                    }
                };

                if let Some(unaliased_name) = tmp_function.unaliased.clone() {
                    name = unaliased_name;
                };

                if !tmp_function.usable && !func.borrow_mut().imported && !ignore_no_def {
                    panic!(
                        "{}",
                        call_location.error(format!(
                            "Function named '{}' was not imported and can't be used",
                            name
                        ))
                    )
                }

                let mut params = vec![];
                let mut add_meta = false;

                if let Some(inner) = tmp_function.arguments.get(0) {
                    if inner.0.is_struct() {
                        let name = inner.0.get_struct_inner().unwrap();

                        if name == META_STRUCT_NAME {
                            add_meta = true;
                        }
                    }
                }

                if self.generic_functions.contains_key(&name) {
                    self.create_monomorphized_function(
                        &mut name,
                        &mut add_meta,
                        base_known_generics,
                        &mut known_generics,
                        parameters.clone(),
                        module,
                        func,
                        &mut call_location,
                        &mut tmp_function,
                        ty,
                    )
                }

                if type_method {
                    if let Some((ty, _)) = first_param.clone() {
                        let parsed_ty = if ty.is_struct()
                            && is_generic!(ty.get_struct_inner().unwrap())
                        {
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
                            tmp.map(|item| item.0.clone())
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
                            && first_arg.0.is_pointer()
                            && (first_arg.0.get_pointer_inner().unwrap()
                                == first_param.clone().unwrap().0)
                        {
                            got_address = true;

                            parameter.1 = AstNode::Address {
                                value: Box::new(parameter.1),
                                location: call_location.clone(),
                            }
                        }
                    }

                    let (ty, val) = if i == 0 && first_param.is_some() && !got_address {
                        first_param.clone().unwrap()
                    } else {
                        self.generate_statement(
                            func,
                            module,
                            parameter.1,
                            param_ty.clone(),
                            None,
                            false,
                        )
                        .expect(&parameter.0.error(
                            format!(
                                "Unexpected error when trying to generate a statement for a parameter in a function called '{}'",
                                name
                            ))
                        )
                    };

                    params.push(if param_ty.is_none() || ty == param_ty.clone().unwrap() {
                        (ty, val)
                    } else {
                        self.convert_to_type(
                            func,
                            ty.into_abi(),
                            param_ty.unwrap(),
                            val,
                            &parameter.0,
                            &parameter.0,
                            false,
                        )
                    });
                }

                let meta_struct = Compiler::generate_meta_struct(
                    func,
                    &params,
                    parameters.clone(),
                    call_location.clone(),
                );

                if tmp_function.format {
                    for (i, (ty, val)) in params.iter_mut().enumerate() {
                        let fmt_tmp;
                        let fmt_ty;
                        let mut func_name;
                        let mut tmp_function = Function::default();

                        if ty.is_struct() {
                            let struct_name = ty.get_struct_inner().unwrap();
                            func_name = format!("{struct_name}.{FORMAT_CONSTANT}");

                            fmt_tmp =
                                self.new_temporary(Some(&format!("{struct_name}.fmt")), false);
                            fmt_ty = Type::Pointer(Box::new(Type::Char));
                            tmp_function = module
                                .borrow()
                                .functions
                                .iter()
                                .find(|func| func.name == func_name)
                                .cloned()
                                .unwrap_or(Function::default());

                            if is_generic!(struct_name) {
                                let (real_struct_name, _) = Type::from_internal_id(struct_name);
                                func_name = format!("{real_struct_name}.{FORMAT_CONSTANT}");

                                if self.generic_functions.contains_key(&func_name) {
                                    self.create_monomorphized_function(
                                        &mut func_name,
                                        &mut false,
                                        vec![],
                                        &mut hashmap!(),
                                        vec![
                                            parameters[i].clone(),
                                            (
                                                call_location.clone(),
                                                AstNode::Literal {
                                                    kind: TokenKind::IntegerLiteral,
                                                    value: ValueKind::Number(0),
                                                    location: call_location.clone(),
                                                },
                                            ),
                                        ],
                                        module,
                                        func,
                                        &mut call_location,
                                        &mut tmp_function,
                                        None,
                                    )
                                }
                            }
                        } else {
                            func_name = format!("{}.{FORMAT_CONSTANT}", ty.strict_id());

                            fmt_tmp =
                                self.new_temporary(Some(&format!("{}.fmt", ty.strict_id())), false);
                            fmt_ty = Type::Pointer(Box::new(Type::Char));
                            tmp_function = module
                                .borrow()
                                .functions
                                .iter()
                                .find(|func| func.name == func_name)
                                .cloned()
                                .unwrap_or(Function::default());

                            if self.generic_functions.contains_key(&func_name) {
                                self.create_monomorphized_function(
                                    &mut func_name,
                                    &mut false,
                                    vec![],
                                    &mut hashmap!(),
                                    vec![
                                        parameters[i].clone(),
                                        (
                                            call_location.clone(),
                                            AstNode::Literal {
                                                kind: TokenKind::IntegerLiteral,
                                                value: ValueKind::Number(0),
                                                location: call_location.clone(),
                                            },
                                        ),
                                    ],
                                    module,
                                    func,
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
                            panic!(
                                "{}",
                                call_location.error(format!(
                                    "The method \"{}\" returns {}{RESET} but it should return {GREEN}string{RESET}.\nThis method's implementation must be changed to return {GREEN}string{RESET}.",
                                    func_name,
                                    tmp_function.return_type.unwrap_or(Type::Unknown("_".into())).display()
                                ))
                            )
                        }

                        func.borrow_mut().assign_instruction(
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
                    let res = self
                        .generate_statement(
                            func,
                            module,
                            meta_struct,
                            Some(ty.clone()),
                            None,
                            false,
                        )
                        .expect(&call_location.error(
                            "Unexpected error when trying to compile the Elle metadata struct",
                        ));

                    params.insert(0, res);
                }

                if tmp_function.variadic {
                    let res = self
                        .generate_statement(
                            func,
                            module,
                            AstNode::Literal {
                                kind: TokenKind::ExactLiteral,
                                value: ValueKind::String("...".into()),
                                location: call_location.clone(),
                            },
                            Some(ty.clone()),
                            None,
                            false,
                        )
                        .expect(&call_location.error(
                            "Unexpected error when trying to compile the variadic literal '...'",
                        ));

                    params.insert(tmp_function.arguments.len(), res);
                }

                if !tmp_function.variadic {
                    if tmp_function.arguments.len() != params.len() {
                        let only =
                            if tmp_function.arguments.len() > params.len() && params.len() != 0 {
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

                        let arg_len =
                            tmp_function.arguments.len() - add_meta as usize - type_method as usize;
                        let param_len = params.len() - add_meta as usize - type_method as usize;

                        panic!(
                            "{}",
                            call_location.error(format!(
                                "Function named '{}({})' takes {} argument{}, but you {}passed {}\n{}",
                                name.replace(".", "::"),
                                if arg_len > 0 { "..." } else { "" },
                                arg_len,
                                if arg_len == 1 { "" } else { "s" },
                                only,
                                param_len,
                                tmp_function.arguments
                                    .iter()
                                    .skip(params.len())
                                    .map(|(ty, val)| format!("Missing argument named \"{}\" (of type \"{}\")", val.get_string_inner().replace("%", "").split(".").nth(0).unwrap(), ty.display()))
                                    .collect::<Vec<String>>()
                                    .join("\n")
                            ))
                        )
                    }
                }

                let temp = self.new_temporary(None, true);
                let val = if is_callback {
                    let tmp = self.new_temporary(None, true);
                    let res =
                        self.get_variable(&format!("{}.addr", name), Some(func), Some(module));

                    if let Ok((_, addr_val)) = res {
                        func.borrow_mut().assign_instruction(
                            &tmp,
                            &Type::Long,
                            Instruction::Load(Type::Long, addr_val),
                        );

                        tmp
                    } else {
                        self.get_variable(&name, Some(func), Some(module))
                            .unwrap_or((Type::Long, Value::Global(name)))
                            .1
                    }
                } else {
                    self.get_variable(&name, Some(func), Some(module))
                        .unwrap_or((Type::Long, Value::Global(name)))
                        .1
                };

                func.borrow_mut()
                    .assign_instruction(&temp, &ty, Instruction::Call(val, params));

                Some((ty, temp))
            }
            AstNode::Buffer {
                name,
                r#type,
                size,
                location,
            } => {
                let buf_ty = Type::Pointer(Box::new(r#type.clone().unwrap()));

                let (ty, val) = self
                    .generate_statement(
                        func,
                        module,
                        if let Some(ref ty) = r#type {
                            AstNode::BinaryOperation {
                                left: size,
                                right: Box::new(AstNode::Literal {
                                    kind: TokenKind::LongLiteral,
                                    value: ValueKind::Number(ty.size(module) as i128),
                                    location: location.clone(),
                                }),
                                operator: TokenKind::Multiply,
                                treat_as_string: false,
                                dunder_methods: true,
                                location: location.clone(),
                            }
                        } else {
                            AstNode::Literal {
                                kind: TokenKind::LongLiteral,
                                value: ValueKind::Number(0),
                                location: location.clone(),
                            }
                        },
                        ty,
                        None,
                        false,
                    )
                    .expect(&location.error(format!(
                        "Unexpected error when trying to compile size for a buffer named '{}'",
                        name
                    )));

                let tmp = self.new_variable(&buf_ty, &name, Some(func), true, false);

                let (_, converted_val) =
                    self.convert_to_type(func, ty, Type::Long, val, &location, &location, true);

                func.borrow_mut().assign_instruction(
                    &tmp,
                    &buf_ty,
                    Instruction::Alloc8(converted_val.clone()),
                );

                self.buf_metadata.insert(
                    tmp.clone(),
                    (buf_ty.get_pointer_inner().unwrap(), converted_val),
                );

                Some((Type::Pointer(Box::new(buf_ty)), tmp))
            }
            AstNode::MemoryOperation {
                left,
                right,
                value,
                left_location,
                right_location,
                value_location,
                is_deref,
            } => {
                let mut tmp_func = func.borrow().clone().to_owned();
                tmp_func.add_block("start");

                let (left_ty, _) = self
                    .generate_statement(
                        &RefCell::new(tmp_func.clone()),
                        module,
                        *left.clone(),
                        None,
                        None,
                        false,
                    )
                    .expect(&left_location.error(format!(
                        "Unexpected error when trying to compile the left side of a {} statement",
                        if value.is_some() { "store" } else { "load" }
                    )));

                if !is_deref
                    && (left_ty.is_struct()
                        || left_ty.is_pointer() && left_ty.get_pointer_inner().unwrap().is_struct())
                {
                    let struct_name = if left_ty.is_struct() {
                        left_ty.get_struct_inner().unwrap()
                    } else {
                        left_ty
                            .get_pointer_inner()
                            .unwrap()
                            .get_struct_inner()
                            .unwrap()
                    };

                    macro_rules! exists {
                        ($constant:expr) => {
                            module
                                .borrow()
                                .functions
                                .iter()
                                .find(|f| f.name == format!("{struct_name}.{}", $constant))
                                .is_some()
                                || (is_generic!(struct_name)
                                    && self
                                        .generic_functions
                                        .keys()
                                        .find(|f| {
                                            **f == format!(
                                                "{}.{}",
                                                Type::from_internal_id(struct_name.clone()).0,
                                                $constant
                                            )
                                        })
                                        .is_some())
                        };
                    }

                    if (value.is_some() && exists!(STORE_CONSTANT)) || exists!(LOAD_CONSTANT) {
                        let mut parameters =
                            vec![(left_location.clone(), *left), (right_location, *right)];

                        if value.is_some() {
                            parameters.push((value_location, *value.clone().unwrap()))
                        }

                        let node = AstNode::FunctionCall {
                            name: if value.is_some() {
                                STORE_CONSTANT
                            } else {
                                LOAD_CONSTANT
                            }
                            .into(),
                            generics: vec![],
                            parameters,
                            type_method: true,
                            ignore_no_def: false,
                            location: left_location,
                        };

                        return self.generate_statement(func, module, node, None, None, false);
                    }
                }

                let (left_ty, _) = self
                    .generate_statement(func, module, *left.clone(), None, None, false)
                    .expect(&left_location.error(format!(
                        "Unexpected error when trying to compile the left side of a {} statement",
                        if value.is_some() { "store" } else { "load" }
                    )));

                let (right_ty, _) = self
                    .generate_statement(func, module, *right.clone(), None, None, false)
                    .expect(&right_location.error(format!(
                        "Unexpected error when trying to compile the right side of a {} statement",
                        if value.is_some() { "store" } else { "load" }
                    )));

                if !(matches!(left_ty, Type::Pointer(_)) || matches!(right_ty, Type::Pointer(_))) {
                    panic!(
                        "{}",
                        left_location.error(format!(
                            "Cannot {} data {} non-pointer types ({} and {})",
                            if value.is_some() { "store" } else { "load" },
                            if value.is_some() { "to" } else { "from" },
                            left_ty.display(),
                            right_ty.display()
                        ))
                    );
                }

                let inner = if left_ty.is_pointer() {
                    left_ty.get_pointer_inner().unwrap()
                } else {
                    right_ty.get_pointer_inner().unwrap()
                };

                let node = AstNode::BinaryOperation {
                    left: if left_ty.is_pointer() {
                        left.clone()
                    } else {
                        right.clone()
                    },
                    right: Box::new(AstNode::BinaryOperation {
                        left: Box::new(AstNode::Literal {
                            kind: TokenKind::LongLiteral,
                            value: ValueKind::Number(inner.size(module) as i128),
                            location: right_location.clone(),
                        }),
                        right: if left_ty.is_pointer() { right } else { left },
                        operator: TokenKind::Multiply,
                        treat_as_string: false,
                        dunder_methods: true,
                        location: right_location.clone(),
                    }),
                    operator: TokenKind::Add,
                    treat_as_string: false,
                    dunder_methods: true,
                    location: right_location.clone(),
                };

                let (_, compiled_location) = self
                    .generate_statement(func, module, node, None, None, false)
                    .expect(&right_location.error(format!(
                        "Unexpected error when trying to compile the offset of a {} statement",
                        if value.is_some() { "store" } else { "load" }
                    )));

                if let Some(ref val) = value {
                    let (_, compiled) = self
                        .generate_statement(
                            func,
                            module,
                            *val.clone(),
                            Some(inner.clone()),
                            None,
                            false,
                        )
                        .expect(&value_location.error(format!(
                            "Unexpected error when trying to compile the value of a {} statement",
                            if value.is_some() { "store" } else { "load" }
                        )));

                    func.borrow_mut().add_instruction(Instruction::Store(
                        inner.clone(),
                        compiled_location.clone(),
                        compiled,
                    ));

                    return Some((inner, compiled_location));
                }

                let temp = self.new_temporary(Some("load"), true);

                func.borrow_mut().assign_instruction(
                    &temp,
                    &inner,
                    Instruction::Load(inner.clone(), compiled_location),
                );

                Some((inner, temp))
            }
            AstNode::IfStatement {
                condition,
                body,
                else_body,
                location,
            } => {
                self.scopes.push(hashmap!());

                let (_, value) = self
                    .generate_statement(func, module, *condition, ty, None, false)
                    .expect(&location.error(
                        "Unexpected error when trying to compile the condition of an if statement",
                    ));

                self.tmp_counter += 1;

                let true_label = format!("ift.{}", self.tmp_counter);
                let false_label = format!("iff.{}", self.tmp_counter);
                let end_label = format!("end.{}", self.tmp_counter);

                func.borrow_mut().add_instruction(Instruction::JumpNonZero(
                    value,
                    true_label.clone(),
                    if else_body.len() > 0 {
                        false_label.clone()
                    } else {
                        end_label.clone()
                    },
                ));

                func.borrow_mut().add_block(true_label);

                for statement in body.iter() {
                    match statement {
                        AstNode::Literal { kind, .. } => match kind {
                            TokenKind::ExactLiteral => {
                                match self.generate_statement(
                                    func,
                                    module,
                                    statement.clone(),
                                    None,
                                    None,
                                    false,
                                ) {
                                    Some((_, value)) => func
                                        .borrow_mut()
                                        .add_instruction(Instruction::Literal(value)),
                                    _ => {}
                                }
                            }
                            TokenKind::Break | TokenKind::Continue => {
                                self.generate_statement(
                                    func,
                                    module,
                                    statement.clone(),
                                    None,
                                    None,
                                    false,
                                );
                            }
                            _ => {}
                        },
                        _ => match self.generate_statement(
                            func,
                            module,
                            statement.clone(),
                            None,
                            None,
                            false,
                        ) {
                            _ => {}
                        },
                    }
                }

                if else_body.len() > 0 {
                    if !func.borrow_mut().blocks.last().map_or(false, |b| b.jumps()) {
                        func.borrow_mut()
                            .add_instruction(Instruction::Jump(end_label.clone()));
                    }

                    func.borrow_mut().add_block(false_label.clone());

                    for statement in else_body.iter() {
                        match statement {
                            AstNode::Literal { kind, .. } => match kind {
                                TokenKind::ExactLiteral => match self.generate_statement(
                                    func,
                                    module,
                                    statement.clone(),
                                    None,
                                    None,
                                    false,
                                ) {
                                    Some((_, value)) => func
                                        .borrow_mut()
                                        .add_instruction(Instruction::Literal(value)),
                                    _ => {}
                                },
                                TokenKind::Break | TokenKind::Continue => {
                                    self.generate_statement(
                                        func,
                                        module,
                                        statement.clone(),
                                        None,
                                        None,
                                        false,
                                    );
                                }
                                _ => {}
                            },
                            _ => {
                                match self.generate_statement(
                                    func,
                                    module,
                                    statement.clone(),
                                    None,
                                    None,
                                    false,
                                ) {
                                    _ => {}
                                }
                            }
                        }
                    }
                }

                func.borrow_mut().add_block(end_label);
                self.scopes.pop();

                None
            }
            AstNode::WhileLoopStatement {
                condition,
                step,
                body,
                location,
            } => {
                self.scopes.push(hashmap!());

                self.tmp_counter += 1;

                let cond_label = format!("loop.{}.cond", self.tmp_counter);
                let step_label = format!("loop.{}.step", self.tmp_counter);
                let body_label = format!("loop.{}.body", self.tmp_counter);
                let end_label = format!("loop.{}.end", self.tmp_counter);

                self.loop_labels.push(format!("loop.{}", self.tmp_counter));

                func.borrow_mut().add_block(cond_label.clone());

                let (_, value) = self
                    .generate_statement(func, module, *condition, ty.clone(), None, false)
                    .expect(&location.error(
                        "Unexpected error when trying to compile the condition of a while loop",
                    ));

                func.borrow_mut().add_instruction(Instruction::JumpNonZero(
                    value,
                    body_label.clone(),
                    end_label.clone(),
                ));

                func.borrow_mut().add_block(step_label.clone());

                if let Some(step) = step {
                    self.generate_statement(func, module, *step, ty, None, false);
                }

                func.borrow_mut()
                    .add_instruction(Instruction::Jump(cond_label.clone()));

                func.borrow_mut().add_block(body_label.clone());

                for statement in body.iter() {
                    match statement {
                        AstNode::Literal { kind, .. } => match kind {
                            TokenKind::ExactLiteral => {
                                match self.generate_statement(
                                    func,
                                    module,
                                    statement.clone(),
                                    None,
                                    None,
                                    false,
                                ) {
                                    Some((_, value)) => func
                                        .borrow_mut()
                                        .add_instruction(Instruction::Literal(value)),
                                    _ => {}
                                }
                            }
                            TokenKind::Break | TokenKind::Continue => {
                                self.generate_statement(
                                    func,
                                    module,
                                    statement.clone(),
                                    None,
                                    None,
                                    false,
                                );
                            }
                            _ => {}
                        },
                        _ => match self.generate_statement(
                            func,
                            module,
                            statement.clone(),
                            None,
                            None,
                            false,
                        ) {
                            _ => {}
                        },
                    }
                }

                if !func.borrow_mut().blocks.last().map_or(false, |b| b.jumps()) {
                    func.borrow_mut()
                        .add_instruction(Instruction::Jump(step_label));
                }

                func.borrow_mut().add_block(end_label);
                self.loop_labels.pop();
                self.scopes.pop();

                None
            }
            AstNode::VariadicStart {
                name,
                size,
                location,
            } => {
                let (ty, size) = self
                    .generate_statement(func, module, *size, ty, None, false)
                    .expect(&location.error(
                        format!("Unexpected error when trying to compile the size of a variadic statement named '{}'", name)
                    ));

                let (_, final_val) =
                    self.convert_to_type(func, ty, Type::Long, size, &location, &location, false);

                let var = self.new_variable(&Type::Long, &name, Some(func), false, false);

                func.borrow_mut().assign_instruction(
                    &var,
                    &Type::Long,
                    Instruction::Alloc8(final_val),
                );

                func.borrow_mut()
                    .add_instruction(Instruction::VAStart(var.clone()));

                Some((Type::Long, var))
            }
            AstNode::VariadicArgument {
                name,
                r#type,
                location,
            } => {
                let ptr = self
                    .get_variable_lazy(&name, Some(func), Some(module), location.clone())
                    .expect(&location.error(format!(
                        "Unexpected error when trying to get a variable named '{}'",
                        name
                    )))
                    .1;

                let ty = r#type.unwrap_or(Type::Long);
                let tmp = self.new_temporary(Some("next"), true);

                func.borrow_mut().assign_instruction(
                    &tmp,
                    &ty.clone().into_base(),
                    Instruction::VAArg(ptr),
                );

                Some((ty, tmp))
            }
            AstNode::Environment { value, location } => {
                if let Some(value) = value {
                    if !self
                        .data_sections
                        .iter()
                        .find(|data| data.name == ENV_ID)
                        .is_some()
                    {
                        self.data_sections.push(Data {
                            linkage: Linkage::public(),
                            name: ENV_ID.into(),
                            align: None,
                            items: vec![(Type::Long, DataItem::Const(0))],
                        })
                    }

                    let (ty, val) =
                        self.generate_statement(func, module, *value, ty, None, is_return)
                            .expect(&location.error(
                                "Unexpected error when compiling value to set to environment",
                            ));

                    func.borrow_mut().add_instruction(Instruction::Store(
                        ty.clone(),
                        Value::Global(ENV_ID.into()),
                        val.clone(),
                    ));

                    Some((ty, val))
                } else {
                    let ty = Type::Pointer(Box::new(Type::Struct(ENV_STRUCT_NAME.into())));
                    let val = self.new_temporary(None, false);

                    func.borrow_mut().assign_instruction(
                        &val,
                        &ty,
                        Instruction::Load(ty.clone(), Value::Global(ENV_ID.into())),
                    );

                    Some((ty, val))
                }
            }
            AstNode::BlockStatement { body, location: _ } => {
                self.scopes.push(hashmap!());
                self.tmp_counter += 1;

                let body_label = format!("block.start.{}", self.tmp_counter);
                let end_label = format!("block.end.{}", self.tmp_counter);
                func.borrow_mut().add_block(body_label.clone());

                for statement in body.iter() {
                    match statement {
                        AstNode::Literal { kind, .. } => match kind {
                            TokenKind::ExactLiteral => {
                                match self.generate_statement(
                                    func,
                                    module,
                                    statement.clone(),
                                    None,
                                    None,
                                    false,
                                ) {
                                    Some((_, value)) => func
                                        .borrow_mut()
                                        .add_instruction(Instruction::Literal(value)),
                                    _ => {}
                                }
                            }
                            TokenKind::Break | TokenKind::Continue => {
                                self.generate_statement(
                                    func,
                                    module,
                                    statement.clone(),
                                    None,
                                    None,
                                    false,
                                );
                            }
                            _ => {}
                        },
                        _ => match self.generate_statement(
                            func,
                            module,
                            statement.clone(),
                            None,
                            None,
                            false,
                        ) {
                            _ => {}
                        },
                    }
                }

                func.borrow_mut().add_block(end_label);
                self.scopes.pop();
                None
            }
            AstNode::Conversion {
                r#type: second,
                value,
                location,
            } => {
                let (first, val) = self
                    .generate_statement(func, module, *value, ty, None, false)
                    .expect(&location.error("Unexpected error when trying to compile the value of a conversion statement"));

                Some(self.convert_to_type(
                    func,
                    first,
                    second.unwrap(),
                    val,
                    &location,
                    &location,
                    true,
                ))
            }
            AstNode::LogicalNot { value, location } => {
                let (ty, val) = self
                    .generate_statement(func, module, *value, ty, None, false)
                    .expect(&location.error(
                        "Unexpected error when trying to compile the value of a not statement",
                    ));

                let temp = self.new_temporary(Some("not"), true);

                func.borrow_mut().assign_instruction(
                    &temp,
                    &Type::Boolean,
                    Instruction::Compare(
                        Type::Boolean,
                        Comparison::Equal,
                        val,
                        Value::Const(
                            if ty.clone() == Type::Double {
                                "d_"
                            } else if ty.clone() == Type::Single {
                                "s_"
                            } else {
                                ""
                            }
                            .into(),
                            0,
                        ),
                    ),
                );

                Some((ty, temp))
            }
            AstNode::BitWiseNot { value, location } => {
                let (ty, val) = self
                    .generate_statement(func, module, *value, ty, None, false)
                    .expect(&location.error(
                        "Unexpected error when trying to compile the value of a not statement",
                    ));

                let temp = self.new_temporary(Some("negate"), true);

                func.borrow_mut().assign_instruction(
                    &temp,
                    &ty,
                    if ty.is_float() {
                        Instruction::Negate(val)
                    } else {
                        Instruction::BitwiseNot(val)
                    },
                );

                Some((ty, temp))
            }
            AstNode::ArrayLength { value, location } => {
                let (_, val) = self
                    .generate_statement(
                        func,
                        module,
                        AstNode::BinaryOperation {
                            left: value,
                            right: Box::new(AstNode::Literal {
                                kind: TokenKind::IntegerLiteral,
                                value: ValueKind::Number(Type::Word.size(module) as i128),
                                location: location.clone(),
                            }),
                            operator: TokenKind::Subtract,
                            treat_as_string: false,
                            dunder_methods: true,
                            location: location.clone(),
                        },
                        ty,
                        None,
                        false,
                    )
                    .expect(&location.error(
                        "Unexpected error when trying to compile the formula for getting the array length",
                    ));

                let temp = self.new_temporary(Some("array.length"), true);

                func.borrow_mut().assign_instruction(
                    &temp,
                    &Type::Word,
                    Instruction::Load(Type::Word, val),
                );

                Some((Type::Word, temp))
            }
            AstNode::Lambda {
                arguments,
                value,
                location,
            } => {
                self.tmp_counter += 1;
                let lambda_name = format!("lambda.{}", self.tmp_counter);

                let scopes = self.scopes.clone();
                self.scopes = vec![hashmap![]];

                let mut args = vec![];

                for argument in arguments.clone() {
                    let ty = argument.r#type.clone();
                    let tmp = if argument.manual {
                        self.new_manual_argument(&ty, &argument.name)
                    } else {
                        self.new_variable(&ty, &argument.name, None, false, false)
                    };

                    args.push((ty.into_abi(), tmp));
                }

                let lambda_func = self.generate_function(
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
                    &arguments,
                    None,
                    value,
                    module,
                    location.clone(),
                    location,
                );

                self.deferred_functions.push(lambda_func.clone());
                self.scopes = scopes;

                Some((
                    Type::Function(Box::new(Some(lambda_func))),
                    Value::Global(lambda_name),
                ))
            }
            AstNode::ArrayLiteral {
                known_generics,
                values,
                location,
                dynamic,
            } => {
                let inner_ty = if let Some(ty) = ty.clone() {
                    ty.get_pointer_inner()
                } else {
                    None
                };

                if dynamic {
                    let new_func = func.borrow_mut().to_owned();

                    let node = if values.len() > 0 {
                        let (ty, _) = self
                            .generate_statement(
                                &RefCell::new(new_func),
                                module,
                                values[0].clone().1,
                                None,
                                None,
                                false,
                            )
                            .expect(&location.error(format!(
                                "Unexpected error when trying to compile the first item in an array"
                            )));

                        AstNode::FunctionCall {
                            name: "Array.new".into(),
                            generics: vec![ty],
                            parameters: values,
                            type_method: false,
                            ignore_no_def: false,
                            location: location.clone(),
                        }
                    } else {
                        AstNode::FunctionCall {
                            name: "Array.new".into(),
                            generics: known_generics,
                            parameters: vec![],
                            type_method: false,
                            ignore_no_def: false,
                            location: location.clone(),
                        }
                    };

                    let (ty, val) = self
                        .generate_statement(func, module, node, ty, value, is_return)
                        .expect(&location.error(format!(
                            "Unexpected error when trying to compile a dynamic array"
                        )));

                    return Some((ty, val));
                }

                let mut first_type: Option<Type> = None;
                let mut results: Vec<Value> = vec![];

                // value.is_some() because we don't want to do this to
                // arrays that aren't assigned to a variable
                if value.is_some() && ty.is_some() && !ty.clone().unwrap().is_pointer() {
                    panic!(
                        "{}",
                        location.error(
                            format!("The type of array '{:?}' must be a pointer to the inner type of the array (it is {})",
                                values, ty.unwrap().display()
                            )
                        )
                    );
                }

                for (i, (location, value)) in values.iter().enumerate() {
                    let (ty, val) = self
                        .generate_statement(
                            func,
                            module,
                            value.clone(),
                            if inner_ty.is_some() {
                                inner_ty.clone()
                            } else {
                                first_type.clone()
                            },
                            None,
                            false,
                        )
                        .expect(
                            &location.error(
                                format!("Unexpected error when trying to compile an item in an array with index {}", i),
                            ),
                        );

                    results.push(val);

                    if let Some(first_type) = first_type.clone() {
                        if ty != first_type {
                            panic!(
                                "{}",
                                location.error(format!(
                                    "Inconsistent array types '{}' and '{}' (possibly more)",
                                    first_type.display(),
                                    ty.display()
                                ))
                            );
                        }

                        if inner_ty.is_some() && inner_ty.clone().unwrap() != first_type {
                            panic!(
                                "{}",
                                location.error(format!(
                                    "Invalid type of element in array '{}' when the array type is '{}'",
                                    ty.display(), inner_ty.unwrap().display(),
                                ))
                            )
                        }
                    } else {
                        if inner_ty.is_some() && inner_ty.clone().unwrap() != ty {
                            panic!(
                                "{}",
                                location.error(format!(
                                    "Invalid type of element in array '{}' when the array type is '{}'",
                                    ty.display(), inner_ty.unwrap().display(),
                                ))
                            )
                        }

                        first_type = Some(ty);
                    }
                }

                let buf_ty = Type::Pointer(Box::new(first_type.clone().unwrap_or(Type::Void)));
                let array_size = if let Some(ref ty) = first_type {
                    values.len() as u64 * ty.size(module)
                } else {
                    0
                };
                let array_size_val =
                    Value::Const("".into(), (array_size + Type::Word.size_base()) as i128);
                let tmp_full = self.new_temporary(Some("array.full"), true);

                func.borrow_mut().assign_instruction_front(
                    &tmp_full,
                    &buf_ty,
                    Instruction::Alloc8(array_size_val.clone()),
                );

                func.borrow_mut().add_instruction(Instruction::Store(
                    Type::Word,
                    tmp_full.clone(),
                    Value::Const("".into(), results.len() as i128),
                ));

                let tmp = self.new_temporary(Some("array"), true);

                func.borrow_mut().assign_instruction(
                    &tmp,
                    &buf_ty,
                    Instruction::Add(
                        tmp_full,
                        Value::Const("".into(), Type::Word.size(module) as i128),
                    ),
                );

                self.buf_metadata.insert(
                    value.unwrap_or(tmp.clone()),
                    (buf_ty.get_pointer_inner().unwrap(), array_size_val),
                );

                for (i, value) in results.iter().enumerate() {
                    let value_ptr = self.new_temporary(Some("array.offset"), true);

                    func.borrow_mut().assign_instruction(
                        &value_ptr,
                        &Type::Long,
                        Instruction::Add(
                            tmp.clone(),
                            Value::Const(
                                "".into(),
                                i as i128 * first_type.as_ref().unwrap().size(module) as i128,
                            ),
                        ),
                    );

                    func.borrow_mut().add_instruction(Instruction::Store(
                        first_type.as_ref().unwrap().clone(),
                        value_ptr,
                        value.clone(),
                    ));
                }

                Some((buf_ty, tmp))
            }
            AstNode::Address { value, location } => {
                let (ty, val) = self
                    .generate_statement(func, module, *value, ty, None, false)
                    .expect(&location.error(
                        "Unexpected error when trying to compile the value of an address statement",
                    ));

                if let Some(addr_val) = self.address_pool.get(&val) {
                    Some((Type::Pointer(Box::new(ty)), addr_val.clone()))
                } else {
                    let addr_val = self.new_temporary(Some("tmp.addr"), true);
                    let addr_ty = Type::Pointer(Box::new(ty.clone()));

                    func.borrow_mut().assign_instruction_front(
                        &addr_val,
                        &addr_ty,
                        Instruction::Alloc8(Value::Const("".into(), ty.size(module) as i128)),
                    );

                    func.borrow_mut().add_instruction(Instruction::Store(
                        ty.clone(),
                        addr_val.clone(),
                        val.clone(),
                    ));

                    Some((addr_ty, addr_val))
                }
            }
            AstNode::Ternary {
                condition,
                if_true,
                if_false,
                location,
            } => {
                let temp = self.new_temporary(Some("ternary"), false);

                let true_label = format!("ift.{}", self.tmp_counter);
                let false_label = format!("iff.{}", self.tmp_counter);
                let end_label = format!("end.{}", self.tmp_counter);

                let (_, condition_val) = self
                    .generate_statement(func, module, *condition, None, None, false)
                    .expect(&location.error(
                        "Unexpected error when trying to compile the `condition` of a ternary",
                    ));

                func.borrow_mut().add_instruction(Instruction::JumpNonZero(
                    condition_val,
                    true_label.clone(),
                    false_label.clone(),
                ));

                func.borrow_mut().add_block(true_label);

                let (if_true_ty, if_true_val) = self
                    .generate_statement(func, module, *if_true, None, None, is_return)
                    .expect(&location.error(
                        "Unexpected error when trying to compile the `true` path of a ternary",
                    ));

                func.borrow_mut().assign_instruction(
                    &temp,
                    &if_true_ty,
                    Instruction::Copy(if_true_val),
                );

                func.borrow_mut()
                    .add_instruction(Instruction::Jump(end_label.clone()));

                func.borrow_mut().add_block(false_label);

                let (if_false_ty, if_false_val) = self
                    .generate_statement(func, module, *if_false, None, None, is_return)
                    .expect(&location.error(
                        "Unexpected error when trying to compile the `false` path of a ternary",
                    ));

                func.borrow_mut().assign_instruction(
                    &temp,
                    &if_false_ty,
                    Instruction::Copy(if_false_val),
                );

                func.borrow_mut()
                    .add_instruction(Instruction::Jump(end_label.clone()));

                func.borrow_mut().add_block(end_label);
                Some((if_true_ty, temp))
            }
            AstNode::Size { value, location } => match value {
                Ok(ty) => {
                    let tmp_ty = Type::Long;
                    let temp = self.new_temporary(Some("size"), true);

                    func.borrow_mut().assign_instruction(
                        &temp,
                        &tmp_ty,
                        Instruction::Copy(Value::Const("".into(), ty.size(module) as i128)),
                    );

                    Some((tmp_ty, temp))
                }

                Err(value) => {
                    let (ty, val) = self
                        .generate_statement(func, module, *value, ty, None, false)
                        .expect(&location.error(
                            "Unexpected error when trying to compile the size of a statement",
                        ));

                    let size = self.new_temporary(Some("size"), true);

                    match &ty {
                        &Type::Pointer(_) => {
                            let ty = Type::Long;

                            if let Some((_, buf_val)) = self.buf_metadata.get(&val).cloned() {
                                func.borrow_mut().assign_instruction(
                                    &size,
                                    &ty,
                                    Instruction::Copy(buf_val),
                                );

                                return Some((ty, size));
                            }

                            func.borrow_mut().assign_instruction(
                                &size,
                                &ty,
                                Instruction::Copy(Value::Const("".into(), ty.size(module) as i128)),
                            );

                            Some((ty, size))
                        }
                        other => {
                            func.borrow_mut().assign_instruction(
                                &size,
                                &other,
                                Instruction::Copy(Value::Const(
                                    if other.clone() == Type::Double {
                                        "d_"
                                    } else if other.clone() == Type::Single {
                                        "s_"
                                    } else {
                                        ""
                                    }
                                    .into(),
                                    ty.size(module) as i128,
                                )),
                            );

                            Some((other.to_owned(), size))
                        }
                    }
                }
            },
            AstNode::StructLiteral {
                mut name,
                values,
                location,
            } => {
                let inner =
                    ty.unwrap_or(func.borrow_mut().return_type.clone().unwrap_or(Type::Void));

                if inner.is_struct()
                    && is_generic!(inner.get_struct_inner().unwrap())
                    && !is_generic!(name)
                {
                    let generic_name = Type::from_internal_id(inner.get_struct_inner().unwrap()).0;

                    if name == generic_name {
                        name = inner.get_struct_inner().unwrap();
                    }
                }

                if self.struct_pool.get(&name).is_none() {
                    if is_generic!(name) {
                        self.create_monomorphized_struct(module, name.clone())
                    } else {
                        panic!(
                            "{}",
                            location.error(format!(
                                "Could not find struct named '{}'. Did you spell it correctly?\nThis struct may be generic but missing generic parameters.",
                                Type::Struct(name).display()
                            ))
                        )
                    }
                }

                let td = module
                    .borrow()
                    .types
                    .clone()
                    .into_iter()
                    .find(|td| td.name == name)
                    .expect(&format!("Unable to find struct named '{}'", name));

                if !td.usable && !func.borrow_mut().imported {
                    panic!(
                        "{}",
                        location.error(format!(
                            "Struct named '{}' was not imported and can't be used",
                            Type::Struct(name.clone()).display()
                        ))
                    )
                }

                let struct_pool = self.struct_pool.clone();
                let members = struct_pool.get(&name).unwrap().1.clone();
                let member_names = members
                    .iter()
                    .map(|member| member.name.clone())
                    .collect::<Vec<String>>();

                let member_set: HashSet<_> = member_names.iter().cloned().collect();
                let value_set: HashSet<_> = values.iter().map(|value| value.0.clone()).collect();

                let diff: Vec<_> = member_set.difference(&value_set).collect();

                if self.warnings.has_warning(Warning::StructFieldsMissing) {
                    for member in diff.iter().cloned() {
                        println!(
                            "{}",
                            location.warning(format!(
                                "Declaring struct '{}' without field '{}'",
                                Type::Struct(name.clone()).display(),
                                member
                            ))
                        );
                    }
                }

                let ty = Type::Struct(name.clone());
                let size = ty.size(module);

                let alloc_tmp = self.new_temporary(Some(&format!("struct.{name}")), true);

                #[cfg(debug_assertions)]
                func.borrow_mut()
                    .add_instruction(Instruction::Comment(format!("size of :{}", name)));

                func.borrow_mut().assign_instruction_front(
                    &alloc_tmp,
                    &Type::Long,
                    Instruction::Alloc8(Value::Const("".into(), size as i128)),
                );

                for (member_name, value) in values.iter().cloned() {
                    if !member_names.contains(&member_name) {
                        panic!(
                            "{}",
                            location.error(format!(
                                "Struct named '{}' has no field named '{}'. Did you spell it correctly?",
                                name, member_name
                            ))
                        );
                    }

                    let (member_ty, offset) =
                        self.member_to_offset(module, &name, &member_name).unwrap();

                    let (mut ty, mut val) = self
                        .generate_statement(
                            func,
                            module,
                            *value,
                            members
                                .iter()
                                .find(|member| member.name == member_name)
                                .map(|arg| arg.r#type.clone()),
                            None,
                            false,
                        )
                        .expect(
                            &location.error(
                                format!("Unexpected error when trying to compile the value of a field '{}' in struct '{}'", member_name, name)
                            ),
                        );

                    if let Some(member_ty) = member_ty {
                        if ty.weight() > member_ty.weight() || ty.weight() < member_ty.weight() {
                            let (new_ty, new_val) = self.convert_to_type(
                                func,
                                ty.clone(),
                                member_ty.clone(),
                                val,
                                &location,
                                &location,
                                false,
                            );

                            ty = new_ty;
                            val = new_val
                        }
                    }

                    let offset_tmp = self.new_temporary(Some("offset"), true);

                    func.borrow_mut().assign_instruction(
                        &offset_tmp,
                        &Type::Long,
                        Instruction::Add(
                            alloc_tmp.clone(),
                            Value::Const("".into(), offset as i128),
                        ),
                    );

                    if ty.is_struct() {
                        func.borrow_mut().add_instruction(Instruction::Call(
                            Value::Global("memcpy".into()),
                            // The structs must have their pointers diminished
                            // to just a `Long` instead of a `Struct(name)`
                            vec![
                                (Type::Long, offset_tmp),
                                (Type::Long, val),
                                (Type::Word, Value::Const("".into(), ty.size(module) as i128)),
                            ],
                        ))
                    } else {
                        func.borrow_mut()
                            .add_instruction(Instruction::Store(ty, offset_tmp, val))
                    }
                }

                Some((ty, alloc_tmp))
            }
            AstNode::FieldAccess {
                left,
                right,
                value,
                location,
            } => {
                let (ty, left) = self
                    .generate_statement(
                        func,
                        module,
                        *left,
                        ty,
                        None,
                        false,
                    )
                    .expect(
                        &location.error(
                            "Unexpected error when trying to compile the left side of a struct field access"
                        ),
                    );

                let (field_ty, offset_tmp) =
                    self.process_field_access(func, module, ty, left, *right, false);

                if let Some(value) = value {
                    let (_, compiled) = self
                        .generate_statement(func, module, *value, Some(field_ty.clone()), None, false)
                        .expect(&location.error("Unexpected error when trying to compile the value of a store statement"));

                    func.borrow_mut().add_instruction(Instruction::Store(
                        field_ty.clone(),
                        offset_tmp.clone(),
                        compiled,
                    ));

                    return Some((field_ty, offset_tmp));
                }

                let temp = self.new_temporary(Some("field"), true);

                // Structs are stored in contiguous memory.
                // Any field that is a struct should not be dereferenced
                // because that will break everything.
                if field_ty.is_struct() {
                    Some((field_ty, offset_tmp))
                } else {
                    func.borrow_mut().assign_instruction(
                        &temp,
                        &field_ty,
                        Instruction::Load(field_ty.clone(), offset_tmp),
                    );

                    Some((field_ty, temp))
                }
            }
            _ => todo!("statement: {:?}", stmt),
        };

        res
    }

    fn generate_struct(
        &mut self,
        name: String,
        public: bool,
        usable: bool,
        imported: bool,
        generics: Vec<String>,
        known_generics: HashMap<String, Type>,
        members: Vec<Argument>,
        ignore_empty: bool,
        keyword_location: Location,
        _location: Location,
    ) -> TypeDef {
        let mut items = vec![];

        if members.is_empty() && !ignore_empty {
            panic!(
                "{}",
                keyword_location.with_extra_info("Replace this with 'namespace'").error(format!("Cannot declare an empty struct (with no members).\nIf you intended to make a namespace, use the '{GREEN}namespace{RESET}' keyword instead."))
            )
        }

        for member in members.iter().cloned() {
            items.push((member.r#type, 1));
        }

        self.struct_pool
            .insert(name.clone(), (generics, members, keyword_location));

        TypeDef {
            name,
            align: None,
            known_generics,
            items,
            public,
            usable,
            imported,
        }
    }

    fn generate_meta_struct(
        func: &RefCell<Function>,
        params: &Vec<(Type, Value)>,
        parameters: Vec<(Location, AstNode)>,
        location: Location,
    ) -> AstNode {
        let node = AstNode::StructLiteral {
            name: META_STRUCT_NAME.into(),
            values: vec![
                (
                    "exprs".into(),
                    Box::new(AstNode::ArrayLiteral {
                        values: params
                            .iter()
                            .enumerate()
                            .map(|(i, _)| {
                                let location = parameters.get(i).unwrap().0.clone();
                                let ctx = format!("{},", location.get_expr_lead());
                                let mut res = String::new();

                                let mut paren_nesting = 0;
                                let mut block_nesting = 0;
                                let mut curly_nesting = 0;

                                let chars = ctx
                                    .as_bytes()
                                    .iter()
                                    .map(|x| *x as char)
                                    .collect::<Vec<char>>();
                                let mut i = 0;

                                loop {
                                    if i + 1 >= chars.len() {
                                        if paren_nesting > 0
                                            || block_nesting > 0
                                            || curly_nesting > 0
                                        {
                                            res.pop();
                                        }

                                        break;
                                    }

                                    // Wrapped statement, deref, nested function call
                                    if chars[i] == '(' {
                                        paren_nesting += 1;
                                    }

                                    // Inline array
                                    if chars[i] == '[' {
                                        block_nesting += 1;
                                    }

                                    // Struct init
                                    if chars[i] == '{' {
                                        curly_nesting += 1;
                                    }

                                    res.push(chars[i]);
                                    advance!(i, chars);

                                    if chars[i] == ',' {
                                        if paren_nesting > 0
                                            || block_nesting > 0
                                            || curly_nesting > 0
                                        {
                                            res.push(chars[i]);
                                            advance!(i, chars);
                                            continue;
                                        } else {
                                            break;
                                        }
                                    }

                                    if chars[i] == ')' {
                                        if paren_nesting > 0 {
                                            paren_nesting -= 1;
                                        } else {
                                            break;
                                        }
                                    }

                                    if chars[i] == ']' {
                                        if block_nesting > 0 {
                                            block_nesting -= 1;
                                        } else {
                                            break;
                                        }
                                    }

                                    if chars[i] == '}' {
                                        if curly_nesting > 0 {
                                            curly_nesting -= 1;
                                        } else {
                                            break;
                                        }
                                    }
                                }

                                (
                                    location.clone(),
                                    AstNode::Literal {
                                        kind: TokenKind::StringLiteral,
                                        value: ValueKind::String(
                                            res.replace("\\", "\\\\").replace("\"", "\\\""),
                                        ),
                                        location: location.clone(),
                                    },
                                )
                            })
                            .collect(),
                        location: location.clone(),
                        known_generics: vec![],
                        dynamic: false,
                    }),
                ),
                (
                    "types".into(),
                    Box::new(AstNode::ArrayLiteral {
                        values: params
                            .iter()
                            .map(|param| {
                                let inner = param.0.id();

                                (
                                    location.clone(),
                                    AstNode::Literal {
                                        kind: TokenKind::StringLiteral,
                                        value: ValueKind::String(inner),
                                        location: location.clone(),
                                    },
                                )
                            })
                            .collect(),
                        location: location.clone(),
                        known_generics: vec![],
                        dynamic: false,
                    }),
                ),
                (
                    "arity".into(),
                    Box::new(AstNode::Literal {
                        kind: TokenKind::IntegerLiteral,
                        value: ValueKind::Number(params.len() as i128),
                        location: location.clone(),
                    }),
                ),
                (
                    "caller".into(),
                    Box::new(AstNode::Literal {
                        kind: TokenKind::StringLiteral,
                        value: ValueKind::String({
                            let name = func.borrow_mut().name.clone();

                            if name == MAIN_ID {
                                "main".into()
                            } else {
                                name
                            }
                        }),
                        location: location.clone(),
                    }),
                ),
                (
                    "file".into(),
                    Box::new(AstNode::Literal {
                        kind: TokenKind::StringLiteral,
                        value: ValueKind::String(
                            location.file.clone().split("/").last().unwrap().to_string(),
                        ),
                        location: location.clone(),
                    }),
                ),
                (
                    "line".into(),
                    Box::new(AstNode::Literal {
                        kind: TokenKind::IntegerLiteral,
                        value: ValueKind::Number((location.row + 1) as i128),
                        location: location.clone(),
                    }),
                ),
                (
                    "column".into(),
                    Box::new(AstNode::Literal {
                        kind: TokenKind::IntegerLiteral,
                        value: ValueKind::Number((location.column + 1) as i128),
                        location: location.clone(),
                    }),
                ),
            ],
            location,
        };

        return node;
    }

    fn member_to_offset(
        &self,
        module: &RefCell<Module>,
        struct_name: &String,
        member_name: &String,
    ) -> Option<(Option<Type>, u64)> {
        match self.struct_pool.get(struct_name) {
            Some((_, members, ..)) => {
                if !members.iter().any(|member| &member.name == member_name) {
                    return None;
                }

                let mut offset = 0_u64;
                let mut ty = None;

                for member in members.iter() {
                    if &member.name == member_name {
                        ty = Some(member.r#type.clone());
                        break;
                    }

                    offset += member.r#type.size(module)
                }

                Some((ty, offset))
            }
            _ => None,
        }
    }

    fn process_field_access(
        &mut self,
        func: &RefCell<Function>,
        module: &RefCell<Module>,
        mut ty: Type,
        mut left: Value,
        mut right: AstNode,
        load: bool,
    ) -> (Type, Value) {
        loop {
            match right {
                AstNode::Literal {
                    kind,
                    value,
                    location,
                } if kind == TokenKind::Identifier => {
                    let field = value.get_string_inner().unwrap();

                    if !ty.is_struct() {
                        // Automatically deref 'Foo *' into 'Foo' when processing
                        if ty.is_pointer() && ty.get_pointer_inner().unwrap().is_struct() {
                            let tmp = self.new_temporary(Some("load"), true);

                            func.borrow_mut().assign_instruction(
                                &tmp,
                                &Type::Long,
                                Instruction::Load(ty.get_pointer_inner().unwrap(), left),
                            );

                            left = tmp;
                            ty = ty.get_pointer_inner().unwrap();
                        } else {
                            panic!(
                                "{}",
                                &location.error(format!(
                                    "Cannot access fields on a non-struct type '{}' (field '{}')",
                                    ty.display(),
                                    field
                                ))
                            );
                        }
                    }

                    let struct_name = ty.get_struct_inner().unwrap();

                    let (member_ty, offset) = self
                        .member_to_offset(module, &ty.get_struct_inner().unwrap(), &field)
                        .expect(&unknown_field!(
                            self.struct_pool.get(&struct_name).unwrap(),
                            ty,
                            field,
                            location
                        ));

                    let offset_tmp = self.new_temporary(Some("offset"), true);

                    func.borrow_mut().assign_instruction(
                        &offset_tmp,
                        &Type::Long,
                        Instruction::Add(left, Value::Const("".into(), offset as i128)),
                    );

                    if load && !member_ty.clone().unwrap().is_struct() {
                        let tmp = self.new_temporary(Some("load"), true);

                        func.borrow_mut().assign_instruction(
                            &tmp,
                            &Type::Long,
                            Instruction::Load(member_ty.clone().unwrap(), offset_tmp),
                        );

                        return (member_ty.unwrap(), tmp);
                    } else {
                        return (member_ty.unwrap(), offset_tmp);
                    }
                }
                AstNode::FieldAccess {
                    left: nested_left,
                    right: nested_right,
                    ..
                } => {
                    let (nested_ty, nested_left_value) =
                        self.process_field_access(func, module, ty, left, *nested_left, true);

                    ty = nested_ty;
                    left = nested_left_value;
                    right = *nested_right;
                }
                _ => panic!("Unexpected AST node type for field access: {:?}", right),
            }
        }
    }

    fn handle_short_circuiting_operation(
        &mut self,
        left: Box<AstNode>,
        right: Box<AstNode>,
        func: &RefCell<Function>,
        module: &RefCell<Module>,
        ty: Option<Type>,
        is_return: bool,
        location: Location,
        kind: TokenKind,
    ) -> (Type, Value) {
        self.tmp_counter += 1;

        let left_label = format!("{}.left.{}", kind, self.tmp_counter);
        let right_label = format!("{}.right.{}", kind, self.tmp_counter);
        let left_matches_label = format!("{}.left.match.{}", kind, self.tmp_counter);
        let right_matches_label = format!("{}.right.match.{}", kind, self.tmp_counter);
        let end_label = format!("{}.end.{}", kind, self.tmp_counter);

        let result_tmp = self.new_temporary(Some(&kind.to_string()), true);

        let (left_ty, left_val) = self
            .generate_statement(func, module, *left, ty.clone(), None, is_return)
            .expect(&location.error(
                "Unexpected error when trying to parse left side of an arithmetic operation",
            ));

        func.borrow_mut().assign_instruction(
            &result_tmp,
            &left_ty,
            Instruction::Copy(Value::Const(
                if left_ty.clone() == Type::Double {
                    "d_"
                } else if left_ty.clone() == Type::Single {
                    "s_"
                } else {
                    ""
                }
                .into(),
                0,
            )),
        );

        func.borrow_mut().add_block(left_label);

        let left_tmp = self.new_temporary(Some(&format!("{}.left", kind)), true);

        func.borrow_mut().assign_instruction(
            &left_tmp,
            &Type::Boolean,
            Instruction::Compare(
                Type::Boolean,
                Comparison::Equal,
                left_val.clone(),
                Value::Const("".into(), 0),
            ),
        );

        match kind {
            TokenKind::And => {
                func.borrow_mut().add_instruction(Instruction::JumpNonZero(
                    left_tmp,
                    end_label.clone(),
                    right_label.clone(),
                ));
            }
            TokenKind::Or => {
                func.borrow_mut().add_instruction(Instruction::JumpNonZero(
                    left_tmp,
                    right_label.clone(),
                    left_matches_label.clone(),
                ));
            }
            other => panic!(
                "{}",
                &location.error(format!(
                    "Invalid operator token for conditional short circuiting '{}'",
                    other
                ))
            ),
        }

        func.borrow_mut().add_block(right_label);

        let (_, right_val) = self
            .generate_statement(func, module, *right, ty, None, is_return)
            .expect(&location.error(
                "Unexpected error when trying to parse right side of an arithmetic operation",
            ));

        let right_tmp = self.new_temporary(Some(&format!("{}.right", kind)), true);

        func.borrow_mut().assign_instruction(
            &right_tmp,
            &Type::Boolean,
            Instruction::Compare(
                Type::Boolean,
                Comparison::Equal,
                right_val.clone(),
                Value::Const("".into(), 0),
            ),
        );

        // This is the same for AND and OR
        func.borrow_mut().add_instruction(Instruction::JumpNonZero(
            right_tmp,
            end_label.clone(),
            right_matches_label.clone(),
        ));

        func.borrow_mut().add_block(left_matches_label);

        func.borrow_mut()
            .assign_instruction(&result_tmp, &left_ty, Instruction::Copy(left_val));

        func.borrow_mut()
            .add_instruction(Instruction::Jump(end_label.clone()));

        func.borrow_mut().add_block(right_matches_label);

        func.borrow_mut()
            .assign_instruction(&result_tmp, &left_ty, Instruction::Copy(right_val));

        func.borrow_mut()
            .add_instruction(Instruction::Jump(end_label.clone()));

        func.borrow_mut().add_block(end_label);
        return (left_ty, result_tmp);
    }

    fn convert_to_type(
        &mut self,
        func: &RefCell<Function>,
        first: Type,
        second: Type,
        val: Value,
        left_location: &Location,
        right_location: &Location,
        explicit: bool,
    ) -> (Type, Value) {
        if first.is_struct() || second.is_struct() {
            if first == second {
                return (second, val);
            }

            if explicit
                && ((first.is_struct() && second.is_pointer_like())
                    || (second.is_struct() && first.is_pointer_like()))
            {
                return (second, val);
            }

            if first.is_pointer() && first.get_pointer_inner().unwrap() == second {
                let tmp = self.new_temporary(Some("load"), false);

                func.borrow_mut().assign_instruction(
                    &tmp,
                    &second.clone(),
                    Instruction::Load(second.clone(), val),
                );

                return (second, tmp);
            }

            panic!(
                "{}",
                left_location
                    .clone()
                    .with_extra_info(format!("This has the type '{}'", first.display()))
                    .error(format!(
                        "Cannot convert from the type '{}' to the type '{}'.",
                        first.display(),
                        second.display()
                    ))
            )
        }

        if ((first.is_strictly_number() && second.is_string())
            || (second.is_strictly_number() && first.is_string()))
            && !explicit
        {
            panic!(
                "{}",
                right_location.clone().with_extra_info(format!(
                    "This has the type '{}'",
                    first.display()
                )).error(format!(
                    "Cannot implicitly convert '{}' to '{}' or vice versa.\nTo explicitly convert, use the C-like '(type)variable' syntax.",
                    first.display(),
                    second.display()
                ))
            )
        }

        if first.is_pointer()
            && second.is_pointer()
            && (first.get_pointer_inner().unwrap().is_void()
                || second.get_pointer_inner().unwrap().is_void())
        {
            return (second, val);
        }

        if first.weight() == second.weight() {
            return (second, val);
        } else if (first.is_int() && second.is_int()) || (first.is_float() && second.is_float()) {
            let conv = self.new_temporary(Some("conv"), true);
            let is_first_higher = first.weight() > second.weight();

            func.borrow_mut().assign_instruction(
                &conv,
                &second,
                if is_first_higher {
                    if first.is_float() {
                        Instruction::Truncate(val)
                    } else {
                        // Subtyping in QBE means that longs can automatically
                        // work as ints but not vice versa
                        Instruction::Copy(val)
                    }
                } else {
                    Instruction::Extension(first, val)
                },
            );

            return (second, conv);
        } else {
            let conv = self.new_temporary(Some("conv"), true);

            func.borrow_mut().assign_instruction(
                &conv,
                &second,
                Instruction::Conversion(first, second.clone(), val),
            );

            return (second, conv);
        }
    }

    fn create_monomorphized_struct(&mut self, module: &RefCell<Module>, generic_name: String) {
        let (name, parts) = Type::from_internal_id(generic_name.clone());

        let (generics, members, ..) = self
            .struct_pool
            .get(&name)
            .expect(&format!("Base {name} should exist"));

        let parsed_generics = HashMap::from_iter(
            generics
                .iter()
                .enumerate()
                .map(|(i, generic)| (generic.clone(), parts[i].clone())),
        );

        let struct_pool = RefCell::new(self.struct_pool.clone());
        let tree = RefCell::new(vec![]);

        let parsed_members = members
            .iter()
            .map(|member| Argument {
                name: member.name.clone(),
                r#type: member.r#type.clone().unknown_to_known(
                    Some(&struct_pool),
                    Some(&tree),
                    generics.clone(),
                    parsed_generics.clone(),
                ),
                manual: member.manual,
            })
            .collect::<Vec<Argument>>();

        self.struct_pool = struct_pool.borrow().to_owned();

        for primitive in tree.borrow().to_owned().into_iter() {
            match primitive {
                Primitive::Struct {
                    name,
                    public,
                    usable,
                    imported,
                    generics,
                    known_generics,
                    members,
                    keyword_location,
                    location,
                    ignore_empty,
                } => {
                    let td = self.generate_struct(
                        name,
                        public,
                        usable,
                        imported,
                        generics,
                        known_generics,
                        members,
                        ignore_empty,
                        keyword_location,
                        location,
                    );

                    module.borrow_mut().add_type(td);
                }
                _ => {}
            };
        }

        let mut items = vec![];

        for member in parsed_members.iter().cloned() {
            items.push((member.r#type, 1));
        }

        let td = TypeDef {
            name: generic_name.clone(),
            align: None,
            known_generics: parsed_generics,
            items,
            public: false,
            usable: true,
            imported: false,
        };

        module.borrow_mut().add_type(td);

        self.struct_pool.insert(
            generic_name.clone(),
            (
                vec![],
                parsed_members,
                Location::default(self.output_path.clone()),
            ),
        );
    }

    fn create_monomorphized_function(
        &mut self,
        name: &mut String,
        add_meta: &mut bool,
        base_known_generics: Vec<Type>,
        known_generics: &mut HashMap<String, Type>,
        parameters: Vec<(Location, AstNode)>,
        module: &RefCell<Module>,
        func: &RefCell<Function>,
        call_location: &mut Location,
        tmp_function: &mut Function,
        ty: Option<Type>,
    ) {
        loop {
            match self.generic_functions.get(&name.clone()).unwrap().clone() {
                Primitive::Function { unaliased, .. } => {
                    if unaliased.is_none() {
                        break;
                    }

                    *name = unaliased.clone().unwrap_or(name.to_string());
                }
                _ => {}
            };
        }

        match self.generic_functions.get(&name.clone()).unwrap().clone() {
            Primitive::Function {
                name: _,
                public,
                usable,
                imported,
                variadic,
                manual,
                external,
                builtin,
                volatile,
                format,
                unaliased,
                generics,
                arguments,
                r#return,
                body,
                location,
                return_location,
            } => {
                // Reassign it if the function is generic
                // as the function won't have been found last time
                if let Some(inner) = arguments.get(0) {
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
                if base_known_generics.len() <= generics.len() {
                    known_generics.extend(HashMap::<String, Type>::from_iter(
                        base_known_generics
                            .iter()
                            .enumerate()
                            .map(|(i, known)| (generics[i].clone(), known.clone()))
                            .collect::<Vec<(String, Type)>>(),
                    ));
                }

                for (i, parameter) in parameters.iter().cloned().enumerate() {
                    let param_ty = {
                        let tmp = arguments.get(i + *add_meta as usize);

                        if tmp.is_some()
                            && !Type::Void.has_generic_type(tmp.unwrap().r#type.clone())
                        {
                            tmp.map(|item| item.r#type.clone())
                        } else {
                            None
                        }
                    };

                    // Use an empty func as to not cause duplicate codegen and/or side effects
                    let mut tmp_func = func.borrow().to_owned();
                    tmp_func.add_block("start");

                    let (ty, _) = self.generate_statement(
                        &RefCell::new(tmp_func),
                        module,
                        parameter.1,
                        param_ty.clone(),
                        None,
                        false,
                    )
                    .expect(&parameter.0.error(
                        format!(
                            "Unexpected error when trying to generate a statement for a parameter in a function called '{}'",
                            name
                        ))
                    );

                    let other = {
                        let tmp = arguments.get(i + *add_meta as usize);

                        if tmp.is_some() {
                            tmp.map(|item| item.r#type.clone())
                        } else {
                            None
                        }
                    }
                    .unwrap_or(Type::Void);

                    if ty.clone().has_generic_type(other.clone())
                        && known_generics.len() < generics.len()
                    {
                        // Possibly Option.generic.8 and Option
                        if let Some(inner) = ty.clone().deduce_generic_type(other.clone()) {
                            known_generics.extend(inner)
                        } else if other.is_unknown() && other.get_unknown_inner().unwrap() == "fn" {
                            println!(
                                "{}",
                                location.warning(format!(
                                    "Failed to deduce a generic type from {} and {}",
                                    ty.display(),
                                    other.display()
                                ),)
                            )
                        }
                    }
                }

                if let Some(other) = r#return.clone() {
                    if let Some(ty) = ty {
                        if ty.clone().has_generic_type(other.clone())
                            && known_generics.len() < generics.len()
                        {
                            // Possibly Option.generic.8 and Option
                            if let Some(inner) = ty.clone().deduce_generic_type(other.clone()) {
                                known_generics.extend(inner)
                            } else if other.is_unknown()
                                && other.get_unknown_inner().unwrap() == "fn"
                            {
                                println!(
                                    "{}",
                                    location.warning(format!(
                                        "Failed to deduce a generic type from {} and {}",
                                        ty.display(),
                                        other.display()
                                    ),)
                                )
                            }
                        }
                    }

                    if let Some(ty) = func.borrow().return_type.clone() {
                        if ty.clone().has_generic_type(other.clone())
                            && known_generics.len() < generics.len()
                        {
                            // Possibly Option.generic.8 and Option
                            if let Some(inner) = ty.clone().deduce_generic_type(other.clone()) {
                                known_generics.extend(inner)
                            } else if other.is_unknown()
                                && other.get_unknown_inner().unwrap() == "fn"
                            {
                                println!(
                                    "{}",
                                    location.warning(format!(
                                        "Failed to deduce a generic type from {} and {}",
                                        ty.display(),
                                        other.display()
                                    ),)
                                )
                            }
                        }
                    }
                }

                if generics.len() != known_generics.len() {
                    if generics.len() < known_generics.len() {
                        todo!("the user passed too many generics");
                    }

                    let a: HashSet<_> = generics.iter().cloned().collect();
                    let b: HashSet<_> = known_generics.keys().cloned().collect();

                    let diff: Vec<_> = a.difference(&b).cloned().collect();

                    call_location.column -=
                        call_location.ctx.len() - call_location.ctx.trim().len();
                    call_location.ctx = Rc::new(call_location.ctx.trim().into());
                    call_location.above = Some(Rc::new(format!(
                        "In function:\n{GREEN}{BOLD}{}{}{RESET}\n\n",
                        " ".repeat(
                            call_location.ctx.len() - call_location.ctx.trim().len()
                                + format!("{}", call_location.row + 1).len()
                                + 8
                        ),
                        location.ctx
                    )));

                    panic!(
                        "{}",
                        call_location.error(format!(
                            "Mismatched number of generics in function {}<{}>({}).\nCould not find generic{} {} where the function specifies <{}>.",
                            name.replace(".", "::"),
                            generics.join(", "),
                            if arguments.len() > 0 { "..." } else { "" },
                            if diff.len() == 1 { "" } else { "s" },
                            diff.join(", "),
                            generics.join(", ")
                        ))
                    )
                }

                let generic_name = format!(
                    "{name}.{GENERIC_IDENTIFIER}.{}.{GENERIC_END}",
                    generics
                        .iter()
                        .map(|generic| {
                            known_generics
                                .get(generic)
                                .unwrap()
                                .to_internal_id()
                                .to_string()
                        })
                        .collect::<Vec<String>>()
                        .join(".")
                );

                let existing = module
                    .borrow()
                    .functions
                    .iter()
                    .find(|function| function.name == generic_name)
                    .cloned();

                *name = generic_name.clone();

                if existing.is_none() {
                    // Temporarily empty the scopes
                    let scopes = self.scopes.clone();
                    self.scopes = vec![hashmap![]];

                    let struct_pool = RefCell::new(self.struct_pool.clone());
                    let tree = RefCell::new(vec![]);

                    let parsed_arguments = &arguments
                        .iter()
                        .cloned()
                        .map(|arg| Argument {
                            name: arg.name,
                            r#type: arg.r#type.unknown_to_known(
                                Some(&struct_pool),
                                Some(&tree),
                                generics.clone(),
                                known_generics.clone(),
                            ),
                            manual: arg.manual,
                        })
                        .collect::<Vec<Argument>>();

                    let parsed_return = if r#return.is_some() {
                        Some(r#return.unwrap().unknown_to_known(
                            Some(&struct_pool),
                            Some(&tree),
                            generics.clone(),
                            known_generics.clone(),
                        ))
                    } else {
                        r#return
                    };

                    let parsed_body = modify_type_in_ast(
                        body,
                        &generics,
                        &known_generics,
                        Some(&struct_pool),
                        Some(&tree),
                    );

                    self.struct_pool = struct_pool.borrow().to_owned();

                    for primitive in tree.borrow().to_owned().into_iter() {
                        match primitive {
                            Primitive::Struct {
                                name,
                                public,
                                usable,
                                imported,
                                generics,
                                known_generics,
                                members,
                                keyword_location,
                                location,
                                ignore_empty,
                            } => {
                                let td = self.generate_struct(
                                    name,
                                    public,
                                    usable,
                                    imported,
                                    generics,
                                    known_generics,
                                    members,
                                    ignore_empty,
                                    keyword_location,
                                    location,
                                );

                                module.borrow_mut().add_type(td);
                            }
                            _ => {}
                        };
                    }

                    let function = self.generate_function(
                        generic_name,
                        public,
                        variadic,
                        manual,
                        external,
                        builtin,
                        volatile,
                        format,
                        false,
                        unaliased,
                        usable,
                        imported,
                        vec![],
                        known_generics.clone(),
                        parsed_arguments,
                        parsed_return,
                        parsed_body,
                        &module,
                        location,
                        return_location,
                    );

                    module.borrow_mut().add_function(function.clone());
                    *tmp_function = function;

                    // Bring them back
                    self.scopes = scopes;
                } else {
                    *tmp_function = existing.unwrap();
                }
            }
            _ => {}
        };
    }

    pub fn compile(
        tree: Vec<Primitive>,
        output_path: String,
        warnings: Warnings,
        object_output: bool,
        string_module_methods: Vec<String>,
    ) {
        let mut generator = Compiler {
            tmp_counter: 0,
            scopes: vec![],
            data_sections: vec![],
            generic_functions: hashmap![],
            struct_pool: hashmap![],
            loop_labels: vec![],
            buf_metadata: hashmap![],
            warnings,
            tree,
            deferred_functions: vec![],
            address_pool: hashmap![],
            output_path: output_path.clone(),
        };

        let module = Module::new();

        // We need internal mutability here
        // Each string data section needs to be added to the module
        let module_ref = RefCell::new(module);

        if generator
            .tree
            .iter()
            .find(|primitive| match primitive {
                Primitive::Function { name, .. } if &(name.to_owned()) == "main" => true,
                _ => false,
            })
            .is_none()
            && !object_output
        {
            panic!(
                "\n{}\nERROR: Could not compile module \"{output_path}\"\n{}\n\n{}\n{}\n",
                "-".repeat(40),
                "Module has no entry-point. To create one, write:",
                "fn main() {\n\n\n}",
                "-".repeat(40),
            )
        }

        for primitive in generator.tree.clone() {
            match primitive.clone() {
                Primitive::Constant {
                    name,
                    public,
                    r#type: ty,
                    value,
                    usable,
                    imported,
                    location,
                } => {
                    let function = generator.generate_function(
                        name.clone(),
                        public,
                        false,
                        false,
                        false,
                        false,
                        false,
                        false,
                        false,
                        None,
                        usable,
                        imported,
                        vec![],
                        hashmap![],
                        &vec![],
                        ty,
                        vec![AstNode::Return {
                            value,
                            location: location.clone(),
                        }],
                        &module_ref,
                        location.clone(),
                        location,
                    );

                    module_ref.borrow_mut().add_function(function);
                }
                Primitive::Function {
                    name,
                    public,
                    variadic,
                    manual,
                    external,
                    builtin,
                    volatile,
                    format,
                    unaliased,
                    generics,
                    arguments,
                    r#return,
                    body,
                    usable,
                    location,
                    return_location,
                    imported,
                } => {
                    if generics.is_empty() {
                        let function = generator.generate_function(
                            name,
                            public,
                            variadic,
                            manual,
                            external,
                            builtin,
                            volatile,
                            format,
                            false,
                            unaliased,
                            usable,
                            imported,
                            generics,
                            hashmap![],
                            &arguments,
                            r#return,
                            body,
                            &module_ref,
                            location,
                            return_location,
                        );

                        module_ref.borrow_mut().add_function(function);

                        for func in generator.deferred_functions.clone() {
                            module_ref.borrow_mut().add_function(func);
                        }

                        generator.deferred_functions.clear();
                    } else {
                        generator.generic_functions.insert(name, primitive);
                    }
                }
                Primitive::Struct {
                    name,
                    public,
                    usable,
                    imported,
                    members,
                    generics,
                    known_generics,
                    keyword_location,
                    location,
                    ignore_empty,
                } => {
                    let td = generator.generate_struct(
                        name.clone(),
                        public,
                        usable,
                        imported,
                        generics,
                        known_generics,
                        members,
                        ignore_empty,
                        keyword_location,
                        location,
                    );

                    if module_ref
                        .borrow()
                        .types
                        .iter()
                        .find(|other_td| **other_td == td)
                        .is_none()
                    {
                        module_ref.borrow_mut().add_type(td);
                    }
                }
                _ => {}
            }
        }

        for data in generator.data_sections {
            module_ref.borrow_mut().add_data(data);
        }

        module_ref.borrow_mut().remove_unused_functions();
        module_ref.borrow_mut().remove_unused_data();
        module_ref.borrow_mut().remove_generics();
        module_ref.borrow_mut().remove_empty_structs();

        // Specifically remove methods defined in std/string.le
        // These methods will be found at runtime by the primary
        // file which actually produces an executable
        if object_output {
            module_ref
                .borrow_mut()
                .functions
                .retain(|f| !string_module_methods.contains(&f.name))
        }

        let mut file = File::create(output_path).expect("Failed to create the file.");
        file.write_all(module_ref.borrow().to_string().as_bytes())
            .expect(&format!("{RED}Failed to write to file."));

        file.flush().expect("Failed to flush file");
    }
}
