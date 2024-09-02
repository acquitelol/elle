use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fs::File,
    io::Write,
};

use crate::{
    advance, cast_warning, hashmap,
    lexer::enums::{Location, TokenKind, ValueKind},
    misc::colors::RED,
    parser::enums::{Argument, AstNode, Primitive},
    unknown_field, unknown_function, Warning, Warnings, META_STRUCT_NAME,
};

type GeneratorResult<T> = Result<T, String>;

use super::enums::{
    Comparison, Data, DataItem, Function, Instruction, Linkage, Module, Statement, Type, TypeDef,
    Value,
};

pub struct Compiler {
    tmp_counter: u32,
    scopes: Vec<HashMap<String, (Type, Value)>>,
    data_sections: Vec<Data>,
    type_sections: Vec<TypeDef>,
    // Struct Name => (Field Name, Field Type)[]
    struct_pool: HashMap<String, Vec<Argument>>,
    loop_labels: Vec<String>,
    ret_types: HashMap<String, Type>,
    buf_metadata: HashMap<Value, (Type, Value)>,
    tree: Vec<Primitive>,
    warnings: Warnings,
}

impl Compiler {
    fn tmp_name_with_debug_assertions(&self, name: &str, minify: bool) -> String {
        if cfg!(debug_assertions) || !minify {
            format!("{}.{}", name, self.tmp_counter)
        } else {
            format!(".{}", self.tmp_counter)
        }
    }

    fn new_temporary(&mut self, name: Option<&str>, minify: bool) -> Value {
        self.tmp_counter += 1;
        Value::Temporary(self.tmp_name_with_debug_assertions(name.unwrap_or("tmp"), minify))
    }

    fn new_variable(
        &mut self,
        ty: &Type,
        name: &str,
        func: Option<&RefCell<Function>>,
        new: bool,
        minify: bool,
    ) -> GeneratorResult<Value> {
        let tmp = if new {
            self.new_temporary(Some(name), minify)
        } else {
            let existing_var = self.get_variable(name, func);

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

        Ok(tmp)
    }

    fn get_variable(
        &mut self,
        name: &str,
        func: Option<&RefCell<Function>>,
    ) -> GeneratorResult<(Type, Value)> {
        let var = self
            .scopes
            .iter()
            .rev()
            .filter_map(|s| s.get(name))
            .next()
            .ok_or_else(|| format!("\nUndefined variable '{}'", name));

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
                                Type::Pointer(Box::new(Type::Byte)),
                                Value::Global(name.into()),
                            ));
                        }
                    }
                    _ => {}
                }
            }
        }

        var.map(|item| item.to_owned())
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
        unaliased: Option<String>,
        usable: bool,
        imported: bool,
        arguments: &Vec<Argument>,
        return_type: Option<Type>,
        body: Vec<AstNode>,
        module: &RefCell<Module>,
        location: Location,
    ) -> GeneratorResult<Function> {
        self.scopes.push(hashmap!());

        let mut args = vec![];

        for argument in arguments {
            let ty = argument.r#type.clone();
            let tmp = self.new_variable(&ty, &argument.name, None, false, false)?;

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
            // Make a good guess if the function isn't defined as variadic
            variadic_index: if variadic { args.len() } else { 1 },
            manual,
            external,
            builtin,
            volatile,
            unaliased,
            usable,
            imported,
            arguments: args,
            return_type,
            blocks: vec![],
        };

        if external {
            return Ok(func);
        }

        if let Some(ty) = func.return_type.clone() {
            self.ret_types.insert(name.clone(), ty);
        }

        func.add_block("start");

        let func_ref = RefCell::new(func.clone());

        // Could be a tail call recursion
        //
        // The compiler is single pass which means that
        // we need to forward-declare the function with an empty body
        module.borrow_mut().add_function(func);

        for statement in body.iter() {
            // Ignore plain literals that aren't assigned to anything
            match statement {
                AstNode::LiteralStatement { kind, .. } => match kind {
                    TokenKind::ExactLiteral => {
                        match self.generate_statement(
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
                        }
                    }
                    _ => {}
                },
                _ => {
                    match self.generate_statement(
                        &func_ref,
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

        let mut first_ty: Option<Type> = None;

        macro_rules! ty_err_message {
            ($first:expr, $second:expr, $location:expr $(,)?) => {
                $location.error(format!(
                    "Inconsistent return types in function '{}': {:?} and {:?}",
                    func_ref.borrow().name.replace(".", "::"),
                    $first,
                    $second
                ))
            };
        }

        for block in func_ref.borrow().blocks.iter() {
            for statement in block.statements.clone() {
                if let Statement::Volatile(Instruction::Return(val)) = statement {
                    if let Some(val) = val {
                        if first_ty.is_none() {
                            first_ty = Some(val.0)
                        } else {
                            let ret_ty = val.0.clone();
                            let fst_ty = first_ty.clone().unwrap();

                            if ret_ty != fst_ty
                                && !matches!(val.1, Value::Const(_, _))
                                && !(ret_ty.is_pointer()
                                    && fst_ty.is_pointer()
                                    && (ret_ty.get_pointer_inner().unwrap().is_void()
                                        || fst_ty.get_pointer_inner().unwrap().is_void()))
                            {
                                panic!("{}", ty_err_message!(val.0, first_ty.unwrap(), val.2))
                            }
                        }
                    }
                }
            }
        }

        if first_ty.is_some() {
            let return_ty = func_ref.borrow_mut().return_type.clone();

            if return_ty.is_none() {
                func_ref.borrow_mut().return_type = first_ty;
            } else {
                let ret_ty = return_ty.clone().unwrap();
                let fst_ty = first_ty.clone().unwrap();

                if ret_ty != fst_ty
                    && !(ret_ty.is_pointer()
                        && fst_ty.is_pointer()
                        && (ret_ty.get_pointer_inner().unwrap().is_void()
                            || fst_ty.get_pointer_inner().unwrap().is_void()))
                {
                    panic!(
                        "{}",
                        ty_err_message!(return_ty.unwrap(), first_ty.unwrap(), location)
                    )
                }
            }
        }

        if !func_ref.borrow_mut().returns() && !func_ref.borrow_mut().manual {
            func_ref
                .borrow_mut()
                .add_instruction(Instruction::Return(Some((
                    Type::Word,
                    Value::Const(Type::Word, 0),
                    location,
                ))));
        }

        self.scopes.pop();

        let mut func_new = func_ref.borrow_mut().to_owned();

        if func_new.return_type.is_none() {
            func_new.return_type = Some(Type::Word)
        }

        func_new.return_type = func_new.return_type.map(|ty| ty.into_abi());

        // Remove the empty function from the module
        // it will be added automatically when this function leaves scope
        module
            .borrow_mut()
            .functions
            .retain(|func| func.name != name);

        Ok(func_new)
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
            AstNode::DeclareStatement {
                name,
                r#type,
                value,
                location,
            } => {
                let existing = match self.get_variable(name.as_str(), Some(func)) {
                    Ok((ty, _)) => ty,
                    Err(_) => Type::Word,
                };

                if r#type.is_none() && self.get_variable(name.as_str(), Some(func)).is_err() {
                    panic!(" {}", location.error(format!("Variable named '{}' hasn't been declared yet.\nPlease declare it before trying to re-declare it.", name)));
                }

                let res = self.get_variable(&format!("{}.addr", name), Some(func));
                let ty = r#type.unwrap_or(existing);

                let temp = self
                    .new_variable(&ty, &name, Some(func), false, false)
                    .expect(&location.error(format!(
                        "Unexpected error when trying to make a new variable called '{}'",
                        name
                    )));

                let parsed = self.generate_statement(
                    func,
                    module,
                    *value,
                    Some(ty.clone()),
                    Some(temp.clone()),
                    false,
                );

                if let Some((ret_ty, value)) = parsed {
                    let (final_ty, final_val) = if ret_ty != ty {
                        self.convert_to_type(func, ret_ty, ty.clone(), value, &location, false)
                    } else {
                        (ty.clone(), value.clone())
                    };

                    if res.is_ok() {
                        let (addr_ty, addr_val) = res.unwrap();
                        let tmp = self
                            .new_variable(&addr_ty, &name, Some(func), true, false)
                            .expect(&location.error(format!(
                                "Unexpected error when trying to make a new variable called '{}'",
                                name
                            )));

                        if addr_ty != final_ty
                            && !(addr_ty.is_pointer()
                                && final_ty.is_pointer()
                                && final_ty.get_pointer_inner().unwrap().is_void())
                        {
                            panic!(
                                "{}",
                                location.error(format!(
                                    "Cannot redeclare '{}' which has type {:?} to type {:?}",
                                    name, addr_ty, final_ty
                                ))
                            )
                        }

                        func.borrow_mut().add_instruction(Instruction::Store(
                            addr_ty.clone(),
                            addr_val.clone(),
                            final_val,
                        ));

                        func.borrow_mut().assign_instruction(
                            &tmp,
                            &addr_ty,
                            Instruction::Load(addr_ty.clone(), addr_val),
                        );

                        return Some((addr_ty, tmp));
                    }

                    let addr_temp = self
                        .new_variable(&ty, &format!("{}.addr", name), Some(func), false, false)
                        .expect(&location.error(format!("Unexpected error when trying to create a variable to store the stack address of a local variable named '{}'", name)));

                    func.borrow_mut().assign_instruction(
                        &addr_temp,
                        &Type::Pointer(Box::new(final_ty.clone())),
                        Instruction::Alloc8(Value::Const(
                            Type::Word,
                            final_ty.size(module) as i128,
                        )),
                    );

                    func.borrow_mut().add_instruction(Instruction::Store(
                        final_ty.clone(),
                        addr_temp,
                        final_val,
                    ));

                    return Some((final_ty, temp));
                }

                None
            }
            AstNode::ReturnStatement {
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
            AstNode::ArithmeticOperation {
                left,
                right,
                operator,
                location,
            } => {
                // Implement conditional short circuiting for logical AND and OR
                if matches!(operator, TokenKind::And | TokenKind::Or) {
                    return Some(self.handle_short_circuiting_operation(
                        left, right, func, module, ty, is_return, location, operator,
                    ));
                }

                let (left_ty_unparsed, left_val_unparsed) = self
                    .generate_statement(func, module, *left, ty.clone(), None, is_return)
                    .expect(&location.error(
                        "Unexpected error when trying to parse left side of an arithmetic operation"
                    ));

                let (right_ty_unparsed, right_val_unparsed) = self
                    .generate_statement(func, module, *right, ty.clone(), None, is_return)
                    .expect(&location.error(
                        "Unexpected error when trying to parse right side of an arithmetic operation"
                    ));

                let mut left_ty = left_ty_unparsed.clone();
                let mut left_val = left_val_unparsed.clone();
                let mut right_val = right_val_unparsed.clone();

                if left_ty_unparsed.weight() > right_ty_unparsed.weight() {
                    let (_, val) = self.convert_to_type(
                        func,
                        right_ty_unparsed,
                        left_ty_unparsed,
                        right_val_unparsed,
                        &location,
                        false,
                    );

                    right_val = val;
                } else if left_ty_unparsed.weight() < right_ty_unparsed.weight() {
                    let (ty, val) = self.convert_to_type(
                        func,
                        left_ty_unparsed,
                        right_ty_unparsed,
                        left_val_unparsed,
                        &location,
                        false,
                    );

                    left_ty = ty;
                    left_val = val;
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
            AstNode::LiteralStatement {
                kind,
                value,
                location,
            } => match kind {
                TokenKind::Identifier => match value {
                    ValueKind::String(name) => {
                        let var = self.get_variable(&name, Some(func));

                        match var {
                            Ok((ty, val)) => {
                                let res = self.get_variable(&format!("{}.addr", name), Some(func));

                                if res.is_ok() {
                                    let (_, addr_val) = res.unwrap();

                                    func.borrow_mut().assign_instruction(
                                        &val,
                                        &ty,
                                        Instruction::Load(ty.clone(), addr_val),
                                    );

                                    return Some((ty, val));
                                }

                                Some((ty, val))
                            }
                            Err(msg) => {
                                // If it fails to get the variable from the current scope
                                // then attempt to get it from a global instead
                                let tmp_module = module.borrow();
                                let global = tmp_module.data.iter().find(|item| item.name == name);

                                if let Some(item) = global {
                                    Some((Type::Long, Value::Global(item.name.clone())))
                                } else {
                                    panic!(
                                        "{}",
                                        location.error(
                                            format!("Unexpected error when trying to get a variable called '{}': {}",
                                            name, msg
                                        ))
                                    );
                                }
                            }
                        }
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

                        let mut final_ty = ty.unwrap_or(num_ty);

                        if is_return {
                            final_ty = func.borrow_mut().return_type.clone().unwrap_or(final_ty);
                        }

                        Some((final_ty.clone(), Value::Const(final_ty, val)))
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
                        Some((Type::Char, Value::Const(Type::Word, val as i128)))
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
                parameters,
                type_method,
                ignore_no_def,
                location,
            } => {
                // Get the type of the functions based on the ones currently imported into this module
                let cached_ty = self.ret_types.get(&name).unwrap_or(&Type::Word).to_owned();
                let declarative_ty = ty.unwrap_or(cached_ty);
                let mut should_get_address = false; // Gets address if first arg's ty is the same as ty predicate
                let mut first_param = None;

                if type_method {
                    let parameter =
                        parameters.get(0).expect(&location.error(
                            "Tried to get the 0th parameter to parse struct call but failed",
                        ));

                    let (ty, val) = self.generate_statement(
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

                    // struct access
                    if ty.is_struct() {
                        should_get_address = true;
                        name = format!("{}.{}", ty.get_struct_inner().unwrap(), name)
                    // struct * access
                    } else if ty.is_pointer() && ty.get_pointer_inner().unwrap().is_struct() {
                        name = format!(
                            "{}.{}",
                            ty.get_pointer_inner().unwrap().get_struct_inner().unwrap(),
                            name
                        )
                    // string access
                    } else if ty.is_pointer() && ty.get_pointer_inner().unwrap() == Type::Char {
                        should_get_address = true;
                        name = format!("string.{}", name)
                    // string * access
                    } else if ty.is_pointer()
                        && ty.get_pointer_inner().unwrap().is_pointer()
                        && ty.get_pointer_inner().unwrap().get_pointer_inner().unwrap()
                            == Type::Char
                    {
                        name = format!("string.{}", name)
                    // primitive access
                    } else {
                        name = format!("{}.{}", ty.id(), name)
                    }

                    // The first param needs to be compiled to get its type
                    // however if the first param is mutating (ie `yield()`)
                    // then there will be a double yield causing many issues.
                    // Therefore, we store the first param here to be
                    // used later when compiling all of the params instead
                    // of compiling the first parameter again
                    first_param = Some((ty, val));
                }

                let tmp_function_option = module
                    .borrow_mut()
                    .functions
                    .iter()
                    .find(|function| function.name == name)
                    .map(|function| function.clone());

                let tmp_function = if let Some(func) = tmp_function_option {
                    func
                } else {
                    // Function could be a callback pointer
                    let callback = self.get_variable(name.as_str(), Some(func));

                    let fallback = Function {
                        linkage: Linkage::public(),
                        name: name.clone(),
                        variadic: false,
                        variadic_index: 0,
                        manual: false,
                        external: false,
                        builtin: false,
                        volatile: false,
                        unaliased: None,
                        usable: true,
                        imported: false,
                        // TODO: Allow the function declaration to specify a real signature instead of just `fn *`
                        arguments: parameters.iter().map(|param| {
                            self.generate_statement(
                                func,
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
                        {
                            fallback
                        } else {
                            unknown_function!(location, name, module)
                        }
                    } else if ignore_no_def {
                        fallback
                    } else {
                        unknown_function!(location, name, module)
                    }
                };

                if let Some(unaliased_name) = tmp_function.unaliased {
                    name = unaliased_name;
                };

                if !tmp_function.usable && !func.borrow_mut().imported && !ignore_no_def {
                    panic!(
                        "{}",
                        location.error(format!(
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

                    if let Some(first_arg) = first_arg {
                        if i == 0
                            && type_method
                            && should_get_address
                            && first_param.is_some()
                            && first_arg.0.is_pointer()
                            && first_arg.0.get_pointer_inner().unwrap()
                                == first_param.clone().unwrap().0
                        {
                            parameter.1 = AstNode::AddressStatement {
                                value: Box::new(parameter.1),
                                location: location.clone(),
                            }
                        }
                    }

                    let (ty, val) = if i == 0 && first_param.is_some() && !should_get_address {
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
                            false,
                        )
                    });
                }

                let ty = tmp_function.return_type.unwrap_or(declarative_ty);

                if add_meta {
                    let res = self
                        .generate_statement(
                            func,
                            module,
                            Compiler::generate_meta_struct(
                                func,
                                &params,
                                parameters,
                                location.clone(),
                            ),
                            Some(ty.clone()),
                            None,
                            false,
                        )
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
                            AstNode::LiteralStatement {
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

                    params.insert(tmp_function.variadic_index, res);
                }

                if !tmp_function.variadic {
                    let only = if tmp_function.arguments.len() > params.len() {
                        "only "
                    } else {
                        ""
                    };

                    if tmp_function.arguments.len() != params.len() {
                        panic!(
                            "{}",
                            location.error(format!(
                                "Function named '{}' takes {} arguments, but you {}passed {}",
                                tmp_function.name,
                                tmp_function.arguments.len() - add_meta as usize,
                                only,
                                params.len() - add_meta as usize
                            ))
                        )
                    }
                }

                let temp =
                    self.new_temporary(Some(&format!("{}.res", func.borrow_mut().name)), true);

                let (_, val) = self
                    .get_variable(&name, Some(func))
                    .unwrap_or((Type::Long, Value::Global(name)));

                func.borrow_mut()
                    .assign_instruction(&temp, &ty, Instruction::Call(val, params));

                Some((ty, temp))
            }
            AstNode::BufferStatement {
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
                            AstNode::ArithmeticOperation {
                                left: size,
                                right: Box::new(AstNode::LiteralStatement {
                                    kind: TokenKind::LongLiteral,
                                    value: ValueKind::Number(ty.size(module) as i128),
                                    location: location.clone(),
                                }),
                                operator: TokenKind::Multiply,
                                location: location.clone(),
                            }
                        } else {
                            AstNode::LiteralStatement {
                                kind: TokenKind::LongLiteral,
                                value: ValueKind::Number(0),
                                location: location.clone(),
                            }
                        },
                        r#type,
                        None,
                        false,
                    )
                    .expect(&location.error(format!(
                        "Unexpected error when trying to compile size for a buffer named '{}'",
                        name
                    )));

                let tmp = self
                    .new_variable(&buf_ty, &name, Some(func), true, false)
                    .expect(&location.error(format!(
                        "Unexpected error when trying to create a variable named '{}'",
                        name
                    )));

                let (_, converted_val) =
                    self.convert_to_type(func, ty, Type::Long, val, &location, false);

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
            AstNode::MemoryStatement {
                left,
                right,
                value,
                left_location,
                right_location,
                value_location,
            } => {
                let (left_ty, _) = self
                    .generate_statement(func, module, *left.clone(), ty.clone(), None, false)
                    .expect(&left_location.error(format!(
                        "Unexpected error when trying to compile the left side of a {} statement",
                        if value.is_some() { "store" } else { "load" }
                    )));

                let (right_ty, _) = self
                    .generate_statement(func, module, *right.clone(), ty.clone(), None, false)
                    .expect(&right_location.error(format!(
                        "Unexpected error when trying to compile the right side of a {} statement",
                        if value.is_some() { "store" } else { "load" }
                    )));

                if !(matches!(left_ty, Type::Pointer(_)) || matches!(right_ty, Type::Pointer(_))) {
                    panic!(
                        "{}",
                        left_location.error(format!(
                            "Cannot {} data {} non-pointer types ({:?} and {:?})",
                            if value.is_some() { "store" } else { "load" },
                            if value.is_some() { "to" } else { "from" },
                            left_ty,
                            right_ty
                        ))
                    );
                }

                let inner = if left_ty.is_pointer() {
                    left_ty.get_pointer_inner().unwrap()
                } else {
                    right_ty.get_pointer_inner().unwrap()
                };

                let node = AstNode::ArithmeticOperation {
                    left: if left_ty.is_pointer() {
                        left.clone()
                    } else {
                        right.clone()
                    },
                    right: Box::new(AstNode::ArithmeticOperation {
                        left: Box::new(AstNode::LiteralStatement {
                            kind: TokenKind::LongLiteral,
                            value: ValueKind::Number(inner.size(module) as i128),
                            location: right_location.clone(),
                        }),
                        right: if left_ty.is_pointer() { right } else { left },
                        operator: TokenKind::Multiply,
                        location: right_location.clone(),
                    }),
                    operator: TokenKind::Add,
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
                        AstNode::LiteralStatement {
                            kind,
                            value: literal_value,
                            location,
                        } => match kind {
                            TokenKind::ExactLiteral => {
                                match self.generate_statement(
                                    func,
                                    module,
                                    statement.clone(),
                                    None,
                                    None,
                                    false,
                                ) {
                                    Some((_, value)) => match literal_value {
                                        ValueKind::String(val) => match val.as_str() {
                                            "__MANUAL_RETURN__" => {
                                                func.borrow_mut().manual = true;
                                            }
                                            _ => func
                                                .borrow_mut()
                                                .add_instruction(Instruction::Literal(value)),
                                        },
                                        _ => {
                                            panic!("{}", location.error("Unexpected error"))
                                        }
                                    },
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
                            AstNode::LiteralStatement { kind, .. } => match kind {
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
            AstNode::WhileLoop {
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
                    self.generate_statement(func, module, *step, ty, None, false)
                        .expect(&location.error(
                            "Unexpected error when trying to compile the step of a while loop",
                        ));
                }

                func.borrow_mut()
                    .add_instruction(Instruction::Jump(cond_label.clone()));

                func.borrow_mut().add_block(body_label.clone());

                for statement in body.iter() {
                    match statement {
                        AstNode::LiteralStatement { kind, .. } => match kind {
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
            AstNode::VariadicStatement {
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
                    self.convert_to_type(func, ty, Type::Long, size, &location, false);

                let var = self
                    .new_variable(&Type::Long, &name, Some(func), false, false)
                    .expect(&location.error(format!(
                        "Unexpected error when trying to create a new variable named '{}'",
                        name
                    )));

                func.borrow_mut().assign_instruction(
                    &var,
                    &Type::Long,
                    Instruction::Alloc8(final_val),
                );

                func.borrow_mut().add_instruction(Instruction::VAStart(var));
                None
            }
            AstNode::NextStatement {
                name,
                r#type,
                location,
            } => {
                let ptr = self
                    .get_variable(&name, Some(func))
                    .expect(&location.error(format!(
                        "Unexpected error when trying to get a variable named '{}'",
                        name
                    )))
                    .1;

                let ty = r#type.unwrap_or(Type::Long);
                let tmp = self.new_temporary(Some("next"), true);

                func.borrow_mut()
                    .assign_instruction(&tmp, &ty, Instruction::VAArg(ptr));

                Some((ty, tmp))
            }
            AstNode::BlockStatement { body, location: _ } => {
                self.scopes.push(hashmap!());
                self.tmp_counter += 1;

                let body_label = format!("block.start.{}", self.tmp_counter);
                let end_label = format!("block.end.{}", self.tmp_counter);
                func.borrow_mut().add_block(body_label.clone());

                for statement in body.iter() {
                    match statement {
                        AstNode::LiteralStatement { kind, .. } => match kind {
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
            AstNode::ConversionStatement {
                r#type: second,
                value,
                location,
            } => {
                let (first, val) = self
                    .generate_statement(func, module, *value, ty, None, false)
                    .expect(&location.error("Unexpected error when trying to compile the value of a conversion statement"));

                Some(self.convert_to_type(func, first, second.unwrap(), val, &location, true))
            }
            AstNode::NotStatement { value, location } => {
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
                        Value::Const(ty.clone(), 0),
                    ),
                );

                Some((ty, temp))
            }
            AstNode::ArrayStatement {
                size,
                values,
                location,
            } => {
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

                let inner_ty = if let Some(ty) = ty {
                    ty.get_pointer_inner()
                } else {
                    None
                };

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

                let buf_ty = Type::Pointer(Box::new(first_type.clone().unwrap()));
                let (ty, val) = self
                    .generate_statement(
                        func,
                        module,
                        if let Some(ref ty) = first_type {
                            AstNode::ArithmeticOperation {
                                left: Box::new(AstNode::LiteralStatement {
                                    kind: TokenKind::LongLiteral,
                                    value: ValueKind::Number(ty.size(module) as i128),
                                    location: location.clone(),
                                }),
                                right: size,
                                operator: TokenKind::Multiply,
                                location: location.clone(),
                            }
                        } else {
                            AstNode::LiteralStatement {
                                kind: TokenKind::LongLiteral,
                                value: ValueKind::Number(0),
                                location: location.clone(),
                            }
                        },
                        first_type.clone(),
                        None,
                        false,
                    )
                    .expect(&location.error(format!(
                        "Unexpected error when trying to compile the size of an array"
                    )));

                let tmp = self.new_temporary(Some("array"), true);

                let (_, converted_val) =
                    self.convert_to_type(func, ty, Type::Long, val, &location, false);

                func.borrow_mut().assign_instruction(
                    &tmp,
                    &buf_ty,
                    Instruction::Alloc8(converted_val.clone()),
                );

                self.buf_metadata.insert(
                    value.unwrap_or(tmp.clone()),
                    (buf_ty.get_pointer_inner().unwrap(), converted_val),
                );

                for (i, value) in results.iter().enumerate() {
                    let value_ptr = self.new_temporary(Some("array.offset"), true);

                    func.borrow_mut().assign_instruction(
                        &value_ptr,
                        &Type::Long,
                        Instruction::Add(
                            tmp.clone(),
                            Value::Const(
                                Type::Word,
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

                Some((Type::Pointer(Box::new(buf_ty)), tmp))
            }
            AstNode::AddressStatement { value, location } => {
                let (_, val) = self
                    .generate_statement(func, module, *value, ty, None, false)
                    .expect(&location.error(
                        "Unexpected error when trying to compile the value of an address statement",
                    ));

                let mut parts = val
                    .get_string_inner()
                    .split('.')
                    .collect::<Vec<&str>>()
                    .iter()
                    .map(|item| item.to_string())
                    .collect::<Vec<String>>();

                parts.pop();
                let name = format!("{}.addr", parts.join("."));

                let (ty, val) =
                    self.get_variable(&name, Some(func))
                        .expect(&location.error(format!(
                            "Unexpected error when trying to get a variable named '{}'",
                            name
                        )));

                Some((Type::Pointer(Box::new(ty)), val))
            }
            AstNode::SizeStatement {
                value,
                standalone,
                location,
            } => match value {
                Ok(ty) => {
                    let tmp_ty = Type::Long;
                    let temp = self.new_temporary(Some("size"), true);

                    func.borrow_mut().assign_instruction(
                        &temp,
                        &tmp_ty,
                        Instruction::Copy(Value::Const(tmp_ty.clone(), ty.size(module) as i128)),
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

                            if let Some((buf_ty, buf_val)) =
                                self.buf_metadata.get(&val).map(|item| item.to_owned())
                            {
                                func.borrow_mut().assign_instruction(
                                    &size,
                                    &ty,
                                    if standalone {
                                        Instruction::Copy(buf_val)
                                    } else {
                                        Instruction::Divide(
                                            buf_val,
                                            Value::Const(Type::Long, buf_ty.size(module) as i128),
                                        )
                                    },
                                );

                                return Some((ty, size));
                            }

                            if !standalone {
                                panic!(
                                    "{}",
                                    location.error(
                                        format!(
                                            "Cannot find the length of an array '{}' that isn't defined in the current function",
                                            val.get_string_inner()
                                        )
                                    )
                                );
                            }

                            func.borrow_mut().assign_instruction(
                                &size,
                                &ty,
                                Instruction::Copy(Value::Const(
                                    Type::Long,
                                    ty.size(module) as i128,
                                )),
                            );

                            Some((ty, size))
                        }
                        other => {
                            func.borrow_mut().assign_instruction(
                                &size,
                                &other,
                                Instruction::Copy(Value::Const(
                                    other.clone(),
                                    ty.size(module) as i128,
                                )),
                            );

                            Some((other.to_owned(), size))
                        }
                    }
                }
            },
            AstNode::StructStatement {
                name,
                values,
                location,
            } => {
                if !self.struct_pool.get(&name).is_some() {
                    panic!(
                        "{}",
                        location.error(format!(
                            "Could not find struct named '{}'. Did you spell it correctly?",
                            name
                        ))
                    )
                }

                let mdl = module.borrow();
                let td = mdl
                    .types
                    .iter()
                    .find(|td| td.name == name)
                    .expect(&format!("Unable to find struct named '{}'", name));

                if !td.usable && !func.borrow_mut().imported {
                    panic!(
                        "{}",
                        location.error(format!(
                            "Struct named '{}' was not imported and can't be used",
                            name
                        ))
                    )
                }

                let struct_pool = self.struct_pool.clone();
                let members = struct_pool.get(&name).unwrap();
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
                                name, member
                            ))
                        );
                    }
                }

                let ty = Type::Struct(name.clone());
                let size = ty.size(module);

                let alloc_tmp = self.new_temporary(Some("struct"), true);

                #[cfg(debug_assertions)]
                func.borrow_mut()
                    .add_instruction(Instruction::Comment(format!("size of :{}", name)));

                func.borrow_mut().assign_instruction(
                    &alloc_tmp,
                    &Type::Long,
                    Instruction::Alloc8(Value::Const(Type::Word, size as i128)),
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

                    let (_, offset) = self.member_to_offset(module, &name, &member_name).unwrap();

                    let (ty, val) = self
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

                    let offset_tmp = self.new_temporary(Some("offset"), true);

                    func.borrow_mut().assign_instruction(
                        &offset_tmp,
                        &Type::Long,
                        Instruction::Add(
                            alloc_tmp.clone(),
                            Value::Const(Type::Word, offset as i128),
                        ),
                    );

                    func.borrow_mut()
                        .add_instruction(Instruction::Store(ty, offset_tmp, val))
                }

                Some((ty, alloc_tmp))
            }
            AstNode::FieldStatement {
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

                func.borrow_mut().assign_instruction(
                    &temp,
                    &field_ty,
                    Instruction::Load(field_ty.clone(), offset_tmp),
                );

                Some((field_ty.into_base(), temp))
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
        members: Vec<Argument>,
    ) -> GeneratorResult<TypeDef> {
        let mut items = vec![];

        for member in members.iter().cloned() {
            items.push((member.r#type, 1));
        }

        self.struct_pool.insert(name.clone(), members);

        Ok(TypeDef {
            name,
            align: None,
            items,
            public,
            usable,
            imported,
        })
    }

    fn generate_meta_struct(
        func: &RefCell<Function>,
        params: &Vec<(Type, Value)>,
        parameters: Vec<(Location, AstNode)>,
        location: Location,
    ) -> AstNode {
        AstNode::StructStatement {
            name: META_STRUCT_NAME.into(),
            values: vec![
                (
                    "exprs".into(),
                    Box::new(AstNode::ArrayStatement {
                        size: Box::new(AstNode::LiteralStatement {
                            kind: TokenKind::IntegerLiteral,
                            value: ValueKind::Number(params.len() as i128),
                            location: location.clone(),
                        }),
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
                                    AstNode::LiteralStatement {
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
                    }),
                ),
                (
                    "types".into(),
                    Box::new(AstNode::ArrayStatement {
                        size: Box::new(AstNode::LiteralStatement {
                            kind: TokenKind::IntegerLiteral,
                            value: ValueKind::Number(params.len() as i128),
                            location: location.clone(),
                        }),
                        values: params
                            .iter()
                            .map(|param| {
                                let inner = param.0.id();

                                (
                                    location.clone(),
                                    AstNode::LiteralStatement {
                                        kind: TokenKind::StringLiteral,
                                        value: ValueKind::String(inner),
                                        location: location.clone(),
                                    },
                                )
                            })
                            .collect(),
                        location: location.clone(),
                    }),
                ),
                (
                    "arity".into(),
                    Box::new(AstNode::LiteralStatement {
                        kind: TokenKind::IntegerLiteral,
                        value: ValueKind::Number(params.len() as i128),
                        location: location.clone(),
                    }),
                ),
                (
                    "caller".into(),
                    Box::new(AstNode::LiteralStatement {
                        kind: TokenKind::StringLiteral,
                        value: ValueKind::String(func.borrow_mut().name.clone()),
                        location: location.clone(),
                    }),
                ),
            ],
            location,
        }
    }

    fn member_to_offset(
        &self,
        module: &RefCell<Module>,
        struct_name: &String,
        member_name: &String,
    ) -> Option<(Option<Type>, u64)> {
        match self.struct_pool.get(struct_name) {
            Some(members) => {
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
                AstNode::LiteralStatement {
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
                            struct_name,
                            field,
                            location
                        ));

                    let offset_tmp = self.new_temporary(Some("offset"), true);

                    func.borrow_mut().assign_instruction(
                        &offset_tmp,
                        &Type::Long,
                        Instruction::Add(left, Value::Const(Type::Word, offset as i128)),
                    );

                    if load {
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
                AstNode::FieldStatement {
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
            Instruction::Copy(Value::Const(left_ty.clone(), 0)),
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
                Value::Const(Type::Word, 0),
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
                Value::Const(Type::Word, 0),
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
        location: &Location,
        explicit: bool,
    ) -> (Type, Value) {
        if first.is_struct() || second.is_struct() {
            if first == second {
                return (second, val);
            }

            if (first.is_struct() && second.is_pointer_like())
                || (second.is_struct() && first.is_pointer_like())
            {
                return (if first.is_struct() { first } else { second }, val);
            }

            panic!(
                "{}",
                location.error(format!(
                    "Cannot convert to or from a struct type (trying to convert '{}' to '{}')",
                    first.display(),
                    second.display()
                ))
            )
        }

        if (first.is_pointer() && first.get_pointer_inner().unwrap().is_void())
            || (second.is_pointer() && second.get_pointer_inner().unwrap().is_void())
        {
            return (first, val);
        }

        if first.weight() == second.weight() {
            return (second, val);
        } else if (first.is_int() && second.is_int()) || (first.is_float() && second.is_float()) {
            cast_warning!(
                explicit,
                location,
                first,
                second,
                self.warnings,
                Warning::ImplicitCast
            );

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
            cast_warning!(
                explicit,
                location,
                first,
                second,
                self.warnings,
                Warning::ImplicitCast
            );

            let conv = self.new_temporary(Some("conv"), true);

            func.borrow_mut().assign_instruction(
                &conv,
                &second,
                Instruction::Conversion(first, second.clone(), val),
            );

            return (second, conv);
        }
    }

    pub fn compile(tree: Vec<Primitive>, output_path: String, warnings: Warnings) {
        let mut generator = Compiler {
            tmp_counter: 0,
            scopes: vec![],
            data_sections: vec![],
            type_sections: vec![],
            struct_pool: hashmap!(),
            loop_labels: vec![],
            ret_types: hashmap!(),
            buf_metadata: hashmap!(),
            warnings,
            tree,
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
            match primitive {
                Primitive::Constant {
                    name,
                    public,
                    r#type: ty,
                    value,
                    usable,
                    imported,
                    location,
                } => match generator.generate_function(
                    name.clone(),
                    public,
                    false,
                    false,
                    false,
                    false,
                    false,
                    None,
                    usable,
                    imported,
                    &vec![],
                    ty,
                    vec![AstNode::ReturnStatement {
                        value,
                        location: location.clone(),
                    }],
                    &module_ref,
                    location,
                ) {
                    Ok(function) => {
                        module_ref.borrow_mut().add_function(function);
                    }
                    Err(msg) => eprintln!("{}", msg),
                },
                Primitive::Function {
                    name,
                    public,
                    variadic,
                    manual,
                    external,
                    builtin,
                    volatile,
                    unaliased,
                    arguments,
                    r#return,
                    body,
                    usable,
                    location,
                    imported,
                } => match generator.generate_function(
                    name,
                    public,
                    variadic,
                    manual,
                    external,
                    builtin,
                    volatile,
                    unaliased,
                    usable,
                    imported,
                    &arguments,
                    r#return,
                    body,
                    &module_ref,
                    location,
                ) {
                    Ok(function) => {
                        module_ref.borrow_mut().add_function(function);
                    }
                    Err(msg) => eprintln!("{}", msg),
                },
                Primitive::Struct {
                    name,
                    public,
                    usable,
                    imported,
                    members,
                    ..
                } => match generator.generate_struct(name, public, usable, imported, members) {
                    Ok(td) => {
                        module_ref.borrow_mut().add_type(td);
                    }
                    Err(msg) => eprintln!("{}", msg),
                },
                _ => {}
            }
        }

        for data in generator.data_sections {
            module_ref.borrow_mut().add_data(data);
        }

        for def in generator.type_sections {
            module_ref.borrow_mut().add_type(def);
        }

        module_ref.borrow_mut().remove_unused_functions();
        module_ref.borrow_mut().remove_unused_data();

        let mut file = File::create(output_path).expect("Failed to create the file.");
        file.write_all(module_ref.borrow().to_string().as_bytes())
            .expect(&format!("{RED}Failed to write to file."));

        file.flush().expect("Failed to flush file");
    }
}
