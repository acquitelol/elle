use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fs::File,
    io::Write,
    rc::Rc,
};

use crate::{
    advance, elle_error, get_MAIN_ID, get_POINTER_ID, hashmap, is_generic,
    lexer::enums::{Location, TokenKind, ValueKind},
    misc::colors::*,
    parser::{
        enums::{
            modify_type_in_ast, Argument, AstNode, BinaryOperation, FunctionCall, Literal,
            Primitive, Return,
        },
        parser::StructPool,
    },
    unknown_field, unknown_function, Warning, Warnings, ARBITRARY_ALLOCATOR_NAME, DUNDER_CONSTANTS,
    ENV_ID, ENV_STRUCT_NAME, EQUALS_CONSTANT, FORMAT_CONSTANT, GC_NOOP, GENERIC_END,
    GENERIC_IDENTIFIER, LOAD_CONSTANT, MAIN_ID, META_STRUCT_NAME, POINTER_ID,
    PTR_PRIORITY_CONSTANTS, STORE_CONSTANT, VA_LIST_SIZE_BYTES, VOID_POINTER_ID,
};

use super::enums::{
    Comparison, Data, DataItem, Function, Instruction, Linkage, Module, Statement, Type, TypeDef,
    Value,
};

pub struct CodegenContext<'a> {
    pub func: &'a RefCell<Function>,
    pub module: &'a RefCell<Module>,
    pub stmt: AstNode,
    pub ty: Option<Type>,
    pub value: Option<Value>,
    pub is_return: bool,
}

pub trait Codegen<'a> {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'a>) -> Option<(Type, Value)>;
}

pub struct Compiler {
    pub tmp_counter: u32,
    pub scopes: Vec<HashMap<String, (Type, Value)>>,
    pub data_sections: Vec<Data>,
    pub generic_functions: HashMap<String, Primitive>,
    // Struct Name => ((Field Name, Field Type)[], (Known Generic)[])
    struct_pool: StructPool,
    pub loop_labels: Vec<String>,
    // ret_types: HashMap<String, Type>,
    pub buf_metadata: HashMap<Value, (Type, Value)>,
    tree: Vec<Primitive>,
    warnings: Warnings,
    // lambda functions that should be added as soon as possible
    deferred_functions: Vec<Function>,
    // Map from temporary to its stack allocated address
    pub address_pool: HashMap<Value, Value>,
    output_path: String,
    pedantic: bool,
    pub no_gc: bool,
}

impl Compiler {
    pub fn tmp_name_with_debug_assertions(&self, name: &str, minify: bool) -> String {
        if cfg!(debug_assertions) || !minify {
            format!("{}.{}", name, self.tmp_counter)
        } else {
            format!(".{}", self.tmp_counter)
        }
    }

    pub fn new_temporary(&mut self, name: Option<&str>, _minify: bool) -> Value {
        self.tmp_counter += 1;
        Value::Temporary(self.tmp_name_with_debug_assertions(name.unwrap_or("tmp"), false))
    }

    pub fn new_variable(
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

    pub fn get_variable(
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
                                elle_error!(location.error(format!(
                                    "Constant named '{}' was not imported and can't be used",
                                    name
                                )))
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
                                elle_error!(location.error(format!(
                                    "Function named '{}' was not imported and can't be used",
                                    name.replace(".", "::")
                                )))
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

    pub fn get_variable_lazy(
        &mut self,
        name: &String,
        func: Option<&RefCell<Function>>,
        module: Option<&RefCell<Module>>,
        location: Rc<Location>,
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
                        elle_error!(location.error(format!(
                            "Unexpected error when trying to get a variable called '{}': {}",
                            name, msg
                        )))
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
        location: Rc<Location>,
        return_location: Rc<Location>,
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

            args.push(((ty.into_abi(), tmp), argument.no_fmt));
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
                AstNode::Literal(Literal { kind, .. }) => match kind {
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
                                    let (a, a_parts) = Type::from_internal_id(
                                        return_type.get_struct_inner().unwrap(),
                                    );

                                    let (b, b_parts) = Type::from_internal_id(
                                        first_type.get_struct_inner().unwrap(),
                                    );

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

    pub fn generate_statement(
        &mut self,
        func: &RefCell<Function>,
        module: &RefCell<Module>,
        stmt: AstNode,
        ty: Option<Type>,
        value: Option<Value>,
        is_return: bool,
    ) -> Option<(Type, Value)> {
        // TODO: Unclone these when the whole codegen is moved to the Codegen trait
        let ctx = CodegenContext {
            func,
            module,
            stmt: stmt.clone(),
            ty: ty.clone(),
            value: value.clone(),
            is_return,
        };

        let res = match stmt {
            AstNode::Declare(this) => this.compile(self, &ctx),
            AstNode::Return(this) => this.compile(self, &ctx),
            AstNode::BinaryOperation(this) => this.compile(self, &ctx),
            AstNode::Literal(this) => this.compile(self, &ctx),
            AstNode::FunctionCall(this) => this.compile(self, &ctx),
            AstNode::Buffer(this) => this.compile(self, &ctx),
            AstNode::MemoryOperation(this) => this.compile(self, &ctx),
            AstNode::IfStatement(this) => this.compile(self, &ctx),
            AstNode::WhileLoopStatement(this) => this.compile(self, &ctx),
            AstNode::VariadicStart { name, .. } => {
                let var = self.new_variable(&Type::Long, &name, Some(func), false, false);

                func.borrow_mut().assign_instruction(
                    &var,
                    &Type::Long,
                    Instruction::Alloc8(Value::Const("".into(), VA_LIST_SIZE_BYTES as i128)),
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
            AstNode::SetAllocator { value, location } => {
                let mut tmp_func = Function::default();
                tmp_func.add_block("start");

                let (ty, _) = self
                    .generate_statement(
                        &RefCell::new(tmp_func),
                        module,
                        *value.clone(),
                        None,
                        None,
                        is_return,
                    )
                    .expect(
                        &location.error("Unexpected error when compiling allocator expresssion"),
                    );

                if !ty.is_struct()
                    && !(ty.is_pointer() && ty.get_pointer_inner().unwrap().is_struct())
                {
                    elle_error!(location
                        .with_extra_info(format!("This has the type {}", ty.display()))
                        .error("Cannot set an allocator to a non-allocator expression"))
                }

                let allocator_name = if ty.is_struct() {
                    ty.get_struct_inner().unwrap()
                } else {
                    ty.get_pointer_inner().unwrap().get_struct_inner().unwrap()
                };

                macro_rules! method_or_noop {
                    ($name:literal) => {{
                        let method_name = format!("{allocator_name}.{}", $name);

                        AstNode::Literal(Literal {
                            kind: TokenKind::Identifier,
                            value: ValueKind::String(if module.borrow().functions.iter().find(|f| f.name == method_name).is_some() {
                                method_name
                            } else {
                                if self.warnings.has_warning(Warning::AllocatorMethodsMissing) {
                                    println!(
                                        "{}",
                                        location.warning(format!(
                                            "The allocator '{GREEN}{}{RESET}' has no method named '{GREEN}{}{RESET}'.\nIt will be set to a function which returns {RED}nil{RESET} instead.",
                                            allocator_name,
                                            method_name.replace(".", "::"),
                                            GREEN = get_GREEN!(),
                                            RED = get_RED!(),
                                            RESET = get_RESET!(),
                                        ))
                                    );
                                }

                                format!("{ARBITRARY_ALLOCATOR_NAME}.noop")
                            }),
                            location: location.clone(),
                        })
                    }};
                }

                let parts = vec![
                    ("inner", *value),
                    (
                        "kind",
                        AstNode::Literal(Literal {
                            kind: TokenKind::StringLiteral,
                            value: ValueKind::String(allocator_name.clone()),
                            location: location.clone(),
                        }),
                    ),
                    ("alloc", method_or_noop!("alloc")),
                    ("realloc", method_or_noop!("realloc")),
                    ("free", method_or_noop!("free")),
                    ("free_self", method_or_noop!("free_self")),
                ];

                for (field, expr) in parts {
                    self.generate_statement(
                        func,
                        module,
                        AstNode::FieldAccess {
                            left: Box::new(AstNode::Environment {
                                value: None,
                                location: location.clone(),
                            }),
                            right: Box::new(AstNode::FieldAccess {
                                left: Box::new(AstNode::Literal(Literal {
                                    kind: TokenKind::Identifier,
                                    value: ValueKind::String("allocator".into()),
                                    location: location.clone(),
                                })),
                                right: Box::new(AstNode::Literal(Literal {
                                    kind: TokenKind::Identifier,
                                    value: ValueKind::String(field.into()),
                                    location: location.clone(),
                                })),
                                value: None,
                                location: location.clone(),
                            }),
                            value: Some(Box::new(expr)),
                            location: location.clone(),
                        },
                        None,
                        None,
                        is_return,
                    )
                    .expect(
                        &location.error(
                            "Unexpected error when compiling a statement to set the allocator.",
                        ),
                    );
                }

                None
            }
            AstNode::BlockStatement { body, location: _ } => {
                self.scopes.push(hashmap!());
                self.tmp_counter += 1;

                let body_label = format!("block.start.{}", self.tmp_counter);
                let end_label = format!("block.end.{}", self.tmp_counter);
                func.borrow_mut().add_block(body_label.clone());

                for statement in body.iter() {
                    match statement {
                        AstNode::Literal(Literal { kind, .. }) => match kind {
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
                explicit,
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
                    explicit,
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
                        AstNode::BinaryOperation(BinaryOperation {
                            left: value,
                            right: Box::new(AstNode::Literal(Literal {
                                kind: TokenKind::IntegerLiteral,
                                value: ValueKind::Number(Type::Word.size(module) as i128),
                                location: location.clone(),
                            })),
                            operator: TokenKind::Subtract,
                            treat_as_string: false,
                            dunder_methods: true,
                            location: location.clone(),
                        }),
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
                explicit_inner,
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
                    let inner_ty = if let Some(ref ty) = explicit_inner {
                        Some(ty.clone())
                    } else if values.len() > 0 {
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

                        Some(ty.clone())
                    } else if !known_generics.is_empty() {
                        Some(known_generics.get(0).unwrap().clone())
                    } else if let Some(ref ty) = ty {
                        Some(ty.clone())
                    // } else if is_return {
                    //     None
                    } else {
                        // panic!(
                        //     "{}",
                        //     location.with_extra_info("Try specifying a type here").error(format!("Could not determine any type for this array.\nPlease specify a type explicitly with the {GREEN}[T;]{RESET} syntax."))
                        // )
                        None
                    };

                    let node = AstNode::FunctionCall(FunctionCall {
                        name: "Array.new".into(),
                        generics: if let Some(ref ty) = inner_ty {
                            vec![ty.clone()]
                        } else {
                            vec![]
                        },
                        parameters: if let Some(ty) = inner_ty {
                            values
                                .into_iter()
                                .map(|(loc, node)| {
                                    (
                                        loc.clone(),
                                        AstNode::Conversion {
                                            r#type: Some(ty.clone()),
                                            value: Box::new(node),
                                            location: loc.clone(),
                                            explicit: false,
                                        },
                                    )
                                })
                                .collect()
                        } else {
                            values
                        },
                        type_method: false,
                        ignore_no_def: false,
                        location: location.clone(),
                    });

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
                    elle_error!(
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
                            elle_error!(location.error(format!(
                                "Inconsistent array types '{}' and '{}' (possibly more)",
                                first_type.display(),
                                ty.display()
                            )));
                        }

                        if inner_ty.is_some() && inner_ty.clone().unwrap() != first_type {
                            elle_error!(location.error(format!(
                                "Invalid type of element in array '{}' when the array type is '{}'",
                                ty.display(),
                                inner_ty.unwrap().display(),
                            )))
                        }
                    } else {
                        if inner_ty.is_some() && inner_ty.clone().unwrap() != ty {
                            elle_error!(location.error(format!(
                                "Invalid type of element in array '{}' when the array type is '{}'",
                                ty.display(),
                                inner_ty.unwrap().display(),
                            )))
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

                if ty.is_struct() {
                    return Some((Type::Pointer(Box::new(ty)), val));
                }

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
                        elle_error!(
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
                    elle_error!(location.error(format!(
                        "Struct named '{}' was not imported and can't be used",
                        Type::Struct(name.clone()).display()
                    )))
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
                        elle_error!(
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
                    self.process_field_access(func, module, ty, left, *right, false, &location);

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
        keyword_location: Rc<Location>,
        _location: Rc<Location>,
    ) -> TypeDef {
        let mut items = vec![];

        if members.is_empty() && !ignore_empty {
            elle_error!(
                keyword_location
                    .with_extra_info("Replace this with 'namespace'")
                    .error(format!(
                        "Cannot declare an empty struct (with no members).\nIf you intended to make a namespace, use the '{GREEN}namespace{RESET}' keyword instead.",
                        GREEN = get_GREEN!(),
                        RESET = get_RESET!()
                    ))
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

    pub fn generate_meta_struct(
        func: &RefCell<Function>,
        params: &Vec<((Type, Value), bool)>,
        parameters: Vec<(Rc<Location>, AstNode)>,
        location: Rc<Location>,
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
                                    AstNode::Literal(Literal {
                                        kind: TokenKind::StringLiteral,
                                        value: ValueKind::String(
                                            res.replace("\\", "\\\\").replace("\"", "\\\""),
                                        ),
                                        location: location.clone(),
                                    }),
                                )
                            })
                            .collect(),
                        location: location.clone(),
                        explicit_inner: None,
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
                                let inner = param.0 .0.id();

                                (
                                    location.clone(),
                                    AstNode::Literal(Literal {
                                        kind: TokenKind::StringLiteral,
                                        value: ValueKind::String(inner),
                                        location: location.clone(),
                                    }),
                                )
                            })
                            .collect(),
                        location: location.clone(),
                        explicit_inner: None,
                        known_generics: vec![],
                        dynamic: false,
                    }),
                ),
                (
                    "arity".into(),
                    Box::new(AstNode::Literal(Literal {
                        kind: TokenKind::IntegerLiteral,
                        value: ValueKind::Number(params.len() as i128),
                        location: location.clone(),
                    })),
                ),
                (
                    "caller".into(),
                    Box::new(AstNode::Literal(Literal {
                        kind: TokenKind::StringLiteral,
                        value: ValueKind::String({
                            let name = func.borrow_mut().name.clone();

                            if name == get_MAIN_ID!() {
                                "main".into()
                            } else {
                                name
                            }
                        }),
                        location: location.clone(),
                    })),
                ),
                (
                    "file".into(),
                    Box::new(AstNode::Literal(Literal {
                        kind: TokenKind::StringLiteral,
                        value: ValueKind::String(
                            location.file.clone().split("/").last().unwrap().to_string(),
                        ),
                        location: location.clone(),
                    })),
                ),
                (
                    "line".into(),
                    Box::new(AstNode::Literal(Literal {
                        kind: TokenKind::IntegerLiteral,
                        value: ValueKind::Number((location.row + 1) as i128),
                        location: location.clone(),
                    })),
                ),
                (
                    "column".into(),
                    Box::new(AstNode::Literal(Literal {
                        kind: TokenKind::IntegerLiteral,
                        value: ValueKind::Number((location.column + 1) as i128),
                        location: location.clone(),
                    })),
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
        location: &Rc<Location>,
    ) -> (Type, Value) {
        loop {
            match right.clone() {
                AstNode::Literal(Literal {
                    kind,
                    value,
                    location,
                }) if kind == TokenKind::Identifier => {
                    let field = value.get_string_inner().unwrap();

                    if !ty.is_struct() {
                        // Automatically deref 'Foo *' into 'Foo' when processing
                        if ty.is_pointer() && ty.get_pointer_inner().unwrap().is_struct() {
                            ty = ty.get_pointer_inner().unwrap();
                        } else {
                            elle_error!(&location.error(format!(
                                "Cannot access fields on a non-struct type '{}' (field '{}')",
                                ty.display(),
                                field
                            )));
                        }
                    }

                    let struct_name = ty.get_struct_inner().unwrap();

                    let (member_ty, offset) = self
                        .member_to_offset(module, &struct_name, &field)
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
                    let (nested_ty, nested_left_value) = self.process_field_access(
                        func,
                        module,
                        ty,
                        left,
                        *nested_left,
                        true,
                        location,
                    );

                    ty = nested_ty;
                    left = nested_left_value;
                    right = *nested_right;
                }
                _ => elle_error!(location.error(format!(
                    "Unexpected AST node type for field access: {:?}",
                    right
                ))),
            }
        }
    }

    pub fn handle_short_circuiting_operation(
        &mut self,
        left: Box<AstNode>,
        right: Box<AstNode>,
        func: &RefCell<Function>,
        module: &RefCell<Module>,
        ty: Option<Type>,
        is_return: bool,
        location: Rc<Location>,
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
            other => elle_error!(location.error(format!(
                "Invalid operator token for conditional short circuiting '{}'",
                other
            ))),
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

    pub fn convert_to_type(
        &mut self,
        func: &RefCell<Function>,
        first: Type,
        second: Type,
        val: Value,
        left_location: &Location,
        right_location: &Location,
        explicit: bool,
    ) -> (Type, Value) {
        // TODO: ADD A VARIANT TO `can_convert_to_type` WHEN ADDING A VARIANT HERE
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
                if second.is_struct() {
                    return (second, val);
                } else {
                    let tmp = self.new_temporary(Some("load"), false);

                    func.borrow_mut().assign_instruction(
                        &tmp,
                        &second.clone(),
                        Instruction::Load(second.clone(), val),
                    );

                    return (second, tmp);
                }
            }

            elle_error!(left_location
                .clone()
                .with_extra_info(format!("This has the type '{}'", first.display()))
                .error(format!(
                    "Cannot convert from the type '{}' to the type '{}'.",
                    first.display(),
                    second.display()
                )))
        }

        macro_rules! implicit_conversion_error {
            () => {
                elle_error!(
                    right_location.clone().with_extra_info(format!(
                        "This has the type '{}'",
                        first.display()
                    )).error(format!(
                        "Cannot implicitly convert '{}' to '{}' or vice versa.\nTo explicitly convert, use the C-like '(type)variable' syntax.",
                        first.display(),
                        second.display()
                    ))
                )
            };
        }

        if ((first.is_strictly_number() && second.is_string())
            || (second.is_strictly_number() && first.is_string()))
            && !explicit
        {
            implicit_conversion_error!()
        }

        if first.is_pointer()
            && second.is_pointer()
            && (first.get_pointer_inner().unwrap().is_void()
                || second.get_pointer_inner().unwrap().is_void())
        {
            return (second, val);
        }

        if ((first.is_pointer() && second.is_pointer())
            && first.get_pointer_inner().unwrap() != second.get_pointer_inner().unwrap())
            && !explicit
            && self.pedantic
        {
            implicit_conversion_error!()
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

    fn can_convert_to_type(&mut self, first: Type, second: Type, explicit: bool) -> bool {
        if first.is_struct() || second.is_struct() {
            let structs_are_the_same = first == second;
            let explicit_struct_to_ptr = explicit
                && ((first.is_struct() && second.is_pointer_like())
                    || (second.is_struct() && first.is_pointer_like()));
            let first_is_ptr_of_second =
                first.is_pointer() && first.get_pointer_inner().unwrap() == second;

            return structs_are_the_same || explicit_struct_to_ptr || first_is_ptr_of_second;
        }

        if ((first.is_strictly_number() && second.is_string())
            || (second.is_strictly_number() && first.is_string()))
            && !explicit
        {
            return false;
        }

        if (first.is_pointer() && second.is_pointer())
            && (first.get_pointer_inner().unwrap().is_void()
                || second.get_pointer_inner().unwrap().is_void())
        {
            return true;
        }

        if ((first.is_pointer() && second.is_pointer())
            && first.get_pointer_inner().unwrap() != second.get_pointer_inner().unwrap())
            && !explicit
            && self.pedantic
        {
            return false;
        }

        let weights_match = first.weight() == second.weight();
        let both_int_or_float =
            (first.is_int() && second.is_int()) || (first.is_float() && second.is_float());

        return weights_match || both_int_or_float;
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
                no_fmt: member.no_fmt,
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
                Rc::new(Location::default(self.output_path.clone())),
            ),
        );
    }

    pub fn create_monomorphized_function(
        &mut self,
        name: &mut String,
        add_meta: &mut bool,
        base_known_generics: Vec<Type>,
        known_generics: &mut HashMap<String, Type>,
        parameters: Vec<(Rc<Location>, AstNode)>,
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

                    if ty.clone().has_generic_type(other.clone()) {
                        // Possibly Option.generic.8 and Option
                        if let Some(inner) = ty.clone().deduce_generic_type(other.clone()) {
                            for (key, ty) in inner.iter().map(|(x, y)| (x.clone(), y.clone())) {
                                match known_generics.get(&key) {
                                    Some(existing_ty)
                                        if !self.can_convert_to_type(
                                            existing_ty.clone(),
                                            ty.clone(),
                                            false,
                                        ) =>
                                    {
                                        call_location.column -= call_location.ctx.len()
                                            - call_location.ctx.trim().len();
                                        call_location.ctx = Rc::from(call_location.ctx.trim());
                                        call_location.above = Some(Rc::from(format!(
                                            "In function:\n{GREEN}{BOLD}{}{}{RESET}\n\n",
                                            " ".repeat(
                                                call_location.ctx.len()
                                                    - call_location.ctx.trim().len()
                                                    + format!("{}", call_location.row + 1).len()
                                                    + 8
                                            ),
                                            location.ctx,
                                            GREEN = get_GREEN!(),
                                            BOLD = get_BOLD!(),
                                            RESET = get_RESET!()
                                        )));

                                        elle_error!(
                                            call_location.with_extra_info(format!("{key} = `{}`, but got `{}`", existing_ty.display(), ty.display())).error(
                                                format!(
                                                    "Mismatched type for generic {key} in {}<{}>({}):\n{key} is defined with both type \"{GREEN}{}{RESET}\" and \"{RED}{}{RESET}\"",
                                                    name.replace(".", "::"),
                                                    generics.join(", "),
                                                    if arguments.len() > 0 { "..." } else { "" },
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
                    call_location.ctx = Rc::from(call_location.ctx.trim());
                    call_location.above = Some(Rc::from(format!(
                        "In function:\n{GREEN}{BOLD}{}{}{RESET}\n\n",
                        " ".repeat(
                            call_location.ctx.len() - call_location.ctx.trim().len()
                                + format!("{}", call_location.row + 1).len()
                                + 8
                        ),
                        location.ctx,
                        GREEN = get_GREEN!(),
                        BOLD = get_BOLD!(),
                        RESET = get_RESET!()
                    )));

                    elle_error!(
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
                            no_fmt: arg.no_fmt,
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
        pedantic: bool,
        no_gc: bool,
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
            pedantic,
            no_gc,
        };

        let module = Module::new();

        // We need internal mutability here
        // Each string data section needs to be added to the module
        let module_ref = RefCell::new(module);

        if generator
            .tree
            .iter()
            .find(|primitive| match primitive {
                Primitive::Function { name, .. } if &(name.to_owned()) == get_MAIN_ID!() => true,
                _ => false,
            })
            .is_none()
            && !object_output
        {
            elle_error!(Location::base().basic_error(format!(
                "Could not compile module \"{MAGENTA}{output_path}{RESET}\":\n{}\n\n{}",
                "Module has no entry-point. To create one, write:",
                format!("{GREEN}+ fn main() {{\n+\n+ }}", GREEN = get_GREEN!()),
                MAGENTA = get_MAGENTA!(),
                RESET = get_RESET!()
            )))
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
                        vec![AstNode::Return(Return {
                            value,
                            location: location.clone(),
                        })],
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

        module_ref
            .borrow_mut()
            .remove_unused_functions(object_output);

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
            .expect(&format!("{RED}Failed to write to file.", RED = get_RED!()));

        file.flush().expect("Failed to flush file");
    }
}
