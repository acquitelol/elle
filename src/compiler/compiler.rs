use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fs::File,
    io::Write,
    rc::Rc,
};

use crate::{
    elle_error, get_MAIN_ID, hashmap,
    lexer::enums::Location,
    misc::colors::*,
    parser::{
        enums::{
            modify_type_in_ast, Argument, AstNode, ConstantSource, FunctionSource, Primitive,
            Return,
        },
        parser::StructPool,
    },
    Warnings, GENERIC_END, GENERIC_IDENTIFIER, MAIN_ID, META_STRUCT_NAME,
};

use super::{
    enums::{Data, Function, Instruction, Module, Type, Value},
    lib::can_convert::can_convert_to_type,
    primitive::{function::generate_function, r#struct::generate_struct},
};

#[derive(Clone)]
pub struct CodegenContext<'a> {
    pub func: &'a RefCell<Function>,
    pub module: &'a RefCell<Module>,
    pub ty: Option<Type>,
    pub value: Option<Value>,
    pub is_return: bool,
}

impl CodegenContext<'_> {
    /// nnf = None, None, false
    ///
    /// ty -> None
    /// value -> None
    /// is_return -> false
    ///
    /// returns a new struct
    pub fn to_nnf(&self) -> Self {
        return CodegenContext {
            ty: None,
            value: None,
            is_return: false,
            ..self.clone()
        };
    }
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
    pub struct_pool: StructPool,
    pub loop_labels: Vec<String>,
    // ret_types: HashMap<String, Type>,
    pub buf_metadata: HashMap<Value, (Type, Value)>,
    tree: Vec<Primitive>,
    pub warnings: Warnings,
    // lambda functions that should be added as soon as possible
    pub deferred_functions: Vec<Function>,
    // Map from temporary to its stack allocated address
    pub address_pool: HashMap<Value, Value>,
    pub output_path: String,
    pub pedantic: bool,
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

    pub fn new_manual_argument(&mut self, ty: &Type, name: &str) -> Value {
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
                    Primitive::Constant(ConstantSource {
                        name: const_name,
                        r#type: ty,
                        location,
                        usable,
                        ..
                    }) => {
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
                    Primitive::Function(FunctionSource {
                        name: op_name,
                        usable,
                        location,
                        builtin,
                        ..
                    }) => {
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
                Primitive::Function(FunctionSource { unaliased, .. }) => {
                    if unaliased.is_none() {
                        break;
                    }

                    *name = unaliased.clone().unwrap_or(name.to_string());
                }
                _ => {}
            };
        }

        match &self.generic_functions.get(&name.clone()).unwrap().clone() {
            Primitive::Function(this) => {
                // Reassign it if the function is generic
                // as the function won't have been found last time
                if let Some(inner) = this.arguments.get(0) {
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
                    known_generics.extend(HashMap::<String, Type>::from_iter(
                        base_known_generics
                            .iter()
                            .enumerate()
                            .map(|(i, known)| (this.generics[i].clone(), known.clone()))
                            .collect::<Vec<(String, Type)>>(),
                    ));
                }

                for (i, parameter) in parameters.iter().cloned().enumerate() {
                    let param_ty = {
                        let tmp = this.arguments.get(i + *add_meta as usize);

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

                    let (ty, _) = parameter.1.compile(
                            self,
                            &CodegenContext {
                                func: &RefCell::new(tmp_func),
                                module,
                                ty: param_ty.clone(),
                                value: None,
                                is_return: false
                            }
                        )
                        .expect(&parameter.0.error(
                            format!(
                                "Unexpected error when trying to generate a statement for a parameter in a function called '{}'",
                                name
                            ))
                        );

                    let other = {
                        let tmp = this.arguments.get(i + *add_meta as usize);

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
                                        if !can_convert_to_type(
                                            self,
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
                                            this.location.ctx,
                                            GREEN = get_GREEN!(),
                                            BOLD = get_BOLD!(),
                                            RESET = get_RESET!()
                                        )));

                                        elle_error!(
                                            call_location.with_extra_info(format!("{key} = `{}`, but got `{}`", existing_ty.display(), ty.display())).error(
                                                format!(
                                                    "Mismatched type for generic {key} in {}<{}>({}):\n{key} is defined with both type \"{GREEN}{}{RESET}\" and \"{RED}{}{RESET}\"",
                                                    name.replace(".", "::"),
                                                    this.generics.join(", "),
                                                    if this.arguments.len() > 0 { "..." } else { "" },
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
                                this.location.warning(format!(
                                    "Failed to deduce a generic type from {} and {}",
                                    ty.display(),
                                    other.display()
                                ),)
                            )
                        }
                    }
                }

                if let Some(other) = this.r#return.clone() {
                    if let Some(ty) = ty {
                        if ty.clone().has_generic_type(other.clone())
                            && known_generics.len() < this.generics.len()
                        {
                            // Possibly Option.generic.8 and Option
                            if let Some(inner) = ty.clone().deduce_generic_type(other.clone()) {
                                known_generics.extend(inner)
                            } else if other.is_unknown()
                                && other.get_unknown_inner().unwrap() == "fn"
                            {
                                println!(
                                    "{}",
                                    this.location.warning(format!(
                                        "Failed to deduce a generic type from {} and {}",
                                        ty.display(),
                                        other.display()
                                    ))
                                )
                            }
                        }
                    }

                    if let Some(ty) = func.borrow().return_type.clone() {
                        if ty.clone().has_generic_type(other.clone())
                            && known_generics.len() < this.generics.len()
                        {
                            // Possibly Option.generic.8 and Option
                            if let Some(inner) = ty.clone().deduce_generic_type(other.clone()) {
                                known_generics.extend(inner)
                            } else if other.is_unknown()
                                && other.get_unknown_inner().unwrap() == "fn"
                            {
                                println!(
                                    "{}",
                                    this.location.warning(format!(
                                        "Failed to deduce a generic type from {} and {}",
                                        ty.display(),
                                        other.display()
                                    ),)
                                )
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
                        this.location.ctx,
                        GREEN = get_GREEN!(),
                        BOLD = get_BOLD!(),
                        RESET = get_RESET!()
                    )));

                    elle_error!(
                        call_location.error(format!(
                            "Mismatched number of generics in function {}<{}>({}).\nCould not find generic{} {} where the function specifies <{}>.",
                            name.replace(".", "::"),
                            this.generics.join(", "),
                            if this.arguments.len() > 0 { "..." } else { "" },
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

                    let parsed_arguments = &this
                        .arguments
                        .iter()
                        .cloned()
                        .map(|arg| Argument {
                            name: arg.name,
                            r#type: arg.r#type.unknown_to_known(
                                Some(&struct_pool),
                                Some(&tree),
                                this.generics.clone(),
                                known_generics.clone(),
                            ),
                            manual: arg.manual,
                            no_fmt: arg.no_fmt,
                        })
                        .collect::<Vec<Argument>>();

                    let parsed_return = if this.r#return.is_some() {
                        Some(this.r#return.clone().unwrap().unknown_to_known(
                            Some(&struct_pool),
                            Some(&tree),
                            this.generics.clone(),
                            known_generics.clone(),
                        ))
                    } else {
                        this.r#return.clone()
                    };

                    let parsed_body = modify_type_in_ast(
                        this.body.clone(),
                        &this.generics,
                        &known_generics,
                        Some(&struct_pool),
                        Some(&tree),
                    );

                    self.struct_pool = struct_pool.borrow().to_owned();

                    for primitive in tree.borrow().to_owned().into_iter() {
                        match primitive {
                            Primitive::Struct(this) => {
                                let td = generate_struct(this, self);
                                module.borrow_mut().add_type(td);
                            }
                            _ => {}
                        };
                    }

                    let function = generate_function(
                        FunctionSource {
                            name: generic_name,
                            generics: vec![],
                            arguments: parsed_arguments.clone(),
                            r#return: parsed_return,
                            body: parsed_body,
                            ..this.clone()
                        },
                        self,
                        false,
                        known_generics.clone(),
                        &module,
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
        let mut gen = Compiler {
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

        if gen
            .tree
            .iter()
            .find(|primitive| match primitive {
                Primitive::Function(FunctionSource { name, .. })
                    if &(name.to_owned()) == get_MAIN_ID!() =>
                {
                    true
                }
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

        for primitive in gen.tree.clone() {
            match primitive.clone() {
                Primitive::Constant(this) => {
                    let function = generate_function(
                        FunctionSource {
                            name: this.name.clone(),
                            public: this.public,
                            variadic: false,
                            manual: false,
                            external: false,
                            builtin: false,
                            volatile: false,
                            format: false,
                            unaliased: None,
                            usable: this.usable,
                            imported: this.imported,
                            generics: vec![],
                            arguments: vec![],
                            r#return: this.r#type,
                            body: vec![AstNode::Return(Return {
                                value: this.value,
                                location: this.location.clone(),
                            })],
                            location: this.location.clone(),
                            return_location: this.location,
                        },
                        &mut gen,
                        false,
                        hashmap![],
                        &module_ref,
                    );

                    module_ref.borrow_mut().add_function(function);
                }
                Primitive::Function(this) => {
                    if this.generics.is_empty() {
                        let function =
                            generate_function(this, &mut gen, false, hashmap![], &module_ref);

                        module_ref.borrow_mut().add_function(function);

                        for func in gen.deferred_functions.clone() {
                            module_ref.borrow_mut().add_function(func);
                        }

                        gen.deferred_functions.clear();
                    } else {
                        gen.generic_functions.insert(this.name, primitive);
                    }
                }
                Primitive::Struct(this) => {
                    let td = generate_struct(this, &mut gen);

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

        for data in gen.data_sections {
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
