use std::{
    cell::RefCell, collections::HashMap, fs::File, io::Write
};

use crate::{
    lexer::enums::{TokenKind, ValueKind},
    parser::enums::{Argument, AstNode, Primitive},
};

type GeneratorResult<T> = Result<T, String>;

use super::enums::{
    Comparison, Data, DataItem, Function, Instruction, Linkage, Module, Statement, Type, Value,
};

pub struct Compiler {
    tmp_counter: u32,
    scopes: Vec<HashMap<String, (Type, Value)>>,
    data_sections: Vec<Data>,
    tree: Vec<Primitive>,
}

impl Compiler {
    pub fn get_type(r#type: String) -> Option<Type> {
        match r#type.as_str() {
            "Byte" => Some(Type::Byte),
            "Halfword" => Some(Type::Halfword),
            "Word" => Some(Type::Word),
            "String" => Some(Type::Long),
            "Int" => Some(Type::Word),
            "Long" => Some(Type::Long),
            "Single" => Some(Type::Single),
            "Double" => Some(Type::Double),
            "Char" => Some(Type::Byte),
            "Nil" => None,
            _ => Some(Type::Word),
        }
    }

    fn new_temporary(&mut self, name: Option<&str>) -> Value {
        self.tmp_counter += 1;
        Value::Temporary(format!("{}_{}", name.unwrap_or("tmp"), self.tmp_counter))
    }

    fn new_var(&mut self, ty: &Type, name: &str, new: bool) -> GeneratorResult<Value> {
        let existing_var = self.get_variable(name);

        let tmp = if new {
            self.new_temporary(Some(name))
        } else {
            match existing_var {
                Ok((_, val)) => {
                    match val.clone() {
                        Value::Temporary(_) => {
                            val.clone()
                        }
                        _ => self.new_temporary(Some(name))
                    }
                }
                Err(_) => self.new_temporary(Some(name))
            }
        };

        let scope = self
            .scopes
            .last_mut()
            .expect("Expected last scope to exist");

        scope.insert(name.to_owned(), (ty.to_owned(), tmp.to_owned()));

        Ok(tmp)
    }

    fn get_variable(&self, name: &str) -> GeneratorResult<&(Type, Value)> {
        self.scopes
            .iter()
            .rev()
            .filter_map(|s| s.get(name))
            .next()
            .ok_or_else(|| format!("\nUndefined variable '{}'\n", name))
    }

    fn generate_function(
        &mut self,
        name: String,
        public: bool,
        arguments: &Vec<Argument>,
        return_type: String,
        body: Vec<AstNode>,
        module: &RefCell<Module>,
    ) -> GeneratorResult<Function> {
        self.scopes.push(HashMap::new());

        let mut args = Vec::new();

        for argument in arguments.clone() {
            let ty = Self::get_type(argument.r#type.clone())
                .expect("Argument cannot have a 'Nil' type.");
            let tmp = self.new_var(&ty, &argument.name, false)?;

            args.push((ty.into_abi(), tmp));
        }

        let mut func = Function {
            linkage: if public {
                Linkage::public()
            } else {
                Linkage::private()
            },
            name: name.clone(),
            arguments: args,
            return_type: Self::get_type(return_type),
            blocks: Vec::new(),
        };

        func.add_block("start".to_string());

        let func_ref = RefCell::new(func);

        for statement in body.clone() {
            // Ignore plain literals that aren't assigned to anything
            match statement.clone() {
                AstNode::LiteralStatement { kind, value: _ } => match kind.clone() {
                    TokenKind::ExactLiteral => match self.generate_statement(&func_ref, module, statement.clone(), None) {
                        Some((_, value)) => {
                            func_ref.borrow_mut().add_instruction(Instruction::Literal(value))
                        }
                        _ => {}
                    }
                    _ => {}
                }
                _ => match self.generate_statement(&func_ref, module, statement.clone(), None) {
                    _ => {}
                }
            }
        }

        if !func_ref.borrow_mut().returns() {
            if func_ref.borrow_mut().return_type.is_none() {
                func_ref
                    .borrow_mut()
                    .add_instruction(Instruction::Return(None));
            } else {
                return Err(format!(
                    "\nFunction does not return on all paths.\nReturn type is {:?}\n",
                    &func_ref.borrow_mut().return_type
                ));
            }
        }

        if func_ref.borrow_mut().return_type.is_none() {
            let mut func_r = func_ref.borrow_mut().clone();
            let last = func_r.last_block().statements.last();

            let return_type = last
                .iter()
                .find(|statement| matches!(statement, Statement::Volatile(Instruction::Return(_))));

            match return_type {
                Some(res) => match res {
                    Statement::Volatile(instruction) => {
                        match instruction {
                            Instruction::Return(value) => {
                                if value.is_none() {
                                    func_ref.borrow_mut().return_type = Some(Type::Word);
                                } else {
                                    match value.clone().unwrap() {
                                        Value::Const(_) => func_ref.borrow_mut().return_type = Some(Type::Word),
                                        Value::Global(_) => func_ref.borrow_mut().return_type = Some(Type::Long),
                                        Value::Temporary(val) => {
                                            let val_temp = val.clone();
                                            let last = val_temp.split("_").last().unwrap().to_string().parse::<i32>().unwrap() - 1;
                                            let var = self.get_variable(format!("r_v{}", last).as_str());
    
                                            match var {
                                                Ok((ty, _)) => {
                                                    func_ref.borrow_mut().return_type = Some(ty.clone());
                                                }
                                                Err(msg) => {
                                                    eprintln!("{}", msg);
                                                    func_ref.borrow_mut().return_type = Some(Type::Word);
                                                }
                                            }
                                        }
                                        Value::Literal(_) => func_ref.borrow_mut().return_type = Some(Type::Long)
                                    }
                                }
                            },
                            _ => panic!("\nSomehow the underlying instruction under the volatile pointer changed.\nThere may be a race condition.\n")
                        };
                    }
                    _ => panic!("Pointer is not volatile."),
                },
                None => func_ref.borrow_mut().return_type = Some(Type::Word),
            }
        }

        self.scopes.pop();

        let func_new = func_ref.borrow_mut().to_owned();
        Ok(func_new)
    }

    fn generate_statement(
        &mut self,
        func: &RefCell<Function>,
        module: &RefCell<Module>,
        stmt: AstNode,
        ty: Option<Type>
    ) -> Option<(Type, Value)> {
        let res = match stmt {
            AstNode::DeclareStatement {
                name,
                r#type,
                value,
            } => {
                let existing = match self.get_variable(name.as_str()) {
                    Ok((ty, _)) => ty.clone(),
                    Err(_) => Type::Word
                };

                if r#type == "Nil".to_owned() && self.get_variable(name.as_str()).is_err() {
                    panic!("\nVariable named '{}' hasn't been declared yet.\nPlease declare it before trying to re-declare it.\n", name);
                }

                let ty = Self::get_type(r#type).unwrap_or(existing);
                let temp = self.new_var(&ty, &name, false).unwrap();
                let parsed = self.generate_statement(func, module, *value.clone(), Some(ty.clone()));

                if parsed.is_some() {
                    let (_, value) = parsed.unwrap();

                    // dbg!(&ty, &value);

                    // match value.clone() {
                    //     Value::Temporary(value_name) => {
                    //         match self.scopes
                    //         .iter_mut()
                    //         .last()
                    //         .unwrap()
                    //         .iter_mut()
                    //         .find(|(_, (_, value))| match value {
                    //             Value::Temporary(name) => name.to_owned() == value_name,
                    //             _ => false
                    //         }) {
                    //             Some(res) => res.1.0 = ty.clone(),
                    //             _ => {}
                    //         }; 
                    //     },
                    //     _ => {}
                    // };

                    func.borrow_mut()
                        .assign_instruction(temp, ty, Instruction::Copy(value));
                }

                None
            }
            AstNode::ReturnStatement { value } => {
                match self.generate_statement(func, module, *value.clone(), ty) {
                    Some((ty, value)) => {
                        // Generate a unique scoped temporary variable with the tmp_counter to ensure
                        // it can be yielded later for inferring the return type
                        let tmp = self.new_var(&ty, format!("r_v{}", self.tmp_counter).as_str(), false);

                        match tmp {
                            Ok(val) => {
                                func.borrow_mut().assign_instruction(
                                    val.clone(),
                                    ty,
                                    Instruction::Copy(value),
                                );

                                func.borrow_mut()
                                    .add_instruction(Instruction::Return(Some(val)));
                            }
                            // In this case we should panic because the variable returned should be in scope
                            Err(msg) => panic!("{}", msg),
                        }
                    }
                    None => func.borrow_mut().add_instruction(Instruction::Return(None)),
                }

                None
            }
            AstNode::ArithmeticOperation {
                left,
                right,
                operator,
            } => {
                // Recursively parse the left and right nodes of the binary tree
                // This is why we cant do exponentiation if there isn't a built in instruction.
                // We essentially would have to pseudo-run the code to get the number of 
                // multiplications that should occur (a ** b) can have a call on lhs or rhs
                let (left_ty, left_val) = self
                    .generate_statement(func, module, *left.clone(), ty.clone())
                    .unwrap();
                
                let (_, right_val) = self
                    .generate_statement(func, module, *right.clone(), ty.clone())
                    .unwrap();

                let instruction_ty = ty.clone().unwrap_or(left_ty.clone());

                let left_temp = self.new_temporary(None);
                let right_temp = self.new_temporary(None);

                func.borrow_mut().assign_instruction(
                    left_temp.clone(),
                    instruction_ty.clone(),
                    Instruction::Copy(left_val),
                );

                // Use the left type as the type of them should match
                func.borrow_mut().assign_instruction(
                    right_temp.clone(),
                    instruction_ty.clone(),
                    Instruction::Copy(right_val),
                );

                let res = match operator {
                    TokenKind::Add => Instruction::Add(left_temp, right_temp),
                    TokenKind::Subtract => Instruction::Subtract(left_temp, right_temp),
                    TokenKind::Multiply => Instruction::Multiply(left_temp, right_temp),
                    TokenKind::Divide => Instruction::Divide(left_temp, right_temp),
                    TokenKind::Modulus => Instruction::Modulus(left_temp, right_temp),
                    TokenKind::GreaterThan => Instruction::Compare(
                        Type::Word,
                        Comparison::GreaterThan,
                        left_temp,
                        right_temp,
                    ),
                    TokenKind::GreaterThanEqual => Instruction::Compare(
                        Type::Word,
                        Comparison::GreaterThanEqual,
                        left_temp,
                        right_temp,
                    ),
                    TokenKind::LessThan => Instruction::Compare(
                        Type::Word,
                        Comparison::LessThan,
                        left_temp,
                        right_temp,
                    ),
                    TokenKind::LessThanEqual => Instruction::Compare(
                        Type::Word,
                        Comparison::LessThanEqual,
                        left_temp,
                        right_temp,
                    ),
                    TokenKind::EqualTo => {
                        Instruction::Compare(Type::Word, Comparison::Equal, left_temp, right_temp)
                    }
                    TokenKind::NotEqualTo => Instruction::Compare(
                        Type::Word,
                        Comparison::NotEqual,
                        left_temp,
                        right_temp,
                    ),
                    TokenKind::And => Instruction::BitwiseAnd(left_temp, right_temp),
                    TokenKind::Or => Instruction::BitwiseOr(left_temp, right_temp),
                    _ => panic!("Invalid operator token"),
                };

                let op_temp = self.new_temporary(None);

                func.borrow_mut()
                    .assign_instruction(op_temp.clone(), instruction_ty.clone(), res);

                Some((Type::Word, op_temp))
            }
            AstNode::LiteralStatement { kind, value } => match kind {
                TokenKind::Identifier => match value {
                    ValueKind::String(val) => {
                        let var = self.get_variable(val.as_str());

                        match var {
                            Ok(res) => Some(res.to_owned()),
                            Err(msg) => {
                                dbg!(&self.scopes);
                                panic!("{}", msg);
                            }
                        }
                    }
                    _ => None,
                },
                TokenKind::ExactLiteral => match value {
                    ValueKind::String(val) => Some((Type::Null, Value::Literal(val))),
                    _ => None,
                },
                _ => match value {
                    ValueKind::Number(val) => Some((Type::Word, Value::Const(val))),
                    ValueKind::String(val) => {
                        self.tmp_counter += 1;

                        let name = format!(
                            "{}_{}",
                            func.borrow_mut().name.to_string(),
                            self.tmp_counter
                        );

                        self.data_sections.push(Data::new(
                            Linkage::private(),
                            name.clone(),
                            None,
                            vec![
                                (Type::Byte, DataItem::String(val)),
                                (Type::Byte, DataItem::Const(0)),
                            ],
                        ));

                        Some((Type::Long, Value::Global(name)))
                    }
                    // Characters are just strings enforced to be a single character
                    // This is done at tokenization time
                    ValueKind::Character(val) => {
                        self.tmp_counter += 1;

                        let name = format!(
                            "{}_{}",
                            func.borrow_mut().name.to_string(),
                            self.tmp_counter
                        );

                        self.data_sections.push(Data::new(
                            Linkage::private(),
                            name.clone(),
                            None,
                            vec![
                                (Type::Byte, DataItem::String(val.to_string())),
                            ],
                        ));

                        Some((Type::Long, Value::Global(name)))
                    }
                    ValueKind::Nil => {
                        self.tmp_counter += 1;

                        let name = format!(
                            "{}_{}",
                            func.borrow_mut().name.to_string(),
                            self.tmp_counter
                        );

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
            AstNode::FunctionCall { name, parameters } => {
                let mut params = vec![];

                for parameter in parameters.clone() {
                    params.push(self.generate_statement(func, module, parameter, ty.clone()).unwrap());
                }

                // Get the type of the functions based on the ones currently imported into this module
                let declarative_ty = ty.unwrap_or(Type::Word);
                let ty = module
                    .borrow_mut()
                    .functions
                    .iter()
                    .find(|function| function.name == name)
                    .unwrap_or(&Function::new(
                        Linkage::public(),
                        name.clone(),
                        vec![],
                        Some(declarative_ty.clone()),
                    ))
                    .return_type
                    .clone()
                    .unwrap_or(declarative_ty);

                let temp = self.new_var(&ty, "tmp", true).unwrap();

                func.borrow_mut().assign_instruction(
                    temp.clone(),
                    ty.clone(),
                    Instruction::Call(name, params),
                );

                Some((ty, temp))
            }
            AstNode::BufferStatement { name, r#type, size } => {
                self.tmp_counter += 1;

                let buf_name = format!(
                    "{}_{}",
                    name,
                    self.tmp_counter
                );

                let buf_size = match size {
                    ValueKind::Number(val) => val,
                    other => panic!("Invalid size type {:?}", other)
                };

                let buf_ty = Self::get_type(r#type).unwrap_or(Type::Byte);

                self.data_sections.push(Data::new(
                    Linkage::private(),
                    buf_name.clone(),
                    None,
                    vec![(Type::Field, DataItem::Const((buf_size as u64 * buf_ty.size()).try_into().unwrap()))],
                ));

                let ty = Type::Long;
                let temp = self.new_var(&ty, name.as_str(), false).unwrap();

                func.borrow_mut().assign_instruction(temp, ty, Instruction::Copy(Value::Global(buf_name)));
                None
            }
            AstNode::StoreStatement { name, r#type, value } => {
                let existing = match self.get_variable(name.as_str()) {
                    Ok(val) => Some(val.clone()),
                    Err(msg) => {
                        eprintln!("{}", msg);
                        None
                    }
                }.unwrap_or((Type::Byte, Value::Literal("".to_owned())));

                let ty = Self::get_type(r#type).unwrap_or(existing.0);
                let (_, compiled) = self.generate_statement(func, module, *value.clone(), Some(ty.clone())).unwrap();

                func.borrow_mut().add_instruction(Instruction::Store(ty, existing.1.to_owned(), compiled));
                None
            }
            _ => todo!("statement: {:?}", stmt),
        };

        res
    }

    pub fn compile(tree: Vec<Primitive>) {
        let mut generator = Compiler {
            tmp_counter: 0,
            scopes: Vec::new(),
            data_sections: Vec::new(),
            tree,
        };

        let module = Module::new();

        // We need internal mutability here
        // Each string data section needs to be added to the module
        let module_ref = RefCell::new(module);

        for primitive in generator.tree.clone() {
            match primitive {
                Primitive::Constant {
                    name,
                    public,
                    r#type: _, // We don't care about the type it only exists for semantics
                    value,
                } => {
                    let res = match value {
                        ValueKind::String(value) => Data::new(
                            if public {
                                Linkage::public()
                            } else {
                                Linkage::private()
                            },
                            name.to_string(),
                            None,
                            vec![
                                (Type::Byte, DataItem::String(value.to_string())),
                                (Type::Byte, DataItem::Const(0)),
                            ],
                        ),
                        ValueKind::Number(value) => Data::new(
                            if public {
                                Linkage::public()
                            } else {
                                Linkage::private()
                            },
                            name.to_string(),
                            None,
                            vec![(Type::Word, DataItem::Const(value))],
                        ),
                        ValueKind::Character(value) => Data::new(
                            if public {
                                Linkage::public()
                            } else {
                                Linkage::private()
                            },
                            name.to_string(),
                            None,
                            vec![(Type::Byte, DataItem::String(value.to_string()))],
                        ),
                        ValueKind::Nil => Data::new(
                            if public {
                                Linkage::public()
                            } else {
                                Linkage::private()
                            },
                            name.to_string(),
                            None,
                            vec![(Type::Byte, DataItem::Const(0))],
                        ),
                    };

                    module_ref.borrow_mut().add_data(res);
                }
                Primitive::Operation {
                    name,
                    public,
                    arguments,
                    r#return,
                    body,
                } => match generator.generate_function(
                    name,
                    public,
                    &arguments,
                    r#return,
                    body,
                    &module_ref,
                ) {
                    Ok(function) => {
                        module_ref.borrow_mut().add_function(function);
                    }
                    Err(msg) => eprintln!("{}", msg),
                },
                _ => {}
            }
        }

        for data in generator.data_sections {
            module_ref.borrow_mut().add_data(data);
        }

        let mut file = File::create("main.ssa").expect("Failed to create the file.");
        file.write_all(module_ref.borrow().to_string().as_bytes())
            .expect("Failed to write to file.");

        file.flush().expect("Failed to flush file");
    }
}
