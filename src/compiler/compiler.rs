use std::{
    borrow::{Borrow, BorrowMut},
    cell::RefCell,
    collections::HashMap,
    fs::File,
    io::Write,
    sync::Mutex,
};

use crate::{
    lexer::enums::{Token, TokenKind, ValueKind},
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
    pub fn get_type(&mut self, r#type: String) -> Option<Type> {
        match r#type.as_str() {
            "Byte" => Some(Type::Byte),
            "Halfword" => Some(Type::Halfword),
            "Word" => Some(Type::Word),
            "String" => Some(Type::Long),
            "Int" => Some(Type::Word),
            "Long" => Some(Type::Long),
            "Single" => Some(Type::Single),
            "Double" => Some(Type::Double),
            "Nil" => None,
            _ => Some(Type::Word),
        }
    }

    fn new_temporary(&mut self) -> Value {
        self.tmp_counter += 1;
        Value::Temporary(format!("tmp_{}", self.tmp_counter))
    }

    fn new_var(&mut self, ty: &Type, name: &str) -> GeneratorResult<Value> {
        let existing_var = self.get_var(name);

        let tmp = match existing_var {
            Ok((_, val)) => {
                match val.clone() {
                    Value::Temporary(name) => {
                        match self.get_tmp_index(name) {
                            Some(val) => Value::Temporary(format!("tmp_{}", val)),
                            None => self.new_temporary()
                        }
                    }
                    _ => self.new_temporary()
                }
            }
            Err(_) => self.new_temporary()
        };

        let scope = self
            .scopes
            .last_mut()
            .expect("Expected last scope to exist");

        scope.insert(name.to_owned(), (ty.to_owned(), tmp.to_owned()));

        Ok(tmp)
    }

    fn get_var(&self, name: &str) -> GeneratorResult<&(Type, Value)> {
        self.scopes
            .iter()
            .rev()
            .filter_map(|s| s.get(name))
            .next()
            .ok_or_else(|| format!("Undefined variable '{}'", name))
    }

    fn get_tmp_index(&self, text: String) -> Option<i32> {
        let parts = text.splitn(2, '_').collect::<Vec<_>>();

        if parts.len() == 2 {
            match parts[1].parse::<i32>() {
                Ok(num) => Some(num),
                Err(_) => None,
            }
        } else {
            // Not in the expected format
            None
        }
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
            let ty = self
                .get_type(argument.r#type.clone())
                .expect("Argument cannot have a Nil type.");
            let tmp = self.new_var(&ty, &argument.name)?;

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
            return_type: self.get_type(return_type),
            blocks: Vec::new(),
        };

        func.add_block("start".to_string());

        let func_ref = RefCell::new(func);

        for statement in body.clone() {
            match self.generate_statement(&func_ref, module, statement.clone()) {
                _ => {}
            }
        }

        if !func_ref.borrow_mut().returns() {
            if func_ref.borrow_mut().return_type.is_none() {
                func_ref
                    .borrow_mut()
                    .add_instruction(Instruction::Return(None));
            } else {
                return Err(format!(
                    "Function does not return on all paths (return type is {:?})",
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
                                            let var = self.get_var(format!("tmp_{}", self.get_tmp_index(val).unwrap_or(1) - 1).as_str());
    
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
                            _ => panic!("Somehow the underlying instruction under the volatile pointer changed. There may be a race condition.")
                        };
                    }
                    _ => panic!("Pointer is not volatile"),
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
    ) -> Option<(Type, Value)> {
        let res = match stmt {
            AstNode::DeclareStatement {
                name,
                r#type,
                value,
            } => {
                let existing = match self.get_var(name.as_str()) {
                    Ok((ty, _)) => ty.clone(),
                    Err(_) => Type::Word
                };

                let ty = self.get_type(r#type).unwrap_or(existing);
                let temp = self.new_var(&ty, &name).unwrap();
                let parsed = self.generate_statement(func, module, *value.clone());

                if parsed.is_some() {
                    let (ty, value) = parsed.unwrap();

                    func.borrow_mut()
                        .assign_instruction(temp, ty, Instruction::Copy(value));
                }

                None
            }
            AstNode::ReturnStatement { value } => {
                match self.generate_statement(func, module, *value.clone()) {
                    Some((ty, value)) => {
                        let tmp = self.new_var(&ty, format!("tmp_{}", self.tmp_counter).as_str());

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
                let (left_ty, left_val) = self
                    .generate_statement(func, module, *left.clone())
                    .unwrap();
                let (right_ty, right_val) = self
                    .generate_statement(func, module, *right.clone())
                    .unwrap();

                let left_temp = self.new_temporary();
                let right_temp = self.new_temporary();

                func.borrow_mut().assign_instruction(
                    left_temp.clone(),
                    left_ty.clone(),
                    Instruction::Copy(left_val),
                );

                func.borrow_mut().assign_instruction(
                    right_temp.clone(),
                    right_ty.clone(),
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

                let op_temp = self.new_temporary();

                func.borrow_mut()
                    .assign_instruction(op_temp.clone(), Type::Word, res);

                Some((Type::Word, op_temp))
            }
            AstNode::LiteralStatement { kind, value } => match kind {
                TokenKind::Identifier => match value {
                    ValueKind::String(val) => {
                        let var = self.get_var(val.as_str());

                        match var {
                            Ok(res) => Some(res.to_owned()),
                            Err(msg) => {
                                eprintln!("{}", msg);
                                None
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
                                (Type::Byte, DataItem::Str(val)),
                                (Type::Byte, DataItem::Const(0)),
                            ],
                        ));

                        Some((Type::Long, Value::Global(name)))
                    }
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
                                (Type::Byte, DataItem::Str(val.to_string())),
                                (Type::Byte, DataItem::Const(0))
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
                    params.push(self.generate_statement(func, module, parameter).unwrap());
                }

                let temp = self.new_temporary();
                let ty = module
                    .borrow_mut()
                    .functions
                    .iter()
                    .find(|function| function.name == name)
                    .unwrap_or(&Function::new(
                        Linkage::public(),
                        name.clone(),
                        vec![],
                        Some(Type::Word),
                    ))
                    .return_type
                    .clone()
                    .unwrap_or(Type::Word);

                func.borrow_mut().assign_instruction(
                    temp.clone(),
                    ty.clone(),
                    Instruction::Call(name, params),
                );

                Some((ty, temp))
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
        let module_ref = RefCell::new(module);

        for primitive in generator.tree.clone() {
            match primitive {
                Primitive::Constant {
                    name,
                    public,
                    r#type,
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
                                (Type::Byte, DataItem::Str(value.to_string())),
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
                            vec![(Type::Byte, DataItem::Str(value.to_string()))],
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
