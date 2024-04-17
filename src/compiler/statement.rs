use crate::{
    compiler::compiler::Global,
    lexer::enums::{TokenKind, ValueKind},
    parser::enums::AstNode,
};

use super::compiler::GLOBALS;

pub struct StatementExpr {
    statement: AstNode,
    op_name: String,
}

impl StatementExpr {
    pub fn new(statement: AstNode, op_name: String) -> Self {
        StatementExpr { statement, op_name }
    }

    fn match_value(&mut self, value: ValueKind) -> String {
        match value {
            ValueKind::String(res) => format!("b \"{}\", b 0", res),
            ValueKind::Character(res) => format!("b \"{}\"", res),
            ValueKind::Number(res) => format!("{}", res),
            ValueKind::Nil => "0".to_string(),
        }
    }

    fn extract_number(&mut self, text: String) -> Option<i32> {
        let parts = text.splitn(2, '.').collect::<Vec<_>>();

        if parts.len() == 2 && parts[0] == self.op_name.clone() {
            match parts[1].parse::<i32>() {
                Ok(num) => Some(num),
                Err(_) => None,
            }
        } else {
            // Not in the expected format
            None
        }
    }

    pub fn compile(&mut self) -> Option<String> {
        let result = match &self.statement {
            AstNode::ReturnStatement { value } => {
                let res = match Self::new(*value.clone(), self.op_name.clone()).compile() {
                    // This is called r_v because Elle doesn't support underscores
                    Some(res) => res,
                    None => "".to_string(),
                };

                match *value.clone() {
                    AstNode::LiteralStatement { kind: _, value: _ } => {
                        format!("ret {}", res)
                    }
                    _ => format!("%r_v =w {}\n    ret %r_v", res),
                }
            }
            AstNode::ArithmeticOperation {
                left,
                right,
                operator,
            } => {
                let operator_string = match operator.clone() {
                    TokenKind::Add => "add".to_string(),
                    TokenKind::Subtract => "sub".to_string(),
                    TokenKind::Multiply => "mul".to_string(),
                    TokenKind::Divide => "div".to_string(),
                    TokenKind::Modulus => "rem".to_string(),
                    _ => "".to_string(),
                };

                let left_value = Self::new(*left.clone(), self.op_name.clone())
                    .compile()
                    .unwrap();

                let right_value = Self::new(*right.clone(), self.op_name.clone())
                    .compile()
                    .unwrap();

                format!("{} {}, {}", operator_string, left_value, right_value)
            }
            AstNode::LiteralStatement { kind, value } => match kind.clone() {
                TokenKind::Identifier => match value.clone() {
                    ValueKind::String(res) => {
                        let globals = GLOBALS.lock().unwrap();

                        match globals.iter().position(|item| item.name == res) {
                            Some(index) => {
                                format!("${}", globals[index].name)
                            }
                            None => format!("%{}", res),
                        }
                    }
                    _ => "0".to_string(),
                },
                TokenKind::StringLiteral => {
                    let mut globals = GLOBALS.lock().unwrap();
                    let mut variate = 0;
                    let parsed_value = self.match_value(value.clone());

                    for global in globals.iter() {
                        if global
                            .name
                            .clone()
                            .starts_with(format!("{}.", self.op_name.clone()).as_str())
                        {
                            match self.extract_number(global.name.clone()) {
                                Some(res) => {
                                    if res == variate {
                                        variate += 1;
                                    }
                                }
                                _ => {}
                            }
                        }

                        if global.value == parsed_value {
                            match self.extract_number(global.name.clone()) {
                                Some(res) => {
                                    variate = res;
                                }
                                _ => {}
                            }
                        }
                    }

                    let parsed_name = format!("{}.{}", self.op_name.clone(), variate);

                    globals.push(Global {
                        name: parsed_name.clone(),
                        value: parsed_value.clone(),
                        public: false,
                    });

                    format!("${}", parsed_name)
                }
                _ => self.match_value(value.clone()),
            },
            AstNode::DeclareStatement {
                name,
                r#type,
                value,
            } => {
                let res = Self::new(*value.clone(), self.op_name.clone())
                    .compile()
                    .unwrap();

                let prefix = if res.starts_with("w") { "" } else { "w " };

                format!("%{} ={}{}", name, prefix, res)
            }
            AstNode::FunctionCall { name, parameters } => {
                let mut res = parameters
                    .clone()
                    .iter_mut()
                    .map(|item| {
                        let res = Self::new(item.clone(), self.op_name.clone())
                            .compile()
                            .unwrap();

                        let prefix = if res.starts_with("$") { "l" } else { "w" };

                        format!("{} {}", prefix, res)
                    })
                    .collect::<Vec<String>>();

                // Polyfill for C variadic printf, inserts ... after the formatted string
                if name == "printf" {
                    res.insert(1, "...".to_string())
                }

                format!("call ${}({})", name, res.join(", "))
            }
            _ => "".to_string(),
        };

        if result == "".to_string() {
            None
        } else {
            Some(result)
        }
    }
}
