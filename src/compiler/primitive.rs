use crate::parser::enums::{AstNode, Primitive};

use super::statement::StatementExpr;

pub struct PrimitiveExpr {
    primitive: Primitive,
}

impl PrimitiveExpr {
    pub fn new(primitive: Primitive) -> Self {
        PrimitiveExpr { primitive }
    }

    pub fn compile(&mut self) -> Option<String> {
        let result = match self.primitive.clone() {
            Primitive::Operation {
                name,
                public,
                arguments,
                r#return,
                body,
            } => {
                let mut lines = vec![];

                let parsed_arguments: Vec<String> = arguments
                    .clone()
                    .iter_mut()
                    .map(|argument| format!("w %{}", argument.name).to_string())
                    .collect();

                let arguments_string = parsed_arguments.join(", ");

                let declaration = format!(
                    "{}function w ${}({}) {{",
                    if public { "export " } else { "" },
                    name,
                    arguments_string
                );

                lines.push(declaration);
                lines.push("@start".to_string());

                let mut has_return = false;

                for statement in body.clone().iter_mut() {
                    match statement {
                        AstNode::ReturnStatement { value: _ } => {
                            has_return = true;
                        }
                        _ => {}
                    }

                    let expr = StatementExpr::new(statement.clone(), name.clone()).compile();

                    match expr {
                        Some(value) => lines.push(format!("    {}", value)),
                        None => {}
                    }
                }

                if !has_return {
                    lines.push("    ret 0".to_string());
                }

                lines.push("}".to_string());
                lines.join("\n")
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
