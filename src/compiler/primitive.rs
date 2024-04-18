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

                dbg!(&lines);
                let return_index = lines
                    .clone()
                    .iter()
                    .position(|line| line.starts_with("    ret"))
                    .unwrap_or(0);

                let return_string = &lines[return_index];
                let return_parts = return_string.splitn(2, "ret ").collect::<Vec<_>>();
                let return_statement = return_parts[1];

                let mut return_type = "w";

                // Data sections use the long type
                // Therefore globals, which use the $ sigil,
                // should use the long type too
                if return_statement.starts_with("$") {
                    return_type = "l"
                }

                let declaration = format!(
                    "{}function {} ${}({}) {{",
                    if public { "export " } else { "" },
                    return_type,
                    name,
                    arguments_string
                );

                lines.insert(0, declaration);
                lines.insert(1, "@start".to_string());

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
