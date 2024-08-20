use std::cell::RefCell;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::fs;

use compiler::enums::Type;
use lexer::enums::Location;
use lexer::enums::ValueKind;
use lexer::{enums::TokenKind, lexer::Lexer};
use parser::enums::Argument;
use parser::enums::AstNode;
use parser::enums::Primitive;
use parser::parser::Parser;

use crate::compiler;
use crate::lexer;
use crate::override_and_add_node;
use crate::parser;
use crate::Warnings;
use crate::META_STRUCT_NAME;

pub fn lex_and_parse(
    input_path: &String,
    existing_tree: Option<&mut Vec<Primitive>>,
    struct_pool: &RefCell<HashSet<String>>,
    parsed_modules: &RefCell<HashSet<String>>,
    warnings: &Warnings,
    root: bool,
) -> Vec<Primitive> {
    let with_elle = fs::metadata(format!("{}.elle", input_path)).is_ok();
    let base = fs::metadata(input_path.clone()).is_ok();

    let content = match fs::read_to_string(&format!(
        "{}{}",
        input_path,
        if base {
            ""
        } else {
            if with_elle {
                ".elle"
            } else {
                ".l"
            }
        }
    )) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("\nERROR: Could not load module \"{input_path}\": {err}\n");
            return Vec::new();
        }
    };

    if content.trim().is_empty() {
        panic!(
            "\n{}\nERROR: Could not load module \"{input_path}\"\n{}\n\n{}\n{}\n",
            "-".repeat(40),
            "Module is empty. To create an entry-point, write:",
            "use std/io;\n\nfn main() {\n\n}",
            "-".repeat(40),
        )
    }

    let mut lexer = Lexer::new(input_path.clone(), content.as_str());
    let mut tokens = vec![];

    while let Some(token) = lexer.next_token() {
        // Even though the lexer does provide us with comments, we don't care about them
        // so we can just ignore them and not pass them the parser
        match token.kind {
            TokenKind::Comment => {}
            _ => tokens.push(token),
        }
    }

    let mut parser = Parser::new(
        tokens.clone(),
        struct_pool.borrow().to_owned(),
        warnings.clone(),
    );

    let mut fallback = vec![];
    let tree = existing_tree.unwrap_or(&mut fallback);

    let (imports, new_struct_pool) = parser.parse(true, None);
    struct_pool.replace_with(|_| new_struct_pool);

    for import in imports.iter().cloned() {
        match import {
            Primitive::Use { module, .. } if !parsed_modules.borrow().contains(&module) => {
                let nodes = lex_and_parse(
                    &module,
                    Some(tree),
                    struct_pool,
                    parsed_modules,
                    warnings,
                    false,
                );

                for symbol in nodes.iter().rev() {
                    match symbol.clone() {
                        Primitive::Use { .. } => {}
                        Primitive::Constant { name, public, .. } => {
                            override_and_add_node!(Primitive::Constant, tree, name, symbol, public);
                        }
                        Primitive::Function { name, public, .. } => {
                            override_and_add_node!(Primitive::Function, tree, name, symbol, public);
                        }
                        Primitive::Struct { name, public, .. } => {
                            override_and_add_node!(Primitive::Struct, tree, name, symbol, public);
                        }
                    }
                }

                parsed_modules.borrow_mut().insert(module);
            }
            _ => {}
        }
    }

    let (others, new_struct_pool) = parser.parse(false, Some(struct_pool.borrow().to_owned()));
    struct_pool.replace_with(|_| new_struct_pool);

    tree.extend(others);

    // Add global constants
    // - nil => 0 (nullptr)
    // - ElleMeta => Utility struct
    if root {
        tree.insert(
            0,
            Primitive::Constant {
                name: "nil".into(),
                public: false,
                usable: true,
                imported: false,
                // void *
                r#type: Some(Type::Pointer(Box::new(Type::Void))),
                value: Box::new(AstNode::LiteralStatement {
                    kind: TokenKind::LongLiteral,
                    value: ValueKind::Number(0),
                    location: Location::default(input_path.clone()),
                }),
                location: Location::default(input_path.clone()),
            },
        );

        tree.insert(
            0,
            Primitive::Struct {
                name: META_STRUCT_NAME.into(),
                public: false,
                usable: true,
                imported: false,
                members: vec![
                    // Holds an array of expressions passed into the function in plain text
                    Argument {
                        name: "exprs".into(),
                        // string[]
                        r#type: Type::Pointer(Box::new(Type::Pointer(Box::new(Type::Char)))),
                    },
                    // Holds an array of the type of arguments passed into the function as strings
                    Argument {
                        name: "types".into(),
                        // string[]
                        r#type: Type::Pointer(Box::new(Type::Pointer(Box::new(Type::Char)))),
                    },
                    // Holds the number of arguments that were passed into a function
                    Argument {
                        name: "arity".into(),
                        // i32
                        r#type: Type::Word,
                    },
                    // Holds the name of the caller method as a string
                    Argument {
                        name: "caller".into(),
                        // string
                        r#type: Type::Pointer(Box::new(Type::Char)),
                    },
                ],
                location: Location::default(input_path.clone()),
            },
        );
    }

    tree.to_vec()
}

pub fn existing_definition(tree: Vec<Primitive>, node_name: String) -> Option<usize> {
    tree.iter().position(|item| match item {
        Primitive::Use { .. } => false,
        Primitive::Constant { name, .. } => name == &node_name,
        Primitive::Function { name, .. } => name == &node_name,
        Primitive::Struct { name, .. } => name == &node_name,
    })
}
