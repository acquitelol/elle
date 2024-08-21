use std::cell::RefCell;
use std::collections::HashSet;
use std::fs;
use std::path::Path;
use std::process::exit;
use std::time::Instant;

use crate::compiler::enums::Type;
use crate::lexer::enums::Location;
use crate::lexer::enums::ValueKind;
use crate::lexer::{enums::TokenKind, lexer::Lexer};
use crate::parser::enums::Argument;
use crate::parser::enums::AstNode;
use crate::parser::enums::Primitive;
use crate::parser::parser::Parser;

use crate::elapsed_with_color;
use crate::lexer::colors::*;
use crate::override_and_add_node;
use crate::Warnings;
use crate::META_STRUCT_NAME;
use crate::STD_LIB_PATH;

pub fn lex_and_parse(
    input_path: &String,
    existing_tree: Option<&mut Vec<Primitive>>,
    struct_pool: &RefCell<HashSet<String>>,
    parsed_modules: &RefCell<HashSet<String>>,
    warnings: &Warnings,
    debug_time: bool,
    nesting: usize,
) -> Vec<Primitive> {
    let content = {
        let with_elle = fs::metadata(format!("{}.elle", input_path)).is_ok();
        let base = fs::metadata(input_path).is_ok();

        let file_path = &format!(
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
        );

        // Try to see if the file is installed as a library first
        let base_path = Path::new(STD_LIB_PATH);
        let relative_path = Path::new(&file_path);
        let full_path = base_path.join(relative_path);

        let final_path = if fs::metadata(&full_path).is_ok() {
            full_path
        } else {
            relative_path.to_path_buf()
        };

        let content = match fs::read_to_string(final_path) {
            Ok(content) => content,
            Err(err) => {
                eprintln!(
                    "\n{RED}ERROR: Could not load module \"{}\": {}\n",
                    input_path, err
                );

                if nesting == 0 {
                    exit(1);
                }

                return vec![];
            }
        };

        content
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
    let mut tree = existing_tree.unwrap_or(&mut fallback);

    let (imports, new_struct_pool) = parser.parse(true, None);
    struct_pool.replace_with(|_| new_struct_pool);

    for import in imports.iter().cloned() {
        match import {
            Primitive::Use { module, .. } if !parsed_modules.borrow().contains(&module) => {
                let now = if debug_time {
                    Some(Instant::now())
                } else {
                    None
                };

                if debug_time {
                    println!(
                        "{}╭― Importing module '{GREEN}{}{RESET}'",
                        if nesting > 0 {
                            "┆    ".repeat(nesting)
                        } else {
                            "".into()
                        },
                        module
                    );
                }

                let nodes = lex_and_parse(
                    &module,
                    Some(tree),
                    struct_pool,
                    parsed_modules,
                    warnings,
                    debug_time,
                    nesting + 1,
                );

                for symbol in nodes.iter().rev() {
                    match symbol.clone() {
                        Primitive::Use { .. } => {}
                        Primitive::Constant { name, public, .. } => {
                            override_and_add_node!(
                                Primitive::Constant,
                                &mut tree,
                                &name,
                                symbol,
                                public
                            );
                        }
                        Primitive::Function { name, public, .. } => {
                            override_and_add_node!(
                                Primitive::Function,
                                &mut tree,
                                &name,
                                symbol,
                                public
                            );
                        }
                        Primitive::Struct { name, public, .. } => {
                            override_and_add_node!(
                                Primitive::Struct,
                                &mut tree,
                                &name,
                                symbol,
                                public
                            );
                        }
                    }
                }

                if debug_time {
                    println!(
                        "{}╰― Imported '{GREEN}{}{RESET}' in {}",
                        if nesting > 0 {
                            "┆    ".repeat(nesting)
                        } else {
                            "".into()
                        },
                        module,
                        elapsed_with_color!(now.unwrap().elapsed())
                    );
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
    if nesting == 0 {
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

pub fn existing_definition(tree: &mut Vec<Primitive>, node_name: &str) -> Option<usize> {
    tree.iter().position(|item| match item {
        Primitive::Use { .. } => false,
        Primitive::Constant { name, .. } => name == &node_name,
        Primitive::Function { name, .. } => name == &node_name,
        Primitive::Struct { name, .. } => name == &node_name,
    })
}
