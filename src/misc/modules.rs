use std::cell::RefCell;
use std::collections::HashSet;
use std::fs;
use std::path::Path;
use std::process::exit;
use std::time::Instant;

use crate::{
    compiler::enums::Type,
    elapsed_with_color,
    lexer::{
        enums::{Location, TokenKind, ValueKind},
        lexer::Lexer,
    },
    misc::colors::*,
    override_and_add_node,
    parser::{
        enums::{Argument, AstNode, Primitive},
        parser::{DoOnly, Parser, StructPool},
    },
    Warnings, STD_LIB_PATH,
};

pub fn lex_and_parse(
    input_path: &String,
    existing_tree: Option<&mut Vec<Primitive>>,
    struct_pool: &RefCell<StructPool>,
    parsed_modules: &RefCell<HashSet<String>>,
    warnings: &Warnings,
    debug_time: bool,
    nesting: usize,
    _import_location: Location,
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

    // Import non-generic modules
    // Import structs
    // Import generic modules
    // Import rest
    let mut parser = Parser::new(
        tokens.clone(),
        struct_pool.borrow().to_owned(),
        warnings.clone(),
    );

    let mut fallback = vec![];
    let mut tree = existing_tree.unwrap_or(&mut fallback);

    // Non-generic imports and generic declarations
    let (imports, new_struct_pool, _) = parser.parse(&DoOnly::Imports, None);
    struct_pool.replace_with(|_| new_struct_pool);

    for import in imports.iter().cloned() {
        match import {
            Primitive::Use {
                module,
                mut location,
                ..
            } if !parsed_modules.borrow().contains(&module) => {
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
                        module,
                    );
                }

                location.length = location.ctx.len();
                location.column = location.ctx.len();

                let nodes = lex_and_parse(
                    &module,
                    Some(tree),
                    struct_pool,
                    parsed_modules,
                    warnings,
                    debug_time,
                    nesting + 1,
                    location,
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
                            if let Some(pos) = existing_definition(tree, &name) {
                                if symbol == tree.get(pos).unwrap() {
                                    tree.remove(pos);
                                    tree.insert(0, symbol.clone());
                                } else {
                                    override_and_add_node!(
                                        Primitive::Struct,
                                        &mut tree,
                                        &name,
                                        symbol,
                                        public
                                    );
                                }
                            } else {
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

    // Structs
    let (structs, new_struct_pool, _) =
        parser.parse(&DoOnly::Structs, Some(struct_pool.borrow().to_owned()));
    struct_pool.replace_with(|_| new_struct_pool);
    tree.extend(structs);

    let (others, new_struct_pool, extra_structs) = parser.parse(
        &DoOnly::FunctionsAndConstants,
        Some(struct_pool.borrow().to_owned()),
    );
    struct_pool.replace_with(|_| new_struct_pool);
    tree.extend(extra_structs);
    tree.extend(others);

    // Add global constants
    // - nil => 0 (nullptr)
    // - ElleMeta => Utility struct
    // - bool::to_string
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
            Primitive::Function {
                name: "bool.to_string".into(),
                public: false,
                usable: true,
                imported: false,
                variadic: false,
                manual: false,
                external: false,
                builtin: true,
                volatile: false,
                unaliased: None,
                generics: vec![],
                arguments: vec![Argument {
                    name: "self".into(),
                    r#type: Type::Boolean,
                    manual: false,
                }],
                r#return: Some(Type::Pointer(Box::new(Type::Char))),
                body: vec![AstNode::IfStatement {
                    condition: Box::new(AstNode::LiteralStatement {
                        kind: TokenKind::Identifier,
                        value: ValueKind::String("self".into()),
                        location: Location::default(input_path.clone()),
                    }),
                    body: vec![AstNode::ReturnStatement {
                        value: Box::new(AstNode::LiteralStatement {
                            kind: TokenKind::StringLiteral,
                            value: ValueKind::String("true".into()),
                            location: Location {
                                file: input_path.clone(),
                                row: 0,
                                column: 0,
                                ctx: "\"false\"".into(),
                                length: 7, // Length of the ctx above
                                above: None,
                                extra_info: "".into(),
                            },
                        }),
                        location: Location {
                            file: input_path.clone(),
                            row: 0,
                            column: 0,
                            ctx: "return \"true\";".into(),
                            length: 14, // Length of the ctx above
                            above: None,
                            extra_info: "".into(),
                        },
                    }],
                    else_body: vec![AstNode::ReturnStatement {
                        value: Box::new(AstNode::LiteralStatement {
                            kind: TokenKind::StringLiteral,
                            value: ValueKind::String("false".into()),
                            location: Location {
                                file: input_path.clone(),
                                row: 0,
                                column: 0,
                                ctx: "\"true\"".into(),
                                length: 6, // Length of the ctx above
                                above: None,
                                extra_info: "".into(),
                            },
                        }),
                        location: Location {
                            file: input_path.clone(),
                            row: 0,
                            column: 0,
                            ctx: "return \"false\";".into(),
                            length: 15, // Length of the ctx above
                            above: None,
                            extra_info: "".into(),
                        },
                    }],
                    location: Location {
                        file: input_path.clone(),
                        row: 0,
                        column: 0,
                        ctx: "if self {".into(),
                        length: 9, // Length of the ctx above
                        above: None,
                        extra_info: "".into(),
                    },
                }],
                location: Location {
                    file: input_path.clone(),
                    row: 0,
                    column: 0,
                    ctx: "fn bool::to_string(bool self) -> string {".into(),
                    length: 41, // Length of the ctx above
                    above: None,
                    extra_info: "".into(),
                },
                return_location: Location {
                    file: input_path.clone(),
                    row: 0,
                    column: 33,
                    ctx: "fn bool::to_string(bool self) -> string {".into(),
                    length: 6, // Length of the type
                    above: None,
                    extra_info: "".into(),
                },
            },
        )
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
