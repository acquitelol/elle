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
    Warnings, LEN_CONSTANT, LONG_EXTENSION, POINTER_ID, PRIMARY_ALLOCATOR_MODULE, SHORT_EXTENSION,
    STD_LIB_PATH,
};

pub fn lex_and_parse(
    input_path: &String,
    existing_tree: Option<&mut Vec<Primitive>>,
    struct_pool: &RefCell<StructPool>,
    parsed_modules: &RefCell<HashSet<String>>,
    warnings: &Warnings,
    no_strings: bool,
    no_gc: bool,
    no_fmt: bool,
    debug_time: bool,
    object_output: bool,
    nesting: usize,
    _import_location: Location,
    string_module_methods: &mut Vec<String>,
) -> Vec<Primitive> {
    let content = {
        let with_elle = fs::metadata(format!("{}{}", input_path, LONG_EXTENSION)).is_ok();
        let base = fs::metadata(input_path).is_ok();

        let file_path = &format!(
            "{}{}",
            input_path,
            if base {
                ""
            } else {
                if with_elle {
                    LONG_EXTENSION
                } else {
                    SHORT_EXTENSION
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

    if content.trim().is_empty() && !object_output {
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
    let (mut imports, new_struct_pool, ..) = parser.parse(&DoOnly::Imports, None);
    struct_pool.replace_with(|_| new_struct_pool);

    if nesting == 0 && !no_fmt {
        imports.insert(
            0,
            Primitive::Use {
                module: "std/fmt".into(),
                location: Location::default(input_path.clone()),
            },
        );
    }

    if nesting == 0 && !no_strings {
        imports.insert(
            0,
            Primitive::Use {
                module: "std/string".into(),
                location: Location::default(input_path.clone()),
            },
        );
    }

    if nesting == 0 && !no_gc {
        imports.insert(
            0,
            Primitive::Use {
                module: PRIMARY_ALLOCATOR_MODULE.into(),
                location: Location::default(input_path.clone()),
            },
        );
    }

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
                    no_strings,
                    no_gc,
                    no_fmt,
                    debug_time,
                    object_output,
                    nesting + 1,
                    location,
                    string_module_methods,
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
                                        public,
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

                    if module == "std/string" {
                        *string_module_methods = tree
                            .iter()
                            .filter(|primitive| matches!(primitive, Primitive::Function { .. }))
                            .map(|f| match f {
                                Primitive::Function { name, .. } => name.clone(),
                                _ => unreachable!(),
                            })
                            .filter(|x| x.starts_with("string."))
                            .collect::<Vec<String>>();
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
    let (structs, new_struct_pool, ..) =
        parser.parse(&DoOnly::Structs, Some(struct_pool.borrow().to_owned()));
    struct_pool.replace_with(|_| new_struct_pool);
    tree.extend(structs);

    let (others, new_struct_pool, ..) = parser.parse(
        &DoOnly::FunctionsAndConstants,
        Some(struct_pool.borrow().to_owned()),
    );

    struct_pool.replace_with(|_| new_struct_pool);
    tree.extend(others);

    // Add global constants
    // - nil => 0 (nullptr)
    // - ElleMeta => Utility struct
    if nesting == 0 {
        let loc = Location::default(input_path.clone());

        tree.insert(
            0,
            Primitive::Constant {
                name: "nil".into(),
                public: false,
                usable: true,
                imported: false,
                // void *
                r#type: Some(Type::Pointer(Box::new(Type::Void))),
                value: Box::new(AstNode::Literal {
                    kind: TokenKind::LongLiteral,
                    value: ValueKind::Number(0),
                    location: loc.clone(),
                }),
                location: loc.clone(),
            },
        );

        tree.insert(
            0,
            Primitive::Function {
                name: format!("{}.{LEN_CONSTANT}", POINTER_ID).into(),
                public: false,
                usable: true,
                imported: false,
                variadic: false,
                manual: false,
                external: false,
                builtin: true,
                volatile: false,
                format: false,
                unaliased: None,
                generics: vec!["T".into()],
                arguments: vec![Argument {
                    name: "self".into(),
                    r#type: Type::Pointer(Box::new(Type::Unknown("T".into()))),
                    manual: false,
                }],
                r#return: Some(Type::Word),
                body: vec![AstNode::Return {
                    value: Box::new(AstNode::ArrayLength {
                        value: Box::new(AstNode::Literal {
                            kind: TokenKind::Identifier,
                            value: ValueKind::String("self".into()),
                            location: loc.clone(),
                        }),
                        location: loc.clone(),
                    }),
                    location: loc.clone(),
                }],
                location: loc.clone(),
                return_location: loc.clone(),
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
