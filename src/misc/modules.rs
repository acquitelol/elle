use std::collections::HashSet;
use std::env::{current_dir, set_current_dir};
use std::fs;
use std::path::Path;
use std::process::exit;
use std::time::Instant;
use std::{cell::RefCell, rc::Rc};

use crate::compiler::qbe::r#type::Type;
use crate::lexer::enums::Token;
use crate::parser::enums::{
    ArrayLength, ConstantSource, FunctionSource, Literal, Return, StructSource, UseSource,
};
use crate::{
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
    Warnings, LEN_CONSTANT, POINTER_ID, PRIMARY_ALLOCATOR_MODULE, SHORT_EXTENSION, STD_LIB_PATH,
};
use crate::{
    elle_error, get_POINTER_ID, get_STD_LIB_PATH, ARBITRARY_ALLOCATOR_MODULE,
    BACKUP_ALLOCATOR_MODULE,
};

pub fn lex_and_parse(
    input_path: &String,
    existing_tree: Option<&mut Vec<Primitive>>,
    struct_pool: &RefCell<StructPool>,
    parsed_modules: &RefCell<HashSet<String>>,
    warnings: &Warnings,
    no_strings: bool,
    no_alloc: bool,
    no_gc: bool,
    no_fmt: bool,
    debug_time: bool,
    object_output: bool,
    expect_info: bool,
    nesting: usize,
    import_location: Rc<Location>,
    string_module_methods: &mut Vec<String>,
) -> Vec<Primitive> {
    let is_std_import;
    let final_path;

    let content = {
        let base = fs::metadata(input_path).is_ok();
        let file_path = &format!("{}{}", input_path, if base { "" } else { SHORT_EXTENSION });

        let relative_path_string = &format!("./{file_path}");

        // Try to see if the file is installed as a library first
        let base_path = Path::new(get_STD_LIB_PATH!());
        let relative_base_path = Path::new(&file_path);
        let relative_path = Path::new(&relative_path_string);
        let full_path = base_path.join(relative_base_path);

        is_std_import = fs::metadata(&full_path).is_ok();
        final_path = if is_std_import {
            full_path
        } else {
            relative_path.to_path_buf()
        };

        let content = match fs::read_to_string(final_path.clone()) {
            Ok(content) => content,
            Err(err) => {
                eprintln!(
                    "{}",
                    import_location.basic_error(format!(
                        "Could not load module \"{RED}{}{RESET}\":\n{}",
                        input_path,
                        err,
                        RED = get_RED!(),
                        RESET = get_RESET!()
                    ))
                );

                if nesting == 0 {
                    exit(1);
                }

                return vec![];
            }
        };

        content
    };

    macro_rules! file_is_empty_error {
        () => {
            elle_error!(import_location.basic_error(format!(
                "Could not load module \"{MAGENTA}{input_path}{RESET}\":\n{}\n\n{}{RESET}",
                "Module is empty. To create an entry-point, write:",
                format!(
                    "{GREEN}+ use std/prelude;\n+ \n+ fn main() {{\n+ \n+ }}{RESET}",
                    GREEN = get_GREEN!(),
                    RESET = get_RESET!()
                ),
                MAGENTA = get_MAGENTA!(),
                RESET = get_RESET!()
            )))
        };
    }

    // Throw an error before it explodes in the lexer
    if content.trim().is_empty() && !object_output {
        file_is_empty_error!()
    }

    // `has_tagged = root only` ensures modules wont have any tagged tokens
    let mut lexer = Lexer::new(
        input_path.clone(),
        content.as_str(),
        nesting != 0 || !expect_info,
    );
    let mut tokens = vec![];

    while let Some(mut token) = lexer.next_token() {
        // Give tokens an alt location so that this can be reported
        // instead, if the error happened in another file
        let mut location = (*token.location).clone();
        location.alt_start = import_location.start.clone();
        location.alt_end = import_location.end.clone();
        token.location = Rc::new(location);

        // Even though the lexer does provide us with comments, we don't care about them
        // so we can just ignore them and not pass them the parser
        match token.kind {
            TokenKind::Comment => {}
            _ => tokens.push(token),
        }
    }

    // File consists entirely of comments which is still effectively
    // an empty file, so we shouldn't bother parsing this
    if tokens.is_empty() {
        file_is_empty_error!()
    }

    // Things are parsed in a specific order:
    // 1. Imports
    // 2. Structs
    // 3. Functions & Constants
    let mut parser = Parser::new(
        tokens.clone(),
        struct_pool.borrow().to_owned(),
        warnings.clone(),
    );

    let mut fallback = vec![];
    let mut tree = existing_tree.unwrap_or(&mut fallback);

    let (mut imports, new_struct_pool, ..) = parser.parse(&DoOnly::Imports, None);
    struct_pool.replace_with(|_| new_struct_pool);
    let loc = Rc::new(Location::default(input_path.clone()));

    if nesting == 0 && !no_fmt {
        imports.insert(
            0,
            Primitive::Use(UseSource {
                module: "std/fmt".into(),
                location: loc.clone(),
            }),
        );
    }

    if nesting == 0 && !no_strings {
        imports.insert(
            0,
            Primitive::Use(UseSource {
                module: "std/string".into(),
                location: loc.clone(),
            }),
        );
    }

    if nesting == 0 && !no_alloc {
        imports.insert(
            0,
            Primitive::Use(UseSource {
                module: if no_gc {
                    BACKUP_ALLOCATOR_MODULE
                } else {
                    PRIMARY_ALLOCATOR_MODULE
                }
                .into(),
                location: loc.clone(),
            }),
        );

        imports.insert(
            0,
            Primitive::Use(UseSource {
                module: ARBITRARY_ALLOCATOR_MODULE.into(),
                location: loc.clone(),
            }),
        );
    }

    for import in imports.iter().cloned() {
        match import {
            Primitive::Use(UseSource {
                module, location, ..
            }) if !parsed_modules.borrow().contains(&module) => {
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
                        GREEN = get_GREEN!(),
                        RESET = get_RESET!(),
                    );
                }

                let location = (*location).clone();
                let current = current_dir().unwrap();

                if !is_std_import {
                    // Set the path to where the current file is so that imports are relative in that regard
                    set_current_dir(Path::new(final_path.parent().unwrap_or_else(|| {
                        elle_error!(location.basic_error(format!(
                            "Failed to get the parent directory of {final_path:#?}"
                        )))
                    })))
                    .unwrap_or_else(|err| {
                        elle_error!(location.basic_error(format!(
                            "Failed to set the current directory of {final_path:#?}\n{err}"
                        )))
                    });
                }

                let nodes = lex_and_parse(
                    &module,
                    Some(tree),
                    struct_pool,
                    parsed_modules,
                    warnings,
                    no_strings,
                    no_alloc,
                    no_gc,
                    no_fmt,
                    debug_time,
                    object_output,
                    expect_info,
                    nesting + 1,
                    if nesting == 0 {
                        Rc::new(location.clone())
                    } else {
                        import_location.clone()
                    },
                    string_module_methods,
                );

                if !is_std_import {
                    // Set the path back
                    set_current_dir(current.clone()).unwrap_or_else(|err| {
                        elle_error!(location.basic_error(format!(
                            "Failed to set the current directory to {current:#?}\n{err}"
                        )))
                    });
                }

                for symbol in nodes.iter().rev() {
                    match symbol.clone() {
                        Primitive::Use { .. } => {}
                        Primitive::Constant(ConstantSource { name, public, .. }) => {
                            override_and_add_node!(
                                Primitive::Constant,
                                &mut tree,
                                &name,
                                symbol,
                                public
                            );
                        }
                        Primitive::Function(FunctionSource { name, public, .. }) => {
                            override_and_add_node!(
                                Primitive::Function,
                                &mut tree,
                                &name,
                                symbol,
                                public
                            );
                        }
                        Primitive::Struct(StructSource { name, public, .. }) => {
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
                                Primitive::Function(FunctionSource { name, .. }) => name.clone(),
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
                        elapsed_with_color!(now.unwrap().elapsed()),
                        GREEN = get_GREEN!(),
                        RESET = get_RESET!(),
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
        tree.insert(
            0,
            Primitive::Constant(ConstantSource {
                name_token: Token::from_ident("nil"),
                name: "nil".into(),
                public: false,
                usable: true,
                imported: false,
                // void *
                r#type: Some(Type::Pointer(Box::new(Type::Void))),
                value: Box::new(AstNode::Literal(Literal {
                    kind: TokenKind::LongLiteral,
                    value: ValueKind::Number(0),
                    location: loc.clone(),
                    tagged: false,
                })),
                location: loc.clone(),
            }),
        );

        tree.insert(
            0,
            Primitive::Function(FunctionSource {
                namespace_token: Token::from_ident(get_POINTER_ID!()),
                name_token: Token::from_ident(LEN_CONSTANT),
                name: format!("{}.{LEN_CONSTANT}", get_POINTER_ID!()).into(),
                public: false,
                usable: true,
                imported: false,
                variadic: false,
                external: false,
                builtin: true,
                volatile: false,
                format: false,
                unaliased: None,
                generics: vec!["T".into()],
                arguments: vec![Argument {
                    name: "self".into(),
                    r#type: Type::Pointer(Box::new(Type::Unknown("T".into()))),
                    no_fmt: false,
                }],
                r#return: Some(Type::Word),
                body: vec![AstNode::Return(Return {
                    value: Box::new(AstNode::ArrayLength(ArrayLength {
                        value: Box::new(AstNode::Literal(Literal {
                            kind: TokenKind::Identifier,
                            value: ValueKind::String("self".into()),
                            location: loc.clone(),
                            tagged: false,
                        })),
                        location: loc.clone(),
                    })),
                    location: loc.clone(),
                })],
                location: loc.clone(),
                return_location: loc.clone(),
            }),
        );
    }

    tree.to_vec()
}

pub fn existing_definition(tree: &mut Vec<Primitive>, node_name: &str) -> Option<usize> {
    tree.iter().position(|item| match item {
        Primitive::Use { .. } => false,
        Primitive::Constant(ConstantSource { name, .. }) => name == &node_name,
        Primitive::Function(FunctionSource { name, .. }) => name == &node_name,
        Primitive::Struct(StructSource { name, .. }) => name == &node_name,
    })
}
