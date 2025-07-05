use std::collections::HashSet;
use std::env::{current_dir, set_current_dir};
use std::fs;
use std::path::Path;
use std::process::exit;
use std::time::Instant;
use std::{cell::RefCell, rc::Rc};

use string_interner::backend::BufferBackend;
use string_interner::symbol::SymbolU32;
use string_interner::StringInterner;

use crate::compiler::qbe::r#type::Type;
use crate::lexer::enums::{MutRc, Token};
use crate::parser::enums::{
    ArrayLength, ConstantSource, EnumSource, FunctionSource, Literal, Return, StructSource,
    UseSource,
};
use crate::parser::parser::EnumPool;
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

pub type Interner = StringInterner<BufferBackend>;

pub fn lex_and_parse(
    input_path: &String,
    existing_tree: Option<&mut Vec<Primitive>>,
    struct_pool: &RefCell<StructPool>,
    enum_pool: &RefCell<EnumPool>,
    parsed_modules: &RefCell<HashSet<SymbolU32>>,
    warnings: &Warnings,
    no_strings: bool,
    no_alloc: bool,
    no_gc: bool,
    no_fmt: bool,
    debug_time: bool,
    object_output: bool,
    expect_info: bool,
    nesting: usize,
    import_location: &MutRc<Location>,
    string_module_methods: &mut Vec<String>,
    interner: &mut Interner,
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

        let content = match fs::read_to_string(&final_path) {
            Ok(content) => content,
            Err(err) => {
                eprintln!(
                    "{}",
                    import_location.borrow().basic_error(format!(
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

    let sym = interner.get_or_intern(final_path.canonicalize().unwrap().to_string_lossy());

    if parsed_modules.borrow().contains(&sym) {
        if debug_time {
            println!(
                "{} Module '{GREEN}{input_path}{RESET}' is already imported.",
                if nesting > 0 {
                    "┆    ".repeat(nesting)
                } else {
                    String::new()
                },
                GREEN = get_GREEN!(),
                RESET = get_RESET!(),
            );
        }

        return vec![];
    }

    parsed_modules.borrow_mut().insert(sym);

    macro_rules! file_is_empty_error {
        () => {
            elle_error!(import_location.borrow().basic_error(format!(
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

    while let Some(token) = lexer.next_token() {
        // Give tokens an alt location so that this can be reported
        // instead, if the error happened in another file
        token.location.borrow_mut().alt_start = import_location.borrow().start;
        token.location.borrow_mut().alt_end = import_location.borrow().end;

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
        enum_pool.borrow().to_owned(),
        warnings.clone(),
    );

    let mut fallback = vec![];
    let mut tree = existing_tree.unwrap_or(&mut fallback);

    let (mut imports, new_struct_pool, new_enum_pool, ..) =
        parser.parse(&DoOnly::Imports, None, None);
    struct_pool.replace_with(|_| new_struct_pool);
    enum_pool.replace_with(|_| new_enum_pool);
    let loc = Rc::new(RefCell::new(Location::default(input_path.clone())));

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
            }) => {
                let now = if debug_time {
                    Some(Instant::now())
                } else {
                    None
                };

                if debug_time {
                    println!(
                        "{}╭― Importing module '{GREEN}{module}{RESET}'",
                        if nesting > 0 {
                            "┆    ".repeat(nesting)
                        } else {
                            String::new()
                        },
                        GREEN = get_GREEN!(),
                        RESET = get_RESET!(),
                    );
                }

                let current = current_dir().unwrap();

                if !is_std_import {
                    // Set the path to where the current file is so that imports are relative in that regard
                    set_current_dir(Path::new(final_path.parent().unwrap_or_else(|| {
                        elle_error!(location.borrow().basic_error(format!(
                            "Failed to get the parent directory of {}",
                            final_path.display()
                        )))
                    })))
                    .unwrap_or_else(|err| {
                        elle_error!(location.borrow().basic_error(format!(
                            "Failed to set the current directory of {}\n{err}",
                            final_path.display()
                        )))
                    });
                }

                let nodes = lex_and_parse(
                    &module,
                    Some(tree),
                    struct_pool,
                    enum_pool,
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
                        &location
                    } else {
                        import_location
                    },
                    string_module_methods,
                    interner,
                );

                if !is_std_import {
                    // Set the path back
                    set_current_dir(current.clone()).unwrap_or_else(|err| {
                        elle_error!(location.borrow().basic_error(format!(
                            "Failed to set the current directory to {}\n{err}",
                            current.display()
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
                        Primitive::Enum(EnumSource { name, public, .. }) => {
                            override_and_add_node!(
                                Primitive::Enum,
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
                        "{}╰> Imported '{GREEN}{module}{RESET}' in {}",
                        if nesting > 0 {
                            "┆    ".repeat(nesting)
                        } else {
                            String::new()
                        },
                        elapsed_with_color!(now.unwrap().elapsed()),
                        GREEN = get_GREEN!(),
                        RESET = get_RESET!(),
                    );
                }
            }
            _ => {}
        }
    }

    // Structs
    let (structs, new_struct_pool, new_enum_pool, ..) = parser.parse(
        &DoOnly::StructsAndEnums,
        Some(struct_pool.borrow().to_owned()),
        Some(enum_pool.borrow().to_owned()),
    );
    struct_pool.replace_with(|_| new_struct_pool);
    enum_pool.replace_with(|_| new_enum_pool);
    tree.extend(structs);

    let (others, new_struct_pool, new_enum_pool, ..) = parser.parse(
        &DoOnly::FunctionsAndConstants,
        Some(struct_pool.borrow().to_owned()),
        Some(enum_pool.borrow().to_owned()),
    );

    struct_pool.replace_with(|_| new_struct_pool);
    enum_pool.replace_with(|_| new_enum_pool);
    tree.extend(others);

    // Add global constants
    // - nil => 0 (nullptr)
    if nesting == 0 {
        tree.insert(
            0,
            Primitive::Constant(ConstantSource {
                namespace_token: Token::from_ident(""),
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
    }

    tree.clone()
}

pub fn existing_definition(tree: &[Primitive], node_name: &str) -> Option<usize> {
    tree.iter().position(|item| match item {
        Primitive::Use { .. } => false,
        Primitive::Constant(ConstantSource { name, .. })
        | Primitive::Function(FunctionSource { name, .. })
        | Primitive::Struct(StructSource { name, .. })
        | Primitive::Enum(EnumSource { name, .. }) => *name == node_name,
    })
}
