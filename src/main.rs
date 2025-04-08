#![warn(clippy::all, clippy::restriction, clippy::pedantic)]
use std::collections::{HashMap, HashSet};
use std::env;
use std::path::Path;
use std::process::{exit, ExitCode};
use std::rc::Rc;
use std::time::Instant;
use std::{cell::RefCell, fs};

mod compiler;
mod lexer;
mod misc;
mod parser;

use compiler::compiler::Compiler;
use lexer::enums::{Location, TokenKind, ValueKind};
use misc::{build::build, colors::*, constants::*, help::print_help, modules::lex_and_parse};
use parser::enums::{Argument, AstNode, Primitive};

use crate::compiler::qbe::r#type::Type;
use crate::parser::enums::{
    Address, BinaryOperation, Declare, Environment, FieldAccess, FunctionCall, FunctionSource,
    Literal, MemoryOperation, Return, SetAllocator, StructLiteral, StructSource,
    WhileLoopStatement,
};

pub enum Warning {
    StructFieldsMissing = 1 << 0,
    InvalidAlias = 1 << 1,
    VariadicNoMeta = 1 << 2,
    CStyleVoid = 1 << 3,
    AllocatorMethodsMissing = 1 << 4,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum EmitKind {
    Executable(String),
    Object(String),
    QbeFile(String),
    AsmFile(String),
    None,
}

impl Warning {
    pub const fn all() -> u32 {
        Self::InvalidAlias as u32
            | Self::StructFieldsMissing as u32
            | Self::VariadicNoMeta as u32
            | Self::CStyleVoid as u32
            | Self::AllocatorMethodsMissing as u32
    }
}

#[derive(Clone)]
struct Warnings {
    flags: u32,
}

impl Warnings {
    fn new() -> Self {
        Warnings { flags: 0 }
    }

    fn set_warning(&mut self, warning: Warning) {
        self.flags |= warning as u32;
    }

    fn has_warning(&self, warning: Warning) -> bool {
        (self.flags & (warning as u32)) != 0
    }

    fn set_all(&mut self) {
        self.flags = Warning::all();
    }
}

fn main() -> ExitCode {
    let mut args = env::args().peekable();
    let program = args.next().expect("program");

    if env::var("NO_COLOR").is_ok_and(|x| !x.is_empty()) {
        disable_colors!()
    }

    if args.peek().is_none() {
        print_help(program);
        exit(0);
    }

    macro_rules! set_with_home {
        ($x:ident, $y:expr) => {{
            let leaked: &'static mut str = Box::leak(
                format!(
                    "{}/{}",
                    env::var("HOME").expect("Failed to get $HOME path"),
                    $y
                )
                .into_boxed_str(),
            );
            unsafe { $x = Some(leaked) };
        }};
    }

    set_with_home!(STD_LIB_PATH, get_STD_LIB_PATH!());
    set_with_home!(RUNTIME_PATH, get_RUNTIME_PATH!());

    let mut input_path = None;
    let mut output_path = None;

    let mut warnings = Warnings::new();

    let mut debug_time = false;
    let mut emit_qbe = false;
    let mut emit_asm = false;
    let mut ast = false;
    let mut hush = false;
    let mut object_output = false;
    let mut no_strings = false; // no string module
    let mut no_std = false; // no stdlib
    let mut no_alloc = false; // no arbitrary allocator
    let mut no_gc = false; // no gc
    let mut no_fmt = false; // no primitive fmt methods
    let mut pedantic = false; // extra checks in type conversions
    let mut object_files: Vec<String> = vec![];

    let mut linker_flags = vec![];
    let mut linker_path = "cc".into();
    let mut qbe_path = "qbe".into();

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "-Wstruct-fields-missing" => warnings.set_warning(Warning::StructFieldsMissing),
            "-Winvalid-alias" => warnings.set_warning(Warning::InvalidAlias),
            "-Wvariadic-no-meta" => warnings.set_warning(Warning::VariadicNoMeta),
            "-Wc-style-void" => warnings.set_warning(Warning::CStyleVoid),
            "-Wall" => warnings.set_all(),
            "-t" | "--time" | "--elapsed-time" => debug_time = true,
            "--ssa" | "--emit-ssa" | "--emit-qbe" => emit_qbe = true,
            "--asm" | "--emit-s" | "--emit-asm" => emit_asm = true,
            "--ast" | "--emit-ast" | "--emit-tree" => ast = true,
            "-p" | "--pedantic" => pedantic = true,
            "-o" => output_path = args.next(),
            "-h" | "--help" => {
                print_help(program);
                exit(0);
            }
            "-c" | "--compile-only" => {
                object_output = true;
            }
            "-z" | "--link-flag" => linker_flags.push(args.next()),
            "-Z" | "--link-path" => linker_path = args.next().unwrap_or("cc".into()),
            "-Q" | "--qbe-path" => qbe_path = args.next().unwrap_or("qbe".into()),
            "-S" | "--std-path" => {
                if let Some(arg) = args.next() {
                    let leaked: &'static mut str = Box::leak(arg.into_boxed_str());
                    unsafe { STD_LIB_PATH = Some(leaked) };
                }
            }
            "-R" | "--runtime-path" => {
                if let Some(arg) = args.next() {
                    let leaked: &'static mut str = Box::leak(arg.into_boxed_str());
                    unsafe { RUNTIME_PATH = Some(leaked) };
                }
            }
            "--cpfmt" | "--clean-ptr-fmt" => unsafe { POINTER_ID = Some("__clean_ptr__") },
            "--hush" | "--silent" => {
                hush = true;
            }
            "--nosm" | "--no-string-module" => no_strings = true,
            "--noalloc" | "--no-allocation" => no_alloc = true,
            "--nogc" | "--no-garbage-collector" => no_gc = true,
            "--nostd" | "--no-stdlib" => no_std = true,
            "--nofmt" | "--no-primitive-formatters" => no_fmt = true,
            "--noclr" | "--no-ansi" => disable_colors!(),
            "-v" | "--version" => {
                println!(
                    "{} {GREEN}{}{RESET} ({GREEN}{}{RESET})\nbuilt on {GREEN}{}{RESET}\nvia rustc {GREEN}{}{RESET}",
                    env!("CARGO_BIN_NAME"),
                    env!("CARGO_PKG_VERSION"),
                    env!("GIT_HASH"),
                    env!("BUILD_DATE"),
                    env!("RUSTC_VERSION"),
                    GREEN = get_GREEN!(),
                    RESET = get_RESET!()
                );

                exit(0);
            }
            other if other.ends_with(SHORT_EXTENSION) => {
                if input_path.is_none() {
                    input_path = Some(other.to_string())
                }
            }
            other if other.ends_with(OBJECT_EXTENSION) => object_files.push(other.into()),
            other => {
                elle_error!(Location::base().basic_error(format!(
                    "{title}\n{help}\n{usage}\n{info}\n{extensions}",
                    title = format!(
                        "An invalid argument was provided: {RED}{other}{RESET}\n",
                        RED = get_RED!(),
                        RESET = get_RESET!()
                    ),
                    help = format!("For help, please use the following command:"),
                    usage = format!(
                        "{}{GREEN}{program} [-h | --help]{RESET}\n",
                        " ".repeat(4),
                        GREEN = get_GREEN!(),
                        RESET = get_RESET!()
                    ),
                    info = format!("If this is a file, please include its file extension."),
                    extensions = format!(
                        "Possible extensions include: {GREEN}{FILE_EXTENSIONS:?}{RESET}",
                        GREEN = get_GREEN!(),
                        RESET = get_RESET!()
                    ),
                )))
            }
        }
    }

    if emit_qbe && emit_asm {
        elle_error!(Location::base().basic_error(format!(
            "{}Cannot generate both assembly and QBE.",
            get_RED!()
        )))
    }

    let now = if debug_time {
        Some(Instant::now())
    } else {
        None
    };
    let mut pool = HashMap::new();
    let default_allocator = if no_gc {
        BACKUP_ALLOCATOR_NAME
    } else {
        PRIMARY_ALOCATOR_NAME
    };

    let meta_members = vec![
        // Holds an array of expressions passed into the function in plain text
        Argument {
            name: "exprs".into(),
            // string[]
            r#type: Type::Pointer(Box::new(Type::Pointer(Box::new(Type::Char)))),
            no_fmt: false,
        },
        // Holds an array of the type of arguments passed into the function as strings
        Argument {
            name: "types".into(),
            // string[]
            r#type: Type::Pointer(Box::new(Type::Pointer(Box::new(Type::Char)))),
            no_fmt: false,
        },
        // Holds the number of arguments that were passed into a function
        Argument {
            name: "arity".into(),
            // i32
            r#type: Type::Word,
            no_fmt: false,
        },
        // Holds the name of the caller method as a string
        Argument {
            name: "caller".into(),
            // string
            r#type: Type::Pointer(Box::new(Type::Char)),
            no_fmt: false,
        },
        // The name of the file that the struct was generated in
        Argument {
            name: "file".into(),
            // string
            r#type: Type::Pointer(Box::new(Type::Char)),
            no_fmt: false,
        },
        // The line number that the struct was generated on
        Argument {
            name: "line".into(),
            // i32
            r#type: Type::Word,
            no_fmt: false,
        },
        // The column number that the struct was generated on
        Argument {
            name: "column".into(),
            // i32
            r#type: Type::Word,
            no_fmt: false,
        },
    ];

    let env_members = vec![
        // The pointer to the current allocator
        Argument {
            name: "allocator".into(),
            r#type: Type::Pointer(Box::new(Type::Struct(ARBITRARY_ALLOCATOR_NAME.into()))),
            no_fmt: false,
        },
        // The pointer to the default allocator
        Argument {
            name: "default_allocator".into(),
            r#type: Type::Pointer(Box::new(Type::Struct(default_allocator.into()))),
            no_fmt: false,
        },
        // An approximation of the top of the stack
        Argument {
            name: "stack_top".into(),
            r#type: Type::Pointer(Box::new(Type::Void)),
            no_fmt: false,
        },
    ];

    let input_path = if let Some(input_path) = input_path {
        input_path
    } else {
        eprintln!("ERROR: no input is provided");
        eprintln!("Usage: {program} <main.l | main.elle>");
        return ExitCode::FAILURE;
    };

    let loc = Rc::new(Location::default(input_path.clone()));

    pool.insert(
        META_STRUCT_NAME.into(),
        (vec![], meta_members.clone(), loc.clone()),
    );

    pool.insert(
        ENV_STRUCT_NAME.into(),
        (vec![], env_members.clone(), loc.clone()),
    );

    let struct_pool = RefCell::new(pool);
    let parsed_modules = RefCell::new(HashSet::new());
    let mut string_module_methods = vec![];

    let mut tree = lex_and_parse(
        &input_path,
        None,
        &struct_pool,
        &parsed_modules,
        &warnings,
        no_strings,
        no_alloc,
        no_gc,
        no_fmt,
        debug_time,
        object_output,
        0,
        loc.clone(),
        &mut string_module_methods,
    );

    tree.insert(
        0,
        Primitive::Struct(StructSource {
            name: META_STRUCT_NAME.into(),
            public: false,
            usable: true,
            imported: false,
            generics: vec![],
            known_generics: hashmap![],
            members: meta_members.clone(),
            keyword_location: loc.clone(),
            location: loc.clone(),
            ignore_empty: false,
        }),
    );

    tree.insert(
        0,
        Primitive::Struct(StructSource {
            name: ENV_STRUCT_NAME.into(),
            public: false,
            usable: true,
            imported: false,
            generics: vec![],
            known_generics: hashmap![],
            members: env_members.clone(),
            keyword_location: loc.clone(),
            location: loc.clone(),
            ignore_empty: false,
        }),
    );

    if !object_output && !no_alloc {
        // Rename main to an internal main
        let mut main_arg_len = 0;
        tree.iter_mut()
            .find(|x| match x {
                Primitive::Function(FunctionSource { name, .. }) if name == "main" => true,
                _ => false,
            })
            .map(|x| match x {
                Primitive::Function(FunctionSource {
                    name, arguments, location, ..
                }) if name == "main" => {
                    *name = get_MAIN_ID!().into();
                    main_arg_len = arguments.len();

                    if main_arg_len > 1 {
                        panic!(
                            "{}",
                            location.error(format!(
                                "You cannot expect more than 1 argument ({RED}{main_arg_len}{RESET}) in the main function.\nOnly a single argument is supplied of type \"{GREEN}string[]{RESET}\".",
                                RED = get_RED!(),
                                GREEN = get_GREEN!(),
                                RESET = get_RESET!()
                            ))
                        )
                    }

                    if main_arg_len == 1 &&
                        arguments[0].r#type != Type::Pointer(Box::new(
                            Type::Struct(format!("Array.{GENERIC_IDENTIFIER}.{}.{GENERIC_END}",
                                Type::Pointer(Box::new(Type::Char)).to_internal_id()
                            ))
                        ))
                    {
                        panic!(
                            "{}",
                            location.error(
                                format!(
                                    "Mismatched type for argument in main function.\nExpected type \"{GREEN}string[]{RESET}\" but got \"{GREEN}{}{RESET}\".",
                                    arguments[0].r#type.display(),
                                    GREEN = get_GREEN!(),
                                    RESET = get_RESET!()
                                )
                            )
                        )
                    }
                }
                _ => {}
            });

        // Define a custom main
        tree.push(Primitive::Function(FunctionSource {
            name: "main".into(),
            public: true,
            usable: true,
            imported: false,
            variadic: false,
            external: false,
            builtin: true,
            volatile: true,
            format: false,
            unaliased: None,
            generics: vec![],
            arguments: vec![
                Argument {
                    name: "argc".into(),
                    r#type: Type::Word,
                    no_fmt: false,
                },
                Argument {
                    name: "argv".into(),
                    r#type: Type::Pointer(Box::new(Type::Pointer(Box::new(Type::Char)))),
                    no_fmt: false,
                },
            ],
            r#return: Some(Type::Word),
            body: [
                vec![
                    AstNode::Declare(Declare {
                        name: "stack_top".into(),
                        r#type: Some(Type::Pointer(Box::new(Type::Void))),
                        value: Some(Box::new(AstNode::Address(Address {
                            value: Box::new(AstNode::Literal(Literal {
                                kind: TokenKind::IntegerLiteral,
                                value: ValueKind::Number(0),
                                location: loc.clone(),
                            })),
                            location: loc.clone(),
                        }))),
                        location: loc.clone(),
                        value_location: loc.clone(),
                    }),
                    AstNode::Declare(Declare {
                        name: "env".into(),
                        r#type: Some(Type::Infer),
                        value: Some(Box::new(AstNode::StructLiteral(StructLiteral {
                            name: ENV_STRUCT_NAME.into(),
                            values: vec![
                                (
                                    "allocator".into(),
                                    Box::new(AstNode::FunctionCall(FunctionCall {
                                        name: format!("{ARBITRARY_ALLOCATOR_NAME}.new"),
                                        generics: vec![],
                                        parameters: vec![],
                                        type_method: false,
                                        ignore_no_def: false,
                                        location: loc.clone(),
                                    })),
                                ),
                                (
                                    "default_allocator".into(),
                                    Box::new(AstNode::FunctionCall(FunctionCall {
                                        name: format!("{default_allocator}.new"),
                                        generics: vec![],
                                        parameters: vec![],
                                        type_method: false,
                                        ignore_no_def: false,
                                        location: loc.clone(),
                                    })),
                                ),
                                (
                                    "stack_top".into(),
                                    Box::new(AstNode::Literal(Literal {
                                        kind: TokenKind::Identifier,
                                        value: ValueKind::String("stack_top".into()),
                                        location: loc.clone(),
                                    })),
                                ),
                            ],
                            location: loc.clone(),
                        }))),
                        location: loc.clone(),
                        value_location: loc.clone(),
                    }),
                    AstNode::Environment(Environment {
                        value: Some(Box::new(AstNode::Address(Address {
                            value: Box::new(AstNode::Literal(Literal {
                                kind: TokenKind::Identifier,
                                value: ValueKind::String("env".into()),
                                location: loc.clone(),
                            })),
                            location: loc.clone(),
                        }))),
                        location: loc.clone(),
                    }),
                    AstNode::SetAllocator(SetAllocator {
                        value: Box::new(AstNode::FieldAccess(FieldAccess {
                            left: Box::new(AstNode::Literal(Literal {
                                kind: TokenKind::Identifier,
                                value: ValueKind::String("env".into()),
                                location: loc.clone(),
                            })),
                            right: Box::new(AstNode::Literal(Literal {
                                kind: TokenKind::Identifier,
                                value: ValueKind::String("default_allocator".into()),
                                location: loc.clone(),
                            })),
                            value: None,
                            location: loc.clone(),
                        })),
                        location: loc.clone(),
                    }),
                ],
                if main_arg_len == 1 {
                    vec![
                        AstNode::Declare(Declare {
                            name: "args".into(),
                            r#type: Some(Type::Infer),
                            value: Some(Box::new(AstNode::FunctionCall(FunctionCall {
                                name: "Array.with_capacity".into(),
                                generics: vec![Type::Pointer(Box::new(Type::Char))],
                                parameters: vec![(
                                    loc.clone(),
                                    AstNode::Literal(Literal {
                                        kind: TokenKind::Identifier,
                                        value: ValueKind::String("argc".into()),
                                        location: loc.clone(),
                                    }),
                                )],
                                type_method: false,
                                ignore_no_def: false,
                                location: loc.clone(),
                            }))),
                            location: loc.clone(),
                            value_location: loc.clone(),
                        }),
                        AstNode::Declare(Declare {
                            name: "i".into(),
                            r#type: Some(Type::Word),
                            value: Some(Box::new(AstNode::Literal(Literal {
                                kind: TokenKind::IntegerLiteral,
                                value: ValueKind::Number(0),
                                location: loc.clone(),
                            }))),
                            location: loc.clone(),
                            value_location: loc.clone(),
                        }),
                        AstNode::WhileLoopStatement(WhileLoopStatement {
                            condition: Box::new(AstNode::BinaryOperation(BinaryOperation {
                                left: Box::new(AstNode::Literal(Literal {
                                    kind: TokenKind::Identifier,
                                    value: ValueKind::String("i".into()),
                                    location: loc.clone(),
                                })),
                                right: Box::new(AstNode::Literal(Literal {
                                    kind: TokenKind::Identifier,
                                    value: ValueKind::String("argc".into()),
                                    location: loc.clone(),
                                })),
                                operator: TokenKind::LessThan,
                                treat_as_string: false,
                                dunder_methods: false,
                                location: loc.clone(),
                            })),
                            step: Some(Box::new(AstNode::Declare(Declare {
                                name: "i".into(),
                                r#type: None,
                                value: Some(Box::new(AstNode::BinaryOperation(BinaryOperation {
                                    left: Box::new(AstNode::Literal(Literal {
                                        kind: TokenKind::Identifier,
                                        value: ValueKind::String("i".into()),
                                        location: loc.clone(),
                                    })),
                                    right: Box::new(AstNode::Literal(Literal {
                                        kind: TokenKind::IntegerLiteral,
                                        value: ValueKind::Number(1),
                                        location: loc.clone(),
                                    })),
                                    operator: TokenKind::Add,
                                    treat_as_string: false,
                                    dunder_methods: false,
                                    location: loc.clone(),
                                }))),
                                location: loc.clone(),
                                value_location: loc.clone(),
                            }))),
                            body: vec![AstNode::FunctionCall(FunctionCall {
                                name: "push".into(),
                                generics: vec![],
                                parameters: vec![
                                    (
                                        loc.clone(),
                                        AstNode::Literal(Literal {
                                            kind: TokenKind::Identifier,
                                            value: ValueKind::String("args".into()),
                                            location: loc.clone(),
                                        }),
                                    ),
                                    (
                                        loc.clone(),
                                        AstNode::MemoryOperation(MemoryOperation {
                                            left: Box::new(AstNode::Literal(Literal {
                                                kind: TokenKind::Identifier,
                                                value: ValueKind::String("argv".into()),
                                                location: loc.clone(),
                                            })),
                                            right: Box::new(AstNode::Literal(Literal {
                                                kind: TokenKind::Identifier,
                                                value: ValueKind::String("i".into()),
                                                location: loc.clone(),
                                            })),
                                            value: None,
                                            left_location: loc.clone(),
                                            right_location: loc.clone(),
                                            value_location: loc.clone(),
                                            is_deref: false,
                                        }),
                                    ),
                                ],
                                type_method: true,
                                ignore_no_def: false,
                                location: loc.clone(),
                            })],
                            location: loc.clone(),
                        }),
                    ]
                } else {
                    vec![]
                },
                vec![
                    AstNode::Declare(Declare {
                        name: "status".into(),
                        r#type: Some(Type::Word),
                        value: Some(Box::new(AstNode::FunctionCall(FunctionCall {
                            name: get_MAIN_ID!().into(),
                            generics: vec![],
                            parameters: if main_arg_len == 1 {
                                vec![(
                                    loc.clone(),
                                    AstNode::Literal(Literal {
                                        kind: TokenKind::Identifier,
                                        value: ValueKind::String("args".into()),
                                        location: loc.clone(),
                                    }),
                                )]
                            } else {
                                vec![]
                            },
                            type_method: false,
                            ignore_no_def: false,
                            location: loc.clone(),
                        }))),
                        location: loc.clone(),
                        value_location: loc.clone(),
                    }),
                    AstNode::FunctionCall(FunctionCall {
                        name: "free_self".into(),
                        generics: vec![],
                        parameters: vec![(
                            loc.clone(),
                            AstNode::FieldAccess(FieldAccess {
                                left: Box::new(AstNode::Literal(Literal {
                                    kind: TokenKind::Identifier,
                                    value: ValueKind::String("env".into()),
                                    location: loc.clone(),
                                })),
                                right: Box::new(AstNode::Literal(Literal {
                                    kind: TokenKind::Identifier,
                                    value: ValueKind::String("allocator".into()),
                                    location: loc.clone(),
                                })),
                                value: None,
                                location: loc.clone(),
                            }),
                        )],
                        type_method: true,
                        ignore_no_def: false,
                        location: loc.clone(),
                    }),
                    AstNode::Return(Return {
                        value: Box::new(AstNode::Literal(Literal {
                            kind: TokenKind::Identifier,
                            value: ValueKind::String("status".into()),
                            location: loc.clone(),
                        })),
                        location: loc.clone(),
                    }),
                ],
            ]
            .concat()
            .into_iter()
            .collect(),
            location: loc.clone(),
            return_location: loc.clone(),
        }));
    } else {
        unsafe { MAIN_ID = Some("main") };
    }

    if ast {
        dbg!(tree);
        return ExitCode::SUCCESS;
    }

    if debug_time {
        println!(
            "\n✦ Tokenization and parsing took {}",
            elapsed_with_color!(now.unwrap().elapsed())
        );
    }

    // #[cfg(debug_assertions)]
    // dbg!(&tree);

    let now = if debug_time {
        Some(Instant::now())
    } else {
        None
    };

    let build_path = format!(
        "./.build-{}",
        Path::new(&input_path)
            .file_stem()
            .unwrap()
            .to_str()
            .unwrap()
            .to_string()
    );

    // Set the build path so it can be deleted during an error
    let leaked: &'static mut str = Box::leak(build_path.into_boxed_str());
    unsafe { BUILD_PATH = Some(leaked) };

    fs::create_dir_all(get_BUILD_PATH!()).expect("Failed to create ./.build.");
    let path_to_qbe_dist = format!("{}/target.ssa", get_BUILD_PATH!());

    Compiler::compile(
        tree,
        path_to_qbe_dist.clone(),
        warnings,
        object_output,
        pedantic,
        no_gc,
        string_module_methods,
    );

    if debug_time {
        println!(
            "✦ Compilation took {}\n",
            elapsed_with_color!(now.unwrap().elapsed())
        );
    }

    let parsed_output_path = if let Some(output_path) = output_path {
        output_path
    } else {
        let tmp = Path::new(&input_path).file_stem().unwrap();
        tmp.to_str().unwrap().into()
    };

    let out;

    if emit_qbe {
        let path = Path::new(&parsed_output_path).with_extension("ssa");
        fs::rename(path_to_qbe_dist, path.clone()).unwrap();

        out = EmitKind::QbeFile(path.to_str().unwrap().to_string());
    } else {
        let result = build(
            qbe_path,
            path_to_qbe_dist,
            parsed_output_path,
            emit_asm,
            object_output,
            linker_flags,
            linker_path,
            object_files,
            no_std,
        );

        out = result;
    }

    fs::remove_dir_all(get_BUILD_PATH!()).expect("Failed to delete ./.build.");

    if out != EmitKind::None {
        if !hush {
            println!(
                "{GREEN}Finished compiling '{path}' successfully! ヽ(•ᴗ•)ﾉ{RESET}",
                path = input_path.split("/").last().unwrap(),
                GREEN = get_GREEN!(),
                RESET = get_RESET!()
            );
        }

        ExitCode::SUCCESS
    } else {
        if !hush {
            println!(
                "{RED}Compilation of '{path}' finished with errors. (っ◞‸◟ c){RESET}",
                path = input_path.split("/").last().unwrap(),
                RED = get_RED!(),
                RESET = get_RESET!()
            );
        }

        ExitCode::FAILURE
    }
}
