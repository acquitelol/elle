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
use compiler::enums::Type;
use lexer::enums::{Location, TokenKind, ValueKind};
use misc::{build::build, colors::*, help::print_help, modules::lex_and_parse};
use parser::enums::{Argument, AstNode, Primitive};

static META_STRUCT_NAME: &str = "ElleMeta";
static ENV_STRUCT_NAME: &str = "ElleEnv";
static PRIMARY_ALOCATOR_NAME: &str = "GCAllocator";
static PRIMARY_ALLOCATOR_MODULE: &str = "std/allocators/gc";
static GENERIC_IDENTIFIER: &str = "0"; // Start of a generic
static GENERIC_END: &str = "1"; // Allowing for nested generic structs
static GENERIC_POINTER: &str = "2"; // Pointer to another type
static GENERIC_UNKNOWN: &str = "3"; // Unknown type T
static STD_LIB_PATH: &str = "/usr/local/include/elle";
static RUNTIME_PATH: &str = "/usr/local/lib";
static LONG_EXTENSION: &str = ".elle";
static SHORT_EXTENSION: &str = ".le";
static OBJECT_EXTENSION: &str = ".o";
static VOID_POINTER_ID: &str = "__void_ptr__";
static POINTER_ID: &str = "__ptr__";
static ENV_ID: &str = "__internal.elle.__env__";
static MAIN_ID: &str = "__internal.elle.__main__";
static GC_NOOP: &str = "__internal_gc_noop";
static FORMAT_CONSTANT: &str = "__fmt__";
static LOAD_CONSTANT: &str = "__load__";
static STORE_CONSTANT: &str = "__store__";
static LEN_CONSTANT: &str = "__len__";
static HASH_CONSTANT: &str = "__hash__";
static EQUALS_CONSTANT: &str = "__equals__";
static INTERNAL_FORMATTER: &str =
    "__internal_formatter_do_not_use_unless_you_know_what_youre_doing__";
static DUNDER_CONSTANTS: &[&'static str] = &[
    FORMAT_CONSTANT,
    LOAD_CONSTANT,
    STORE_CONSTANT,
    LEN_CONSTANT,
    HASH_CONSTANT,
    EQUALS_CONSTANT,
];
static PTR_PRIORITY_CONSTANTS: &[&'static str] = &[FORMAT_CONSTANT];
static RESERVED_KEYWORDS: &[&'static str] = &[
    "as", "mut", "enum", "match", "static", "super", "do", "macro", "step", "of", "class", "var",
    "impl",
];
static VA_LIST_SIZE_BYTES: usize = 32;

#[macro_export]
macro_rules! INTERNAL_IDX_FORMAT {
    () => {
        "__internal_{}_idx"
    };
}

#[macro_export]
macro_rules! INTERNAL_ITERATOR_FORMAT {
    () => {
        "__internal_{}_iterator"
    };
}

pub enum Warning {
    StructFieldsMissing = 1 << 0,
    InvalidAlias = 1 << 1,
    VariadicNoMeta = 1 << 2,
    CStyleVoid = 1 << 3,
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

    if args.peek().is_none() {
        print_help(program);
        exit(0);
    }

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
    let mut no_gc = false; // no gc
    let mut no_fmt = false; // no primitive fmt methods
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
            "-ssa" | "--emit-ssa" | "--emit-qbe" => emit_qbe = true,
            "-asm" | "--emit-s" | "--emit-asm" => emit_asm = true,
            "-ast" | "--emit-ast" | "--emit-tree" => ast = true,
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
            "--hush" | "--silent" => {
                hush = true;
            }
            "-nosm" | "--no-string-module" => {
                no_strings = true;
            }
            "-nogc" | "--no-garbage-collector" => no_gc = true,
            "-nostd" | "--no-stdlib" => no_std = true,
            "-nofmt" | "--no-primitive-formatters" => no_fmt = true,
            other if other.ends_with(SHORT_EXTENSION) || other.ends_with(LONG_EXTENSION) => {
                if input_path.is_none() {
                    input_path = Some(other.to_string())
                }
            }
            other if other.ends_with(OBJECT_EXTENSION) => object_files.push(other.into()),
            other => {
                println!("{RED}Invalid argument: {}", other);
                println!("For help, please use the following command:");
                println!("\n{program} [-h | --help]\n");
                println!("If this is a file, please include its file extension.{RESET}",);

                exit(1);
            }
        }
    }

    if emit_qbe && emit_asm {
        panic!("{RED}Cannot generate both assembly and QBE.")
    }

    let now = if debug_time {
        Some(Instant::now())
    } else {
        None
    };
    let mut pool = HashMap::new();

    let meta_members = vec![
        // Holds an array of expressions passed into the function in plain text
        Argument {
            name: "exprs".into(),
            // string[]
            r#type: Type::Pointer(Box::new(Type::Pointer(Box::new(Type::Char)))),
            manual: false,
        },
        // Holds an array of the type of arguments passed into the function as strings
        Argument {
            name: "types".into(),
            // string[]
            r#type: Type::Pointer(Box::new(Type::Pointer(Box::new(Type::Char)))),
            manual: false,
        },
        // Holds the number of arguments that were passed into a function
        Argument {
            name: "arity".into(),
            // i32
            r#type: Type::Word,
            manual: false,
        },
        // Holds the name of the caller method as a string
        Argument {
            name: "caller".into(),
            // string
            r#type: Type::Pointer(Box::new(Type::Char)),
            manual: false,
        },
        // The name of the file that the struct was generated in
        Argument {
            name: "file".into(),
            // string
            r#type: Type::Pointer(Box::new(Type::Char)),
            manual: false,
        },
        // The line number that the struct was generated on
        Argument {
            name: "line".into(),
            // i32
            r#type: Type::Word,
            manual: false,
        },
        // The column number that the struct was generated on
        Argument {
            name: "column".into(),
            // i32
            r#type: Type::Word,
            manual: false,
        },
    ];

    let env_members = vec![
        // The pointer to the region allocator
        Argument {
            name: "allocator".into(),
            // Region *
            r#type: Type::Pointer(Box::new(Type::Struct(PRIMARY_ALOCATOR_NAME.into()))),
            manual: false,
        },
        // An approximation of the top of the stack
        Argument {
            name: "stack_top".into(),
            r#type: Type::Pointer(Box::new(Type::Void)),
            manual: false,
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
        Primitive::Struct {
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
        },
    );

    tree.insert(
        0,
        Primitive::Struct {
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
        },
    );

    if !object_output {
        // Rename main to an internal main
        let mut main_arg_len = 0;
        tree.iter_mut()
            .find(|x| match x {
                Primitive::Function { name, .. } if name == "main" => true,
                _ => false,
            })
            .map(|x| match x {
                Primitive::Function {
                    name, arguments, location, ..
                } if name == "main" => {
                    *name = MAIN_ID.into();
                    main_arg_len = arguments.len();

                    if main_arg_len > 1 {
                        panic!(
                            "{}",
                            location.error(format!("You cannot expect more than 1 argument ({RED}{main_arg_len}{RESET}) in the main function.\nOnly a single argument is supplied of type \"{GREEN}string[]{RESET}\"."))
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
                                    arguments[0].r#type.display()
                                )
                            )
                        )
                    }
                }
                _ => {}
            });

        // Define a custom main
        tree.push(Primitive::Function {
            name: "main".into(),
            public: true,
            usable: true,
            imported: false,
            variadic: false,
            manual: false,
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
                    manual: false,
                },
                Argument {
                    name: "argv".into(),
                    r#type: Type::Pointer(Box::new(Type::Pointer(Box::new(Type::Char)))),
                    manual: false,
                },
            ],
            r#return: Some(Type::Word),
            body: [
                vec![
                    AstNode::Declare {
                        name: "stack_top".into(),
                        r#type: Some(Type::Pointer(Box::new(Type::Void))),
                        value: Some(Box::new(AstNode::Address {
                            value: Box::new(AstNode::Literal {
                                kind: TokenKind::IntegerLiteral,
                                value: ValueKind::Number(0),
                                location: loc.clone(),
                            }),
                            location: loc.clone(),
                        })),
                        location: loc.clone(),
                        value_location: loc.clone(),
                    },
                    AstNode::Declare {
                        name: "env".into(),
                        r#type: Some(Type::Infer),
                        value: Some(Box::new(AstNode::StructLiteral {
                            name: ENV_STRUCT_NAME.into(),
                            values: vec![
                                (
                                    "allocator".into(),
                                    Box::new(AstNode::FunctionCall {
                                        name: format!("{PRIMARY_ALOCATOR_NAME}.new"),
                                        generics: vec![],
                                        parameters: vec![],
                                        type_method: false,
                                        ignore_no_def: false,
                                        location: loc.clone(),
                                    }),
                                ),
                                (
                                    "stack_top".into(),
                                    Box::new(AstNode::Literal {
                                        kind: TokenKind::Identifier,
                                        value: ValueKind::String("stack_top".into()),
                                        location: loc.clone(),
                                    }),
                                ),
                            ],
                            location: loc.clone(),
                        })),
                        location: loc.clone(),
                        value_location: loc.clone(),
                    },
                    AstNode::Environment {
                        value: Some(Box::new(AstNode::Address {
                            value: Box::new(AstNode::Literal {
                                kind: TokenKind::Identifier,
                                value: ValueKind::String("env".into()),
                                location: loc.clone(),
                            }),
                            location: loc.clone(),
                        })),
                        location: loc.clone(),
                    },
                ],
                if main_arg_len == 1 {
                    vec![
                        AstNode::Declare {
                            name: "args".into(),
                            r#type: Some(Type::Infer),
                            value: Some(Box::new(AstNode::FunctionCall {
                                name: "Array.with_capacity".into(),
                                generics: vec![Type::Pointer(Box::new(Type::Char))],
                                parameters: vec![(
                                    loc.clone(),
                                    AstNode::Literal {
                                        kind: TokenKind::Identifier,
                                        value: ValueKind::String("argc".into()),
                                        location: loc.clone(),
                                    },
                                )],
                                type_method: false,
                                ignore_no_def: false,
                                location: loc.clone(),
                            })),
                            location: loc.clone(),
                            value_location: loc.clone(),
                        },
                        AstNode::Declare {
                            name: "i".into(),
                            r#type: Some(Type::Word),
                            value: Some(Box::new(AstNode::Literal {
                                kind: TokenKind::IntegerLiteral,
                                value: ValueKind::Number(0),
                                location: loc.clone(),
                            })),
                            location: loc.clone(),
                            value_location: loc.clone(),
                        },
                        AstNode::WhileLoopStatement {
                            condition: Box::new(AstNode::BinaryOperation {
                                left: Box::new(AstNode::Literal {
                                    kind: TokenKind::Identifier,
                                    value: ValueKind::String("i".into()),
                                    location: loc.clone(),
                                }),
                                right: Box::new(AstNode::Literal {
                                    kind: TokenKind::Identifier,
                                    value: ValueKind::String("argc".into()),
                                    location: loc.clone(),
                                }),
                                operator: TokenKind::LessThan,
                                treat_as_string: false,
                                dunder_methods: false,
                                location: loc.clone(),
                            }),
                            step: Some(Box::new(AstNode::Declare {
                                name: "i".into(),
                                r#type: None,
                                value: Some(Box::new(AstNode::BinaryOperation {
                                    left: Box::new(AstNode::Literal {
                                        kind: TokenKind::Identifier,
                                        value: ValueKind::String("i".into()),
                                        location: loc.clone(),
                                    }),
                                    right: Box::new(AstNode::Literal {
                                        kind: TokenKind::IntegerLiteral,
                                        value: ValueKind::Number(1),
                                        location: loc.clone(),
                                    }),
                                    operator: TokenKind::Add,
                                    treat_as_string: false,
                                    dunder_methods: false,
                                    location: loc.clone(),
                                })),
                                location: loc.clone(),
                                value_location: loc.clone(),
                            })),
                            body: vec![AstNode::FunctionCall {
                                name: "push".into(),
                                generics: vec![],
                                parameters: vec![
                                    (
                                        loc.clone(),
                                        AstNode::Literal {
                                            kind: TokenKind::Identifier,
                                            value: ValueKind::String("args".into()),
                                            location: loc.clone(),
                                        },
                                    ),
                                    (
                                        loc.clone(),
                                        AstNode::MemoryOperation {
                                            left: Box::new(AstNode::Literal {
                                                kind: TokenKind::Identifier,
                                                value: ValueKind::String("argv".into()),
                                                location: loc.clone(),
                                            }),
                                            right: Box::new(AstNode::Literal {
                                                kind: TokenKind::Identifier,
                                                value: ValueKind::String("i".into()),
                                                location: loc.clone(),
                                            }),
                                            value: None,
                                            left_location: loc.clone(),
                                            right_location: loc.clone(),
                                            value_location: loc.clone(),
                                            is_deref: false,
                                        },
                                    ),
                                ],
                                type_method: true,
                                ignore_no_def: false,
                                location: loc.clone(),
                            }],
                            location: loc.clone(),
                        },
                    ]
                } else {
                    vec![]
                },
                vec![
                    AstNode::Declare {
                        name: "status".into(),
                        r#type: Some(Type::Word),
                        value: Some(Box::new(AstNode::FunctionCall {
                            name: MAIN_ID.into(),
                            generics: vec![],
                            parameters: if main_arg_len == 1 {
                                vec![(
                                    loc.clone(),
                                    AstNode::Literal {
                                        kind: TokenKind::Identifier,
                                        value: ValueKind::String("args".into()),
                                        location: loc.clone(),
                                    },
                                )]
                            } else {
                                vec![]
                            },
                            type_method: false,
                            ignore_no_def: false,
                            location: loc.clone(),
                        })),
                        location: loc.clone(),
                        value_location: loc.clone(),
                    },
                    AstNode::FunctionCall {
                        name: "free_self".into(),
                        generics: vec![],
                        parameters: vec![(
                            loc.clone(),
                            AstNode::FieldAccess {
                                left: Box::new(AstNode::Literal {
                                    kind: TokenKind::Identifier,
                                    value: ValueKind::String("env".into()),
                                    location: loc.clone(),
                                }),
                                right: Box::new(AstNode::Literal {
                                    kind: TokenKind::Identifier,
                                    value: ValueKind::String("allocator".into()),
                                    location: loc.clone(),
                                }),
                                value: None,
                                location: loc.clone(),
                            },
                        )],
                        type_method: true,
                        ignore_no_def: false,
                        location: loc.clone(),
                    },
                    AstNode::Return {
                        value: Box::new(AstNode::Literal {
                            kind: TokenKind::Identifier,
                            value: ValueKind::String("status".into()),
                            location: loc.clone(),
                        }),
                        location: loc.clone(),
                    },
                ],
            ]
            .concat()
            .into_iter()
            .collect(),
            location: loc.clone(),
            return_location: loc.clone(),
        });
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

    fs::create_dir_all(&build_path).expect("Failed to create ./.build.");
    let path_to_qbe_dist = format!("{build_path}/target.ssa");

    Compiler::compile(
        tree,
        path_to_qbe_dist.clone(),
        warnings,
        object_output,
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

    fs::remove_dir_all(&build_path).expect("Failed to delete ./.build.");

    if out != EmitKind::None {
        if !hush {
            println!(
                "{GREEN}Finished compiling '{path}' successfully! ヽ(•ᴗ•)ﾉ{RESET}",
                path = input_path.split("/").last().unwrap()
            );
        }

        ExitCode::SUCCESS
    } else {
        if !hush {
            println!(
                "{RED}Compilation of '{path}' finished with errors. (っ◞‸◟ c){RESET}",
                path = input_path.split("/").last().unwrap()
            );
        }

        ExitCode::FAILURE
    }
}
