use std::collections::{HashMap, HashSet};
use std::env;
use std::path::Path;
use std::process::{exit, ExitCode};
use std::time::Instant;
use std::{cell::RefCell, fs};

mod compiler;
mod lexer;
mod misc;
mod parser;

use compiler::compiler::Compiler;
use compiler::enums::Type;
use lexer::enums::Location;
use misc::{build::build, colors::*, help::print_help, modules::lex_and_parse};
use parser::enums::{Argument, Primitive};

static META_STRUCT_NAME: &str = "ElleMeta";
static GENERIC_IDENTIFIER: &str = "0"; // Start of a generic
static GENERIC_END: &str = "1"; // Allowing for nested generic structs
static GENERIC_POINTER: &str = "2"; // Pointer to another type
static GENERIC_UNKNOWN: &str = "3"; // Unknown type T
static STD_LIB_PATH: &str = "/usr/local/include/elle";
static RESERVED_KEYWORDS: &[&'static str] = &[
    "as", "let", "mut", "enum", "match", "static", "super", "do", "macro", "in", "step",
];

pub enum Warning {
    ImplicitCast = 1 << 0,
    StructFieldsMissing = 1 << 1,
    InvalidAlias = 1 << 2,
    VariadicNoMeta = 1 << 3,
    CStyleVoid = 1 << 4,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum EmitKind {
    Executable(String),
    QbeFile(String),
    AsmFile(String),
    None,
}

impl Warning {
    pub const fn all() -> u32 {
        Self::ImplicitCast as u32
            | Self::InvalidAlias as u32
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
    let mut hush = false;

    let mut linker_flags = None;
    let mut linker_path = "cc".into();
    let mut qbe_path = "qbe".into();

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "-Wimplicit-cast" => warnings.set_warning(Warning::ImplicitCast),
            "-Wstruct-fields-missing" => warnings.set_warning(Warning::StructFieldsMissing),
            "-Winvalid-alias" => warnings.set_warning(Warning::InvalidAlias),
            "-Wvariadic-no-meta" => warnings.set_warning(Warning::VariadicNoMeta),
            "-Wc-style-void" => warnings.set_warning(Warning::CStyleVoid),
            "-Wall" => warnings.set_all(),
            "--elapsed-time" | "-Dtime" => debug_time = true,
            "--emit-qbe" | "-Demit-qbe" | "-Demit-ssa" => emit_qbe = true,
            "--emit-asm" | "-Demit-asm" | "-Demit-s" => emit_asm = true,
            "-o" => output_path = args.next(),
            "-h" | "--help" => {
                print_help(program);
                exit(0);
            }
            "--link-flags" | "-Clink-flags" | "-Clinker-flags" => linker_flags = args.next(),
            "--link-path" | "-Clink-path" | "-Clinker-path" => {
                linker_path = args.next().unwrap_or("cc".into())
            }
            "--qbe-path" | "-Cqbe-path" | "-Cssa-path" => {
                qbe_path = args.next().unwrap_or("qbe".into())
            }
            "--hush" | "-Chush" => {
                hush = true;
            }
            other if other.ends_with(".l") || other.ends_with(".elle") => {
                if input_path.is_none() {
                    input_path = Some(other.to_string())
                }
            }
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

    let input_path = if let Some(input_path) = input_path {
        input_path
    } else {
        eprintln!("ERROR: no input is provided");
        eprintln!("Usage: {program} <main.l | main.elle>");
        return ExitCode::FAILURE;
    };

    pool.insert(
        META_STRUCT_NAME.into(),
        (
            vec![],
            meta_members.clone(),
            Location::default(input_path.clone()),
        ),
    );

    let struct_pool = RefCell::new(pool);
    let parsed_modules = RefCell::new(HashSet::new());

    let mut tree = lex_and_parse(
        &input_path,
        None,
        &struct_pool,
        &parsed_modules,
        &warnings,
        debug_time,
        0,
        Location::default(input_path.clone()),
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
            keyword_location: Location::default(input_path.clone()),
            location: Location::default(input_path.clone()),
            ignore_empty: false,
        },
    );

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

    fs::create_dir_all("./.build").expect("Failed to create ./.build.");

    let path_to_qbe_dist = "./.build/target.ssa".to_string();
    Compiler::compile(tree, path_to_qbe_dist.clone(), warnings);

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
            linker_flags,
            linker_path,
        );

        out = result;
    }

    fs::remove_dir_all("./.build").expect("Failed to delete ./.build.");

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
