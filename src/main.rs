use std::collections::HashSet;
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
use misc::{build::build, colors::*, help::print_help, modules::lex_and_parse};

static META_STRUCT_NAME: &str = "ElleMeta";
static STD_LIB_PATH: &str = "/usr/local/include/elle";

pub enum Warning {
    ImplicitCast = 1 << 0,
    StructFieldsMissing = 1 << 1,
    InvalidAlias = 1 << 2,
}

impl Warning {
    pub const fn all() -> u32 {
        Self::ImplicitCast as u32 | Self::InvalidAlias as u32 | Self::StructFieldsMissing as u32
    }
}

#[derive(Debug, Clone)]
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
        exit(1);
    }

    let mut input_path = None;
    let mut output_path = None;

    let mut warnings = Warnings::new();

    let mut debug_time = false;
    let mut emit_qbe = false;
    let mut emit_asm = false;

    let mut linker_flags = None;
    let mut linker_path = "cc".into();
    let mut qbe_path = "qbe".into();

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "-Wimplicit-cast" => warnings.set_warning(Warning::ImplicitCast),
            "-Wstruct-fields-missing" => warnings.set_warning(Warning::StructFieldsMissing),
            "-Winvalid-alias" => warnings.set_warning(Warning::InvalidAlias),
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
            other if other.ends_with(".l") || other.ends_with(".elle") => {
                if input_path.is_none() {
                    input_path = Some(other.to_string())
                }
            }
            other => {
                println!("{RED}Invalid argument: {}", other);
                println!("{RED}For help, please use the following command:");
                println!("{RED}\n{program} [-h | --help]");

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
    let mut pool = HashSet::new();

    pool.insert(META_STRUCT_NAME.into());

    let struct_pool: RefCell<HashSet<String>> = RefCell::new(pool);
    let parsed_modules = RefCell::new(HashSet::new());

    let input_path = if let Some(input_path) = input_path {
        input_path
    } else {
        eprintln!("ERROR: no input is provided");
        eprintln!("Usage: {program} <main.l | main.elle>");
        return ExitCode::FAILURE;
    };

    let tree = lex_and_parse(
        &input_path,
        None,
        &struct_pool,
        &parsed_modules,
        &warnings,
        debug_time,
        0,
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

    let success;

    if emit_qbe {
        let path = Path::new(&parsed_output_path).with_extension("ssa");
        fs::rename(path_to_qbe_dist, path.clone()).unwrap();

        success = true;
    } else {
        let result = build(
            qbe_path,
            path_to_qbe_dist,
            parsed_output_path,
            emit_asm,
            linker_flags,
            linker_path,
        );

        success = result;
    }

    fs::remove_dir_all("./.build").expect("Failed to delete ./.build.");

    if success {
        println!(
            "{GREEN}Finished compiling '{path}' successfully! ヽ(•ᴗ•)ﾉ{RESET}",
            path = input_path.split("/").last().unwrap()
        );

        ExitCode::SUCCESS
    } else {
        println!(
            "{RED}Compilation of '{path}' finished with errors. (っ◞‸◟ c){RESET}",
            path = input_path.split("/").last().unwrap()
        );

        ExitCode::FAILURE
    }
}
