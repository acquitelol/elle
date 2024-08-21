use std::cell::RefCell;
use std::collections::HashSet;
use std::env;
use std::path::Path;
use std::process::ExitCode;
use std::time::Instant;

mod compiler;
mod lexer;
mod misc;
mod parser;

use compiler::compiler::Compiler;
use lexer::colors::{GREEN, RESET};
use misc::modules::lex_and_parse;
static META_STRUCT_NAME: &str = "ElleMeta";

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
    let mut args = env::args();
    let program = args.next().expect("program");

    let input_path = if let Some(input_path) = args.next() {
        input_path
    } else {
        eprintln!("ERROR: no input is provided");
        eprintln!("Usage: {program} <main.l | main.elle>");
        return ExitCode::FAILURE;
    };

    let output_path = if let Some(output) = args.next() {
        output
    } else {
        let tmp = Path::new(&input_path).with_extension("ssa");
        tmp.to_str().unwrap().into()
    };

    let mut warnings = Warnings::new();
    let mut debug = false;

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "-Wimplicitcast" => warnings.set_warning(Warning::ImplicitCast),
            "-Wstructfieldsmissing" => warnings.set_warning(Warning::StructFieldsMissing),
            "-Winvalidalias" => warnings.set_warning(Warning::InvalidAlias),
            "-Wall" => warnings.set_all(),
            "-Dtime" => debug = true,
            _ => {}
        }
    }

    let now = if debug { Some(Instant::now()) } else { None };
    let mut pool = HashSet::new();

    pool.insert(META_STRUCT_NAME.into());

    let struct_pool: RefCell<HashSet<String>> = RefCell::new(pool);
    let parsed_modules = RefCell::new(HashSet::new());

    let tree = lex_and_parse(
        &input_path,
        None,
        &struct_pool,
        &parsed_modules,
        &warnings,
        debug,
        0,
    );

    if debug {
        println!(
            "\n✦ Tokenization and parsing took {}",
            elapsed_with_color!(now.unwrap().elapsed())
        );
    }

    // #[cfg(debug_assertions)]
    // dbg!(&tree);

    let now = if debug { Some(Instant::now()) } else { None };

    Compiler::compile(tree, output_path, warnings);

    if debug {
        println!(
            "✦ Compilation took {}\n",
            elapsed_with_color!(now.unwrap().elapsed())
        );
        println!(
            "{GREEN}Finished compiling '{path}' successfully! ヽ(•ᴗ•)ﾉ\n{RESET}",
            path = input_path.split("/").last().unwrap()
        )
    }

    ExitCode::SUCCESS
}
