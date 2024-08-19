use std::cell::RefCell;
use std::env;
use std::path::Path;
use std::process::ExitCode;

mod compiler;
mod lexer;
mod misc;
mod parser;

use compiler::compiler::Compiler;
use misc::modules::lex_and_parse;
static META_STRUCT_NAME: &str = "ElleMeta";

pub enum Warning {
    ImplicitCast = 1 << 0,
    ImplicitDeclaration = 1 << 1,
    StructFieldsMissing = 1 << 2,
    InvalidAlias = 1 << 3,
}

impl Warning {
    pub const fn all() -> u32 {
        Self::ImplicitCast as u32
            | Self::ImplicitDeclaration as u32
            | Self::InvalidAlias as u32
            | Self::StructFieldsMissing as u32
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

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "-Wimplicitcast" => warnings.set_warning(Warning::ImplicitCast),
            "-Wimplicitdeclaration" => warnings.set_warning(Warning::ImplicitDeclaration),
            "-Wstructfieldsmissing" => warnings.set_warning(Warning::StructFieldsMissing),
            "-Winvalidalias" => warnings.set_warning(Warning::InvalidAlias),
            "-Wall" => warnings.set_all(),
            _ => {}
        }
    }

    let pool = vec![META_STRUCT_NAME.into()];
    let struct_pool: RefCell<Vec<String>> = RefCell::new(pool);
    let tree = lex_and_parse(input_path, &struct_pool, warnings.clone(), true);

    // #[cfg(debug_assertions)]
    // dbg!(&tree);

    Compiler::compile(tree, output_path, warnings);
    ExitCode::SUCCESS
}
