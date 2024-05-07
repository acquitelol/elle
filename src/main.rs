use std::env;
use std::fs;
use std::path::Path;
use std::process::ExitCode;

mod compiler;
mod lexer;
mod parser;

use compiler::compiler::Compiler;
use lexer::{enums::TokenKind, lexer::Lexer};
use parser::parser::Parser;

fn main() -> ExitCode {
    let mut args = env::args();
    let program = args.next().expect("program");

    let input_path = if let Some(input_path) = args.next() {
        input_path
    } else {
        eprintln!("ERROR: no input is provided");
        eprintln!("Usage: {program} <main.elle>");
        return ExitCode::FAILURE;
    };

    let output_path = Path::new(&input_path).with_extension("ssa");

    let content = match fs::read_to_string(&input_path) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("ERROR: could not load file {input_path}: {err}");
            return ExitCode::FAILURE;
        }
    };

    let mut lexer = Lexer::new(input_path, content.as_str());
    let mut tokens = vec![];

    while let Some(token) = lexer.next_token() {
        // Even though the lexer does provide us with comments, we don't care about them
        // so we can just ignore them and not pass them the parser
        match token.kind {
            TokenKind::Comment => {}
            _ => tokens.push(token),
        }
    }

    // dbg!(&tokens);

    let mut parser = Parser::new(tokens);
    let tree = parser.parse();

    // dbg!(&tree);

    Compiler::compile(tree, output_path);
    ExitCode::SUCCESS
}
