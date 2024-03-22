mod lexer;
mod parser;

use lexer::Lexer;
use parser::Parser;
use std::fs;

fn main() {
    let binding = fs::read_to_string("main.elle").unwrap();
    let input = binding.as_str();

    let mut lexer = Lexer::new(input);
    let mut tokens = vec![];

    while let Some(token) = lexer.next_token() {
        tokens.push(token);
    }

    let mut parser = Parser::new(tokens);
    parser.parse();
}
