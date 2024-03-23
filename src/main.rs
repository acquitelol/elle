mod lexer;
mod parser;

use lexer::Lexer;
use parser::Parser;
use std::fs;

fn main() {
    let file = "main.elle".to_owned();
    let binding = fs::read_to_string(&file).unwrap();
    let input = binding.as_str();

    let mut lexer = Lexer::new(file, input);
    let mut tokens = vec![];

    while let Some(token) = lexer.next_token() {
        tokens.push(token);
    }

    dbg!(&tokens);

    let mut parser = Parser::new(tokens);
    parser.parse();
}
