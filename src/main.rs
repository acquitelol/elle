mod lexer;

use std::fs;

use lexer::Lexer;

fn main() {
    let binding = fs::read_to_string("main.elle").unwrap();
    let input = binding.as_str();

    let mut lexer = Lexer::new(input);
    while let Some(token) = lexer.next_token() {
        println!("{:?}", token);
    }
}
