mod compiler;
mod lexer;
mod parser;

use compiler::compiler::Compiler;
use lexer::{enums::TokenKind, lexer::Lexer};
use parser::parser::Parser;
use std::fs;

fn main() {
    let file = "main.elle".to_owned();
    let binding = fs::read_to_string(&file).unwrap();
    let input = binding.as_str();

    let mut lexer = Lexer::new(file, input);
    let mut tokens = vec![];

    while let Some(token) = lexer.next_token() {
        // Even though the lexer does provide us with comments, we don't care about them
        // so we can just ignore them and not pass these tokens to the parser
        match token.kind {
            TokenKind::Comment => {}
            _ => tokens.push(token),
        }
    }

    // dbg!(&tokens);

    let mut parser = Parser::new(tokens);
    let tree = parser.parse();

    // dbg!(&tree);

    let mut compiler = Compiler::new(tree);
    compiler.compile();
}
