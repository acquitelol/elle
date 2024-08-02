use std::env;
use std::fs;
use std::path::Path;
use std::process::ExitCode;

mod compiler;
mod lexer;
mod parser;

use compiler::compiler::Compiler;
use compiler::enums::Type;
use lexer::enums::Location;
use lexer::enums::ValueKind;
use lexer::{enums::TokenKind, lexer::Lexer};
use parser::enums::AstNode;
use parser::enums::Primitive;
use parser::parser::Parser;

fn lex_and_parse(input_path: String, root: bool) -> Vec<Primitive> {
    let with_elle = fs::metadata(format!("{}.elle", input_path)).is_ok();
    let base = fs::metadata(input_path.clone()).is_ok();

    let content = match fs::read_to_string(&format!(
        "{}{}",
        input_path,
        if base {
            ""
        } else {
            if with_elle {
                ".elle"
            } else {
                ".l"
            }
        }
    )) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("\nERROR: could not load module \"{input_path}\": {err}\n");
            format!("")
        }
    };

    let mut lexer = Lexer::new(input_path.clone(), content.as_str());
    let mut tokens = vec![];

    while let Some(token) = lexer.next_token() {
        // Even though the lexer does provide us with comments, we don't care about them
        // so we can just ignore them and not pass them the parser
        match token.kind {
            TokenKind::Comment => {}
            _ => tokens.push(token),
        }
    }

    let mut parser = Parser::new(tokens.clone());
    let mut tree = parser.parse();

    for node in tree.clone().iter().cloned() {
        handle_node(&mut tree, node);
    }

    // Add global constants
    // - EOF => -1
    // - NULL => 0 (nullptr)
    if root {
        tree.insert(
            0,
            Primitive::Constant {
                name: "EOF".to_string(),
                public: false,
                usable: true,
                imported: false,
                r#type: Some(Type::Long),
                value: Box::new(AstNode::LiteralStatement {
                    kind: TokenKind::LongLiteral,
                    value: ValueKind::Number(-1),
                    location: Location::default(input_path.clone()),
                }),
                location: Location::default(input_path.clone()),
            },
        );

        tree.insert(
            0,
            Primitive::Constant {
                name: "NULL".to_string(),
                public: false,
                usable: true,
                imported: false,
                r#type: Some(Type::Long),
                value: Box::new(AstNode::LiteralStatement {
                    kind: TokenKind::LongLiteral,
                    value: ValueKind::Number(0),
                    location: Location::default(input_path.clone()),
                }),
                location: Location::default(input_path.clone()),
            },
        );
    }

    tree
}

fn handle_node(tree: &mut Vec<Primitive>, node: Primitive) {
    match node {
        Primitive::Use {
            module, functions, ..
        } => {
            let module_tree = lex_and_parse(module, false);
            let allow_all = functions.len() == 0;

            for symbol in module_tree.iter().cloned().rev() {
                match symbol.clone() {
                    Primitive::Use { .. } => {}
                    Primitive::Constant { name, public, .. } => {
                        match existing_definition(tree.clone(), name.clone()) {
                            Some(index) => {
                                tree.remove(index);
                            }
                            None => {}
                        }

                        let mut new_symbol = symbol.clone();
                        if let Primitive::Constant {
                            ref mut usable,
                            ref mut imported,
                            ..
                        } = new_symbol
                        {
                            *usable = is_valid_insert_context(
                                name.clone(),
                                public,
                                allow_all,
                                functions.clone(),
                            );

                            *imported = true;
                        }

                        tree.insert(0, new_symbol);
                    }
                    Primitive::Function { name, public, .. } => {
                        match existing_definition(tree.clone(), name.clone()) {
                            Some(index) => {
                                tree.remove(index);
                            }
                            None => {}
                        }

                        let mut new_symbol = symbol.clone();
                        if let Primitive::Function {
                            ref mut usable,
                            ref mut imported,
                            ..
                        } = new_symbol
                        {
                            *usable = is_valid_insert_context(
                                name.clone(),
                                public,
                                allow_all,
                                functions.clone(),
                            );

                            *imported = true;
                        }

                        tree.insert(0, new_symbol);
                    }
                }
            }
        }
        _ => {}
    }
}

fn is_valid_insert_context(
    node_name: String,
    public: bool,
    allow_all: bool,
    functions: Vec<String>,
) -> bool {
    public && (allow_all || functions.contains(&node_name))
}

fn existing_definition(tree: Vec<Primitive>, node_name: String) -> Option<usize> {
    tree.iter().position(|item| match item.to_owned().clone() {
        Primitive::Use { .. } => false,
        Primitive::Constant { name, .. } => name == node_name,
        Primitive::Function { name, .. } => name == node_name,
    })
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
        tmp.to_str().unwrap().to_string()
    };

    let tree = lex_and_parse(input_path, true);

    dbg!(&tree);

    Compiler::compile(tree, output_path);
    ExitCode::SUCCESS
}
