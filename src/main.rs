use std::cell::RefCell;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::Path;
use std::process::ExitCode;

mod compiler;
mod lexer;
mod macros;
mod parser;

use compiler::compiler::Compiler;
use compiler::enums::Type;
use lexer::enums::Location;
use lexer::enums::ValueKind;
use lexer::{enums::TokenKind, lexer::Lexer};
use parser::enums::Argument;
use parser::enums::AstNode;
use parser::enums::Primitive;
use parser::parser::Parser;

static META_STRUCT_NAME: &str = "ElleMeta";

fn lex_and_parse(
    input_path: String,
    struct_pool: &RefCell<Vec<String>>,
    root: bool,
) -> Vec<Primitive> {
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
            eprintln!("\nERROR: Could not load module \"{input_path}\": {err}\n");
            format!("")
        }
    };

    if content.trim().is_empty() {
        panic!(
            "\n{}\nERROR: Could not load module \"{input_path}\"\n{}\n\n{}\n{}\n",
            "-".repeat(40),
            "Module is empty. To create an entry-point, write:",
            "use std/io;\n\nfn main() {\n    puts(\"Hello world!\");\n}",
            "-".repeat(40),
        )
    }

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

    // dbg!(&tokens);

    let mut parser = Parser::new(tokens.clone(), struct_pool.borrow().to_owned());
    let (mut tree, new_struct_pool) = parser.parse(true, None);

    struct_pool.replace_with(|_| new_struct_pool);

    for node in tree.clone().iter().cloned() {
        handle_node(&mut tree, struct_pool, node);
    }

    let (mut tree, new_struct_pool) = parser.parse(false, Some(struct_pool.borrow().to_owned()));

    struct_pool.replace_with(|_| new_struct_pool);

    for node in tree.clone().iter().cloned() {
        handle_node(&mut tree, struct_pool, node);
    }

    // Add global constants
    // - nil => 0 (nullptr)
    // - ElleMeta => Utility struct
    if root {
        tree.insert(
            0,
            Primitive::Constant {
                name: "nil".into(),
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

        tree.insert(
            0,
            Primitive::Struct {
                name: META_STRUCT_NAME.into(),
                public: false,
                usable: true,
                imported: false,
                members: vec![
                    // Holds an array of expressions passed into the function in plain text
                    Argument {
                        name: "exprs".into(),
                        // string[]
                        r#type: Type::Pointer(Box::new(Type::Pointer(Box::new(Type::Char)))),
                    },
                    // Holds an array of the type of arguments passed into the function as strings
                    Argument {
                        name: "types".into(),
                        // string[]
                        r#type: Type::Pointer(Box::new(Type::Pointer(Box::new(Type::Char)))),
                    },
                    // Holds the number of arguments that were passed into a function
                    Argument {
                        name: "arity".into(),
                        // i32
                        r#type: Type::Word,
                    },
                    // Holds the name of the caller method as a string
                    Argument {
                        name: "caller".into(),
                        // string
                        r#type: Type::Pointer(Box::new(Type::Char)),
                    },
                ],
                location: Location::default(input_path.clone()),
            },
        );
    }

    tree
}

fn handle_node(tree: &mut Vec<Primitive>, struct_pool: &RefCell<Vec<String>>, node: Primitive) {
    match node {
        Primitive::Use {
            module, functions, ..
        } => {
            let module_tree = lex_and_parse(module, struct_pool, false);
            let allow_all = functions.len() == 0;

            for symbol in module_tree.iter().cloned().rev() {
                match symbol.clone() {
                    Primitive::Use { .. } => {}
                    Primitive::Constant { name, public, .. } => {
                        override_and_add_node!(
                            Primitive::Constant,
                            tree,
                            name,
                            symbol,
                            public,
                            allow_all,
                            functions
                        );
                    }
                    Primitive::Function { name, public, .. } => {
                        override_and_add_node!(
                            Primitive::Function,
                            tree,
                            name,
                            symbol,
                            public,
                            allow_all,
                            functions
                        );
                    }
                    Primitive::Struct { name, public, .. } => {
                        override_and_add_node!(
                            Primitive::Struct,
                            tree,
                            name,
                            symbol,
                            public,
                            allow_all,
                            functions
                        );
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
        Primitive::Struct { name, .. } => name == node_name,
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
        tmp.to_str().unwrap().into()
    };

    let pool = vec![META_STRUCT_NAME.into()];
    let struct_pool: RefCell<Vec<String>> = RefCell::new(pool);
    let tree = lex_and_parse(input_path, &struct_pool, true);

    #[cfg(debug_assertions)]
    dbg!(&tree);

    Compiler::compile(tree, output_path);
    ExitCode::SUCCESS
}
