use crate::{
    compiler::primitive::PrimitiveExpr, lexer::enums::ValueKind, parser::enums::Primitive,
};

use lazy_static::lazy_static;
use std::io::Write;
use std::{fs::File, sync::Mutex};

pub struct Global {
    pub name: String,
    pub value: String,
    pub public: bool,
}

lazy_static! {
    pub static ref GLOBALS: Mutex<Vec<Global>> = Mutex::new(Vec::new());
}

pub struct Compiler {
    exprs: Vec<String>,
    tree: Vec<Primitive>,
}

impl Compiler {
    pub fn new(tree: Vec<Primitive>) -> Self {
        Compiler {
            tree,
            exprs: vec![],
        }
    }

    pub fn compile(&mut self) {
        for primitive in self.tree.iter_mut() {
            match primitive {
                Primitive::Constant {
                    name,
                    public,
                    r#type,
                    value,
                } => {
                    let parsed_value = match value {
                        ValueKind::String(value) => format!("b \"{}\", b 0", value),
                        ValueKind::Number(value) => format!("w {}", value),
                        ValueKind::Character(value) => format!("w \"{}\"", value),
                        ValueKind::Nil => "w 0".to_string(),
                    };

                    let mut globals = GLOBALS.lock().unwrap();
                    globals.push(Global {
                        name: name.to_string(),
                        value: parsed_value,
                        public: public.clone(),
                    });
                }
                _ => match PrimitiveExpr::new(primitive.clone()).compile() {
                    Some(expr) => self.exprs.push(expr),
                    None => {}
                },
            };
        }

        let mut file = File::create("main.ssa").expect("Failed to create the file.");

        let mut globals = GLOBALS.lock().unwrap();
        let mut res = globals
            .iter_mut()
            .map(|global| {
                let prefix = if global.public { "export " } else { "" };
                format!("{}data ${} = {{ {} }}", prefix, global.name, global.value)
            })
            .collect::<Vec<String>>();

        res.append(&mut self.exprs);

        file.write_all(res.join("\n").as_bytes())
            .expect("Failed to write to file.");

        file.flush().expect("Failed to flush file");
    }
}
