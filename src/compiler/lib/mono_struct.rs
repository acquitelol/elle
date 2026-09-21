use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    compiler::{
        compiler::Compiler,
        primitive::r#struct::generate_struct,
        qbe::{module::Module, r#type::Type, typedef::TypeDef},
    },
    lexer::enums::Location,
    parser::enums::{Argument, Primitive},
};

pub fn create_monomorphized_struct(
    compiler: &mut Compiler,
    module: &RefCell<Module>,
    generic_name: &str,
) {
    let (name, parts) = Type::from_internal_id(generic_name);

    let (generics, members, ..) = compiler
        .struct_pool
        .get(&name)
        .unwrap_or_else(|| panic!("Base {name} should exist"));

    let parsed_generics = generics
        .iter()
        .enumerate()
        .map(|(i, generic)| (generic.clone(), parts[i].clone()))
        .collect::<HashMap<_, _>>();

    let struct_pool = RefCell::new(compiler.struct_pool.clone());
    let tree = RefCell::new(vec![]);

    let parsed_members = members
        .iter()
        .map(|member| Argument {
            name: member.name.clone(),
            r#type: member.r#type.clone().unknown_to_known(
                Some(&struct_pool),
                Some(&tree),
                generics,
                &parsed_generics,
            ),
            no_fmt: member.no_fmt,
            is_unused: member.is_unused,
        })
        .collect::<Vec<Argument>>();

    struct_pool.borrow().clone_into(&mut compiler.struct_pool);

    for primitive in tree.borrow().to_owned() {
        match primitive {
            Primitive::Struct(this) => {
                let td = generate_struct(this, compiler);
                module.borrow_mut().add_type(td);
            }
            _ => {}
        }
    }

    let mut items = vec![];

    for member in parsed_members.iter().cloned() {
        items.push((member.r#type, 1));
    }

    let td = TypeDef {
        name: generic_name.to_string(),
        align: None,
        known_generics: parsed_generics,
        items,
        public: false,
        usable: true,
        imported: false,
    };

    module.borrow_mut().add_type(td);

    compiler.struct_pool.insert(
        generic_name.to_string(),
        (
            vec![],
            parsed_members,
            Rc::new(RefCell::new(Location::default(
                compiler.output_path.clone(),
            ))),
        ),
    );
}
