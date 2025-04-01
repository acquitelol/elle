use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    compiler::{
        compiler::Compiler,
        enums::{Module, TypeDef},
        primitive::r#struct::generate_struct,
        qbe::r#type::Type,
    },
    lexer::enums::Location,
    parser::enums::{Argument, Primitive},
};

pub fn create_monomorphized_struct(
    gen: &mut Compiler,
    module: &RefCell<Module>,
    generic_name: String,
) {
    let (name, parts) = Type::from_internal_id(generic_name.clone());

    let (generics, members, ..) = gen
        .struct_pool
        .get(&name)
        .expect(&format!("Base {name} should exist"));

    let parsed_generics = HashMap::from_iter(
        generics
            .iter()
            .enumerate()
            .map(|(i, generic)| (generic.clone(), parts[i].clone())),
    );

    let struct_pool = RefCell::new(gen.struct_pool.clone());
    let tree = RefCell::new(vec![]);

    let parsed_members = members
        .iter()
        .map(|member| Argument {
            name: member.name.clone(),
            r#type: member.r#type.clone().unknown_to_known(
                Some(&struct_pool),
                Some(&tree),
                generics.clone(),
                parsed_generics.clone(),
            ),
            manual: member.manual,
            no_fmt: member.no_fmt,
        })
        .collect::<Vec<Argument>>();

    gen.struct_pool = struct_pool.borrow().to_owned();

    for primitive in tree.borrow().to_owned().into_iter() {
        match primitive {
            Primitive::Struct(this) => {
                let td = generate_struct(this, gen);
                module.borrow_mut().add_type(td);
            }
            _ => {}
        };
    }

    let mut items = vec![];

    for member in parsed_members.iter().cloned() {
        items.push((member.r#type, 1));
    }

    let td = TypeDef {
        name: generic_name.clone(),
        align: None,
        known_generics: parsed_generics,
        items,
        public: false,
        usable: true,
        imported: false,
    };

    module.borrow_mut().add_type(td);

    gen.struct_pool.insert(
        generic_name.clone(),
        (
            vec![],
            parsed_members,
            Rc::new(Location::default(gen.output_path.clone())),
        ),
    );
}
