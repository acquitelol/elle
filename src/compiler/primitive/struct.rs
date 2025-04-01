use crate::{
    compiler::{compiler::Compiler, enums::TypeDef},
    elle_error, get_GREEN, get_RESET,
    parser::enums::StructSource,
    GREEN, RESET,
};

pub fn generate_struct(this: StructSource, gen: &mut Compiler) -> TypeDef {
    let mut items = vec![];

    if this.members.is_empty() && !this.ignore_empty {
        elle_error!(
            this.keyword_location
                .with_extra_info("Replace this with 'namespace'")
                .error(format!(
                    "Cannot declare an empty struct (with no members).\nIf you intended to make a namespace, use the '{GREEN}namespace{RESET}' keyword instead.",
                    GREEN = get_GREEN!(),
                    RESET = get_RESET!()
                ))
        )
    }

    for member in this.members.iter().cloned() {
        items.push((member.r#type, 1));
    }

    gen.struct_pool.insert(
        this.name.clone(),
        (this.generics, this.members, this.keyword_location),
    );

    TypeDef {
        name: this.name,
        align: None,
        known_generics: this.known_generics,
        items,
        public: this.public,
        usable: this.usable,
        imported: this.imported,
    }
}
