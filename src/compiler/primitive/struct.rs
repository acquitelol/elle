use crate::{
    GREEN, RESET,
    compiler::{compiler::Compiler, qbe::typedef::TypeDef},
    elle_error, get_GREEN, get_RESET,
    parser::enums::StructSource,
};

pub fn generate_struct(this: StructSource, compiler: &mut Compiler) -> TypeDef {
    let mut items = vec![];

    if this.members.is_empty() && !this.ignore_empty {
        elle_error!(
            this.keyword_location
                .borrow()
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

    compiler.struct_pool.insert(
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
