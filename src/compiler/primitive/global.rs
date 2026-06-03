use std::cell::RefCell;

use crate::{
    compiler::{
        compiler::{CodegenContext, Compiler},
        qbe::{
            data::Data, data_item::DataItem, function::Function, linkage::Linkage, module::Module,
            r#type::Type,
        },
    },
    parser::enums::GlobalSource,
};

pub fn generate_global(this: GlobalSource, gen: &mut Compiler, module: &RefCell<Module>) {
    let func = RefCell::new(Function::default());
    func.borrow_mut().add_block("start");

    let ty = if this.external {
        this.r#type.unwrap()
    } else if let Some(ty) = this.r#type {
        ty
    } else {
        this.value
            .unwrap()
            .compile(
                gen,
                &CodegenContext {
                    func: &func,
                    module,
                    ty: None,
                    value: None,
                    is_return: false,
                    is_generic: false,
                    is_field_access: false,
                },
            )
            .unwrap()
            .0
    };

    let data = Data {
        ty: Some(ty.clone()),
        external: this.external,
        linkage: if this.public {
            Linkage::public()
        } else {
            Linkage::private()
        },
        name: this.name.clone(),
        align: None,
        items: vec![(Type::Zeroed, DataItem::Const(ty.size(module) as i128))],
    };

    gen.data_sections.insert(this.name.clone(), data.clone());
    module.borrow_mut().add_data((this.name, data));
}
