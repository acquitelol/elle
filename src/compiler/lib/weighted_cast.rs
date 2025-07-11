use std::{cell::RefCell, cmp::Ordering};

use crate::{
    compiler::{
        compiler::Compiler,
        qbe::{function::Function, r#type::Type, value::Value},
    },
    lexer::enums::{Location, MutRc},
};

use super::convert::convert_to_type;

pub fn handle_weighted_cast(
    gen: &mut Compiler,
    func: &RefCell<Function>,
    left_ty: &mut Type,
    left_val: &mut Value,
    right_ty: &mut Type,
    right_val: &mut Value,
    location: &MutRc<Location>,
) {
    match left_ty.weight().cmp(&right_ty.weight()) {
        Ordering::Greater => {
            let (ty, val) = convert_to_type(
                gen,
                func,
                right_ty.clone(),
                left_ty.clone(),
                right_val.clone(),
                location,
                location,
                false,
            );

            *right_ty = ty;
            *right_val = val;
        }
        Ordering::Less => {
            let (ty, val) = convert_to_type(
                gen,
                func,
                left_ty.clone(),
                right_ty.clone(),
                left_val.clone(),
                location,
                location,
                false,
            );

            *left_ty = ty;
            *left_val = val;
        }
        Ordering::Equal => {}
    }
}
