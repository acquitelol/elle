use std::cell::RefCell;

use crate::{
    compiler::{
        compiler::Compiler,
        enums::{Function, Instruction, Type, Value},
    },
    elle_error,
    lexer::enums::Location,
};

pub fn convert_to_type(
    gen: &mut Compiler,
    func: &RefCell<Function>,
    first: Type,
    second: Type,
    val: Value,
    left_location: &Location,
    right_location: &Location,
    explicit: bool,
) -> (Type, Value) {
    // TODO: ADD A VARIANT TO `can_convert_to_type` WHEN ADDING A VARIANT HERE
    if first.is_struct() || second.is_struct() {
        if first == second {
            return (second, val);
        }

        if explicit
            && ((first.is_struct() && second.is_pointer_like())
                || (second.is_struct() && first.is_pointer_like()))
        {
            return (second, val);
        }

        if first.is_pointer() && first.get_pointer_inner().unwrap() == second {
            if second.is_struct() {
                return (second, val);
            } else {
                let tmp = gen.new_temporary(Some("load"), false);

                func.borrow_mut().assign_instruction(
                    &tmp,
                    &second.clone(),
                    Instruction::Load(second.clone(), val),
                );

                return (second, tmp);
            }
        }

        elle_error!(left_location
            .clone()
            .with_extra_info(format!("This has the type '{}'", first.display()))
            .error(format!(
                "Cannot convert from the type '{}' to the type '{}'.",
                first.display(),
                second.display()
            )))
    }

    macro_rules! implicit_conversion_error {
        () => {
            elle_error!(
                right_location.clone().with_extra_info(format!(
                    "This has the type '{}'",
                    first.display()
                )).error(format!(
                    "Cannot implicitly convert '{}' to '{}' or vice versa.\nTo explicitly convert, use the C-like '(type)variable' syntax.",
                    first.display(),
                    second.display()
                ))
            )
        };
    }

    if ((first.is_strictly_number() && second.is_string())
        || (second.is_strictly_number() && first.is_string()))
        && !explicit
    {
        implicit_conversion_error!()
    }

    if first.is_pointer()
        && second.is_pointer()
        && (first.get_pointer_inner().unwrap().is_void()
            || second.get_pointer_inner().unwrap().is_void())
    {
        return (second, val);
    }

    if ((first.is_pointer() && second.is_pointer())
        && first.get_pointer_inner().unwrap() != second.get_pointer_inner().unwrap())
        && !explicit
        && gen.pedantic
    {
        implicit_conversion_error!()
    }

    if first.weight() == second.weight() {
        return (second, val);
    } else if (first.is_int() && second.is_int()) || (first.is_float() && second.is_float()) {
        let conv = gen.new_temporary(Some("conv"), true);
        let is_first_higher = first.weight() > second.weight();

        func.borrow_mut().assign_instruction(
            &conv,
            &second,
            if is_first_higher {
                if first.is_float() {
                    Instruction::Truncate(val)
                } else {
                    // Subtyping in QBE means that longs can automatically
                    // work as ints but not vice versa
                    Instruction::Copy(val)
                }
            } else {
                Instruction::Extension(first, val)
            },
        );

        return (second, conv);
    } else {
        let conv = gen.new_temporary(Some("conv"), true);

        func.borrow_mut().assign_instruction(
            &conv,
            &second,
            Instruction::Conversion(first, second.clone(), val),
        );

        return (second, conv);
    }
}
