use std::cell::RefCell;

use crate::{
    compiler::{
        compiler::Compiler,
        qbe::{function::Function, instruction::Instruction, r#type::Type, value::Value},
    },
    elle_error,
    lexer::enums::{Location, MutRc},
};

pub fn convert_to_type(
    gen: &mut Compiler,
    func: &RefCell<Function>,
    first: Type,
    second: Type,
    val: Value,
    left_location: &MutRc<Location>,
    right_location: &MutRc<Location>,
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
            }

            let tmp = gen.new_temporary(Some("load"), false);

            func.borrow_mut().assign_instruction(
                &tmp,
                &second,
                Instruction::Load(second.clone(), val),
            );

            return (second, tmp);
        }

        elle_error!(left_location
            .borrow()
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
                right_location.borrow().with_extra_info(format!(
                    "This has the type '{}'",
                    first.display()
                )).error(format!(
                    "Cannot implicitly convert '{}' to '{}' or vice versa.\nTo explicitly convert, use the '#cast(T, expr)' directive.",
                    first.display(),
                    second.display()
                ))
            )
        };
    }

    if first.is_enum() || second.is_enum() {
        if (first.is_enum() && second == first.get_enum_repr().unwrap_or(Type::Word))
            || (second.is_enum() && first == second.get_enum_repr().unwrap_or(Type::Word))
            || (first.is_enum()
                && second.is_enum()
                && first.get_enum_inner().unwrap() == second.get_enum_inner().unwrap())
            || first == second
            // (Foo(u32) >> char) should be allowed
            // char will be casted up to a word
            || first.weight() < second.weight()
            || explicit
        {
            // if were involving floats at all, that probably means
            // were converting an enum for a binary operation. so we should properly
            // perform the conversion to repr type using extension and conversion
            // for ints to floats.
            //
            // there is a loose invariant that enum reprs can only ever be
            //
            // - integers
            // - floats
            // - strings
            // - other enums
            //
            // due to the literal values for variants only being allowed to be
            //
            // - char literals
            // - string literals
            // - integers
            //
            // this means that can safely assume that, if floats are involved,
            // the type conversion should be fully fledged and not a reinterpret
            // cast, as this is likely a conversion from an enum to a float
            // like `Foo::A * 3.0`
            if !first.get_enum_repr().unwrap_or(first.clone()).is_float()
                && !second.get_enum_repr().unwrap_or(second.clone()).is_float()
            {
                return (second, val);
            }

            return convert_to_type(
                gen,
                func,
                first.get_enum_repr().unwrap_or(first.clone()),
                second.get_enum_repr().unwrap_or(second.clone()),
                val,
                left_location,
                right_location,
                explicit,
            );
        }

        implicit_conversion_error!()
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

    if (first.is_static_array()
        && second.is_pointer()
        && first.get_static_array_inner().unwrap() == second.get_pointer_inner().unwrap())
        || (first.is_static_array()
            && second.is_string()
            && first.get_static_array_inner().unwrap() == Type::Char)
    {
        return (second, val);
    }

    if let Type::StaticArray(ref lhs, ref lhs_size) = first
        && let Type::StaticArray(ref rhs, ref rhs_size) = second
    {
        if lhs.function_eq(rhs, Some(&left_location)) && (lhs_size == rhs_size || explicit) {
            return (second, val);
        }

        implicit_conversion_error!()
    }

    if ((first.is_pointer_like() && !first.is_void_pointer() && second.is_map_to_int())
        || (first.is_map_to_int() && second.is_pointer_like() && !second.is_void_pointer()))
        && !explicit
    {
        implicit_conversion_error!()
    }

    if first.is_function() && second.is_function() {
        if explicit || first.function_eq(&second, Some(&left_location)) {
            return (second, val);
        }

        implicit_conversion_error!()
    }

    if ((first.is_pointer() && second.is_pointer())
        && !first
            .get_pointer_inner()
            .unwrap()
            .function_eq(&second.get_pointer_inner().unwrap(), Some(left_location)))
        && !explicit
        && gen.pedantic
    {
        implicit_conversion_error!()
    }

    if first.weight() == second.weight() {
        (second, val)
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

        (second, conv)
    } else {
        let conv = gen.new_temporary(Some("conv"), true);

        func.borrow_mut().assign_instruction(
            &conv,
            &second,
            Instruction::Conversion(first, second.clone(), val),
        );

        (second, conv)
    }
}
