use crate::compiler::{compiler::Compiler, qbe::r#type::Type};

pub fn can_convert_to_type(gen: &Compiler, first: Type, second: Type, explicit: bool) -> bool {
    if first.is_struct() || second.is_struct() {
        let structs_are_the_same = first == second;
        let explicit_struct_to_ptr = explicit
            && ((first.is_struct() && second.is_pointer_like())
                || (second.is_struct() && first.is_pointer_like()));
        let first_is_ptr_of_second =
            first.is_pointer() && first.get_pointer_inner().unwrap() == second;

        return structs_are_the_same || explicit_struct_to_ptr || first_is_ptr_of_second;
    }

    if ((first.is_strictly_number() && second.is_string())
        || (second.is_strictly_number() && first.is_string()))
        && !explicit
    {
        return false;
    }

    if (first.is_pointer() && second.is_pointer())
        && (first.get_pointer_inner().unwrap().is_void()
            || second.get_pointer_inner().unwrap().is_void())
    {
        return true;
    }

    if ((first.is_pointer() && second.is_pointer())
        && first.get_pointer_inner().unwrap() != second.get_pointer_inner().unwrap())
        && !explicit
        && gen.pedantic
    {
        return false;
    }

    let weights_match = first.weight() == second.weight();
    let both_int_or_float =
        (first.is_int() && second.is_int()) || (first.is_float() && second.is_float());
    let explicit_enum_cast = if explicit {
        first.is_enum() || second.is_enum()
    } else {
        (first.is_enum() && first.get_enum_repr().unwrap() == second)
            || (second.is_enum() && second.get_enum_repr().unwrap() == first)
    };

    return weights_match || both_int_or_float || explicit_enum_cast;
}
