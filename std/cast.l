use std/io;
use std/core;
use std/string;

global pub;

local fn is_valid(string str, string type) -> bool {
    string *valid_strspn = ["0123456789", "0123456789", "0123456789."];
    string *valid_types = ["i32", "i64", "f64"];
    i32 type_index = -1;

    for i32 i = 0; i < #arrlen(valid_types); i += 1 {
        if valid_types[i].equals(type) {
            type_index = i;
        }
    }

    if type_index == -1 {
        io::eprintln(type, " is not a valid type for casting. Falling back to 0", type);
        io::eprintln("Valid types:");

        for (i32 i = 0; i < #arrlen(valid_types); i += 1) {
            io::eprintln("-\t", valid_types[i]);
        }

        return false;
    }

    if strspn(str, valid_strspn[type_index]) != str.len() {
        io::eprintln("Invalid input: '", str, "'. Falling back to 0.");
        return false;
    }

    return true;
}

fn i32_cast(string str) -> i32 {
    bool valid = is_valid(str, "i32");

    if valid {
        return atoi(str);
    } else {
        return 0;
    }
}

fn i64_cast(string str) -> i64 {
    bool valid = is_valid(str, "i64");

    if valid {
        return atol(str);
    } else {
        return 0;
    }
}

fn f64_cast(string str) -> f64 {
    bool valid = is_valid(str, "f64");

    if valid {
        return atof(str);
    } else {
        return 0.0;
    }
}
