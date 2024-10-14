use std/core;
use std/io;
global pub;

fn bool::parse(string str) {
    if str == "true" {
        return true;
    }

    return false;
}

fn bool::try_parse(string str, bool *res) {
    bool valid = str == "true" || str == "false";

    if valid {
        *res = bool::parse(str);
        return true;
    }

    return false;
}

local fn is_valid(string str, string type) -> bool {
    string *valid_strspn = ["-0123456789", "-0123456789", "-0123456789."];
    string *valid_types = ["i32", "i64", "f64"];
    i32 type_index = -1;

    for i32 i = 0; i < #len(valid_types); i += 1 {
        if valid_types[i] == type {
            type_index = i;
        }
    }

    if type_index == -1 {
        io::eprintln(type, " is not a valid type for casting. Falling back to 0", type);
        io::eprintln("Valid types:");

        for (i32 i = 0; i < #len(valid_types); i += 1) {
            io::eprintln("-\t", valid_types[i]);
        }

        return false;
    }

    if strspn(str, valid_strspn[type_index]) != str.len() {
        return false;
    }

    return true;
}

local fn is_valid_with_err(string str, string type) -> bool {
    bool res = is_valid(str, type);

    if !res {
        io::eprintln("Invalid input: '", str, "'. Falling back to 0.");
    }

    return res;
}

fn i32::parse(string str) -> i32 {
    bool valid = is_valid_with_err(str, "i32");

    if valid {
        return atoi(str);
    }

    return 0;
}

fn i32::try_parse(string str, i32 *res) -> bool {
    bool valid = is_valid(str, "i32");

    if valid {
        *res = atoi(str);
        return true;
    }

    return false;
}

fn i64::parse(string str) -> i64 {
    bool valid = is_valid_with_err(str, "i64");

    if valid {
        return atol(str);
    }

    return 0;
}

fn i64::try_parse(string str, i64 *res) -> bool {
    bool valid = is_valid(str, "i64");

    if valid {
        *res = atol(str);
        return true;
    }

    return false;
}

fn f32::parse(string str) -> f32 {
    bool valid = is_valid_with_err(str, "f64");

    if valid {
        return (f32)atof(str);
    }

    return 0;
}

fn f32::try_parse(string str, f32 *res) -> bool {
    bool valid = is_valid(str, "f64");

    if valid {
        *res = (f32)atof(str);
        return true;
    }

    return false;
}

fn f64::parse(string str) -> f64 {
    bool valid = is_valid_with_err(str, "f64");

    if valid {
        return atof(str);
    }

    return 0;
}

fn f64::try_parse(string str, f64 *res) -> bool {
    bool valid = is_valid(str, "f64");

    if valid {
        *res = atof(str);
        return true;
    }

    return false;
}
