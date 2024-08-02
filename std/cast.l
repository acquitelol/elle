use std/io@{printf, fprintf, stderr};
use std/core@{atol, atof};
use std/string@{strspn, strlen, streq};

fn is_valid(string str, string type) -> bool {
    string *valid_strspn = ["0123456789", "0123456789", "0123456789."];
    string *valid_types = ["i32", "i64", "f64"];
    i32 type_index = EOF;

    for i32 i = 0; i < #arrlen(valid_types); i++ {
        if streq(valid_types[i], type) {
            type_index = i;
        }
    }

    if type_index == EOF {
        printf("'%s' is not a valid type for casting. Falling back to 0\n", type);
        printf("Valid types:\n");

        for (i32 i = 0; i < #arrlen(valid_types); i++) {
            printf("-\t%s\n", valid_types[i]);
        }

        return false;
    }

    if strspn(str, valid_strspn[type_index]) != strlen(str) {
        fprintf(stderr, "Invalid input: '%s'. Falling back to 0\n", str);
        return false;
    }

    return true;
}

pub fn int_cast(string str) -> i32 {
    bool valid = is_valid(str, "i32");

    if valid {
        return atoi(str);
    } else {
        return 0;
    }
}

pub fn long_cast(string str) -> i64 {
    bool valid = is_valid(str, "i64");

    if valid {
        return atol(str);
    } else {
        return 0;
    }
}

pub fn double_cast(string str) -> f64 {
    bool valid = is_valid(str, "f64");

    if valid {
        return atof(str);
    } else {
        return 0.0;
    }
}
