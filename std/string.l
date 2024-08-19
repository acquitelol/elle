use std/libc/io;
use std/libc/mem;
use std/core;
global pub;

external fn memchr(string str, char character, i64 n) -> i64;
external fn memcmp(string str1, string str2, i64 n) -> i32;
external fn memcpy(void *dest, void *src, i64 n) -> i64;
external fn memmove(void *dest, void *src, i64 n) -> i64;
external fn memset(string str, char character, i64 n) -> i64;

external fn strcat(string dest, string src) -> string;
external fn strncat(string dest, string src, i64 n) -> string;
external fn strchr(string str, char character) -> string;
external fn strcmp(string str1, string str2) -> i32;
external fn strncmp(string str1, string str2, i64 n) -> i32;
external fn strcoll(string str1, string str2) -> i32;
external fn strcpy(string dest, string src) -> string;
external fn strncpy(string dest, string src, i64 n) -> string;
external fn strspn(string str1, string str2) -> i64;
external fn strcspn(string str1, string str2) -> i64;
external fn strerror(i32 errnum) -> string;
external fn strlen(string str) -> i64;
external fn strpbrk(string str1, string str2) -> string;
external fn strrchr(string str, char character) -> string;
external fn strstr(string haystack, string needle) -> string;
external fn strtok(string str, string delim) -> string;
external fn strxfrm(string dest, string src, i64 n) -> i64;

// NOTE: This is for null-terminated strings
// so it may perform differently than you expect
fn string::len(string self) {
    return strlen(self);
}

fn string::is_empty(string self) {
    return self.len() == 0;
}

fn string::contains(string self, string needle) -> bool {
    return strstr(self, needle) != nil;
}

fn string::starts_with(string self, string prefix) -> bool {
    return strncmp(prefix, self, prefix.len()) == 0;
}

fn string::ends_with(string self, string suffix) -> bool {
    i32 self_length = self.len();
    i32 suffix_length = suffix.len();

    if (suffix_length > self_length) {
        return false;
    }

    return strncmp(self + self_length - suffix_length, suffix, suffix_length) == 0;
}

fn string::equals(string self, string other) {
    return self == other || strcmp(self, other) == 0;
}

fn string::to_upper(string self) {
    i32 length = self.len();
    string out = malloc((length + 1) * #size(char));

    for i32 i = 0; i < length; i += 1 {
        char c = self[i];

        if c >= 'a' && c <= 'z' {
            out[i] = c - ('a' - 'A');
        } else {
            out[i] = c;
        }
    }

    return out;
}

fn string::to_lower(string self) {
    i32 length = self.len();
    string out = malloc((length + 1) * #size(char));

    for i32 i = 0; i < length; i += 1 {
        char c = self[i];

        if c >= 'A' && c <= 'Z' {
            out[i] = c + ('a' - 'A');
        } else {
            out[i] = c;
        }
    }

    return out;
}


fn string::ascii_lower() {
    return "abcdefghijklmnopqrstuvwxyz";
}

fn string::ascii_upper() {
    return string::ascii_lower().to_upper();
}

fn string::slice(string self, i32 start, i32 end) -> string {
    i32 length = self.len();

    if start < 0 || end > length || start > end {
        return nil;
    }

    i32 new_length = end - start;
    string sliced = malloc(new_length + 1);

    if !sliced {
        return nil;
    }

    for i32 i = 0; i < new_length; i += 1 {
        sliced[i] = self[start + i];
    }

    sliced[new_length] = '\0';
    return sliced;
}

fn string::concat(ElleMeta meta, ...) -> string {
    variadic args[meta.arity];

    string strings[meta.arity];
    i32 sizes[meta.arity];
    i64 length = 0;

    for i32 i = 0; i < meta.arity; i += 1 {
        strings[i] = args.yield(string);
        sizes[i] = strings[i].len();
        length += sizes[i];
    }

    string result = malloc((length + 1) * #size(char));

    if !result {
        return nil;
    }

    i32 index = 0;

    for i32 i = 0; i < meta.arity; i += 1 {
        string current = strings[i];

        for i32 j = 0; j < sizes[i]; j += 1 {
            result[index] = current[j];
            index += 1;
        }
    }

    result[index] = '\0';
    return result;
}

fn string::extend(ElleMeta meta, string *self, ...) {
    // - 1 to account for `string *self` which isnt variadic
    variadic args[meta.arity - 1];
    string res = *self;

    for i32 i = 0; i < meta.arity - 1; i += 1 {
        res = res.concat(args.yield(string));
    }

    *self = res;
}

fn string::color(string self, string color) {
    string fmt = "\x1b[%dm";
    i32 code = -1;

    if color.equals("bold") {
        code = 1;
    }

    if color.equals("italic") {
        code = 3;
    }

    if color.equals("underline") {
        code = 4;
    }

    if color.equals("red") {
        code = 31;
    }

    if color.equals("green") {
        code = 32;
    }

    if color.equals("yellow") {
        code = 33;
    }

    if color.equals("blue") {
        code = 34;
    }

    if color.equals("magenta") {
        code = 35;
    }

    if color.equals("cyan") {
        code = 36;
    }

    if color.equals("white") {
        code = 37;
    }

    if code == -1 {
        return self;
    }

    string formatting = malloc(fmt.len() + 1);
    io::sprintf(formatting, fmt, code);
    string result = formatting.concat(self);

    return result;
}

fn string::reset(string self) {
    return self.concat("\x1b[0m");
}

fn string::repeat(string self, i32 count) {
    string res = self;

    // count - 1 because we already have 1
    // in the initial declaration
    for i32 i = 0; i < count - 1; i += 1 {
        res = res.concat(self);
    }

    return res;
}

fn string::insert(string *self, i32 index, char c) {
    i32 length = (*self).len();

    if index < 0 || index > length {
        return nil;
    }

    // +1 for the new char, +1 for the null terminator
    string result = malloc((length + 2) * #size(char));

    if !result {
        return nil;
    }

    strncpy(result, *self, index);
    result[index] = c;
    strcpy(result + index + 1, *self + index);

    result[length + 1] = '\0';
    *self = result;
}

fn string::pop(string *self) -> char {
    i32 length = (*self).len();

    if length == 0 {
        return '\0';
    }

    char last_char = (*self)[length - 1];
    string result = malloc(length * #size(char));

    if !result {
        return '\0';
    }

    strncpy(result, *self, length - 1);
    result[length - 1] = '\0';
    *self = result;

    return last_char;
}

fn string::push(string *self, char c) {
    i32 length = (*self).len();
    string result = malloc((length + 2) * #size(char));

    if (!result) {
        return nil;
    }

    strcpy(result, *self);
    result[length] = c;
    result[length + 1] = '\0';

    *self = result;
}

fn to_string(string ty, i64 args, bool wrap) {
    string res = nil;
    string fmt = nil;
    i32 length = 0;

    if ty.contains("*") {
        // Ensure the ty is `char *` but isn't `char **`
        // Any variation (`char ***`) will also match
        // so `**` is all that's necessary
        if ty.starts_with("char") && !ty.contains("**") {
            string arg = args.yield(string);

            if wrap {
                arg = string::concat("\"", arg, "\"");
            }

            res = malloc(arg.len() + 1);
            strcpy(res, arg);
            return res;
        }

        i64 arg = args.yield(i64);
        fmt = "%p";

        length = io::snprintf(nil, 0, fmt, arg);
        res = malloc(length);
        io::sprintf(res, fmt, arg);
        return res;
    }

    if ty.starts_with("i32") {
        i32 arg = args.yield(i32);
        fmt = "%d";

        length = io::snprintf(nil, 0, fmt, arg);
        res = malloc(length);
        io::sprintf(res, fmt, arg);
    }

    if ty.starts_with("i64") {
        i64 arg = args.yield(i64);
        fmt = "%ld";

        length = io::snprintf(nil, 0, fmt, arg);
        res = malloc(length);
        io::sprintf(res, fmt, arg);
    }

    if ty.starts_with("f32") || ty.starts_with("f64") {
        f64 arg = 0;
        fmt = "%f";

        if ty.starts_with("f32") {
            arg = (f64)args.yield(f32);
        } else {
            arg = args.yield(f64);
        }

        length = io::snprintf(nil, 0, fmt, arg);
        res = malloc(length);
        io::sprintf(res, fmt, arg);
    }

    if ty.starts_with("char") {
        char arg = args.yield(char);

        if wrap {
            fmt = "'%c'";
        } else {
            fmt = "%c";
        }

        res = malloc(length);
        io::sprintf(res, fmt, arg);
    }

    if ty.starts_with("bool") {
        // Even though this yields a string we need to
        // copy it into a heap-allocated string
        // so that the caller can reliably free it
        string arg = args.yield(bool).to_string();
        fmt = "%s";

        res = malloc(arg.len() + 1);
        io::sprintf(res, fmt, arg);
    }

    if res == nil {
        io::fprintf(stderr, "Unimplemented expression type '%s'\n", ty);
    }

    return res;
}
