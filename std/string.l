use std/libc/string;
use std/libc/mem;
use std/libc/io;

global pub;

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

// Note: these are null terminated strings, which means this
// function returns a CLONE of the slice, NOT a string view
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

// Concatenates all of the arguments into 1 final string
//
// examples:
//
// "a".concat("b").equals("ab")
// string::concat("me", "ow").equals("meow")
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

// Concatenates all of the strings provided and then finally reassigns
// the original string to the new string
//
// examples:
//
// string a = "a";
// a.extend("b");
// a.equals("ab");
//
// string a = "me";
// string::extend(a, "ow");
// a.equals("meow")
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

// Repeats a string `count` amount of times
//
// examples:
//
// ```
// io::assert("a".repeat(4).equals("aaaa"))
// io::assert(string::repeat("e", 2).equals("ee"))
// ```
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

// Pops the last character off of a string and returns it
//
// This function mutates the string in-place
//
// examples:
//
// ```
// string a = "mew";
// char last = a.pop();
//
// io::assert(last == 'w', nil)
// io::assert(a.equals("me"), nil)
// ```
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

// !! Do not use this unless you know what you're doing !!
// If you want to turn a value of arbitrary type to a string
// please use string::as_string or string::as_string_wrapped
fn __to_string(string ty, i64 args, bool wrap) {
    string res = nil;
    string fmt = nil;
    i32 length = 0;

    if ty.contains("*") {
        i64 arg = args.yield(i64);
        fmt = "%p";

        length = io::snprintf(nil, 0, fmt, arg);
        res = malloc(length);
        io::sprintf(res, fmt, arg);
        return res;
    }

    if ty.starts_with("string") {
        string arg = args.yield(string);

        if wrap {
            arg = string::concat("\"", arg, "\"");
        }

        res = malloc(arg.len() + 1);
        strcpy(res, arg);
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
        i64 arg = args.yield(i64);
        fmt = "%p";

        length = io::snprintf(nil, 0, fmt, arg);
        res = malloc(length);
        io::sprintf(res, fmt, arg);
        return res;
    }

    return res;
}

fn string::as_string(ElleMeta meta, ...) {
    variadic args[meta.arity];
    return __to_string(meta.types[0], args, false);
}

fn string::as_string_wrapped(ElleMeta meta, ...) {
    variadic args[meta.arity];
    return __to_string(meta.types[0], args, true);
}

// !! Do not use this unless you know what you're doing !!
// If you want to format arguments of arbitrary types
// into a new string, use string::format() instead
fn __internal_fmt(string fmt, ElleMeta meta, i64 args) {
    string result = malloc(fmt.len());

    i32 arg = 0;
    bool is_open = false;
    bool wrapped = false;

    for i32 i = 0; i < fmt.len(); i += 1 {
        if is_open {
            // Allow for escaping { using {{}
            if fmt[i] == '{' {
                is_open = false;

                // Push '{' as it was skipped the first time
                result.push(fmt[i]);
                continue;
            }

            // {:} enables wrapped mode which means
            // that strings are printed as "str" not str
            // and chars are printed as 'c' not c
            if fmt[i] == ':' {
                wrapped = true;
                continue;
            }

            if fmt[i] != '}' {
                io::fprintf(stderr, "Invalid format at position '%d' (%c)\n", i, fmt[i]);
                io::fflush(stderr);
                return "";
            }

            arg += 1;
            result.extend(__to_string(meta.types[arg], args, wrapped));

            is_open = false;
            wrapped = false;
            continue;
        }

        if fmt[i] == '{' {
            is_open = true;
            continue;
        }

        result.push(fmt[i]);
    }

    return result;
}

fn string::format(ElleMeta meta, string fmt, ...) -> string {
    variadic args[meta.arity - 1];
    return __internal_fmt(fmt, meta, args);
}

fn string::ascii_lower() {
    return "abcdefghijklmnopqrstuvwxyz";
}

fn string::ascii_upper() {
    return string::ascii_lower().to_upper();
}

fn string::ascii_numerics() {
    return "0123456789";
}

fn string::ascii_all() {
    return string::format(
        "{}{}{}",
        string::ascii_lower(),
        string::ascii_upper(),
        string::ascii_numerics()
    )
}
