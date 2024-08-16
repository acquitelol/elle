global pub;

external fn malloc(i64 size) -> i64;
external fn snprintf(string str, i32 size, string formatter, ...);
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

fn streq(string str1, string str2) {
    return str1 == str2 || strcmp(str1, str2) == 0;
}

fn strprefix(string str, string pre) {
    return strncmp(pre, str, strlen(pre)) == 0;
}

fn concat(ElleMeta meta, ...) {
    variadic args[meta.arity];
    string strings[meta.arity];
    i32 sizes[meta.arity];
    i64 length = 0;

    // Collect the strings and the final string length
    for i32 i = 0; i < meta.arity; i += 1 {
        strings[i] = args.yield(string);
        sizes[i] = strlen(strings[i]);
        length += sizes[i] + 1;
    }

    string result = malloc((length + 1) * #size(char));
    i32 index = 0;

    // Construct the final string
    for i32 i = 0; i < meta.arity; i += 1 {
        string current = strings[i];

        for i32 j = 0; j < sizes[i]; j += 1 {
            result[index] = current[j];
            index += 1;
        }

        result[index] = '\n';
        index += 1;
    }

    // Include null terminator
    result[index] = '\0';
    return result;
}

fn to_string(string ty, i64 args, bool wrap) {
    string res = nil;
    string fmt = nil;
    i32 length = 0;

    if strstr(ty, "*") {
        // Ensure the ty is `char *` but isn't `char **`
        // Any variation (`char ***`) will also match
        // so `**` is all that's necessary
        if strprefix(ty, "char") && !strstr(ty, "**") {
            string arg = args.yield(string);

            if wrap {
                fmt = "\"%s\"";
            } else {
                fmt = "%s";
            }

            length = snprintf(nil, 0, fmt, arg);
            res = malloc(length);
            sprintf(res, fmt, arg);
            return res;
        }

        i64 arg = args.yield(i64);
        fmt = "%p";

        length = snprintf(nil, 0, fmt, arg);
        res = malloc(length);
        sprintf(res, fmt, arg);
        return res;
    }

    if strprefix(ty, "i32") {
        i32 arg = args.yield(i32);
        fmt = "%d";

        length = snprintf(nil, 0, fmt, arg);
        res = malloc(length);
        sprintf(res, fmt, arg);
    }

    if strprefix(ty, "i64") {
        i64 arg = args.yield(i64);
        fmt = "%ld";

        length = snprintf(nil, 0, fmt, arg);
        res = malloc(length);
        sprintf(res, fmt, arg);
    }

    if strprefix(ty, "f32") || strprefix(ty, "f64") {
        f64 arg = 0;
        fmt = "%f";

        if strprefix(ty, "f32") {
            arg = (f64)args.yield(f32);
        } else {
            arg = args.yield(f64);
        }

        length = snprintf(nil, 0, fmt, arg);
        res = malloc(length);
        sprintf(res, fmt, arg);
    }

    if strprefix(ty, "char") {
        char arg = args.yield(char);

        if wrap {
            fmt = "'%c'";
        } else {
            fmt = "%c";
        }

        length = snprintf(nil, 0, fmt, arg);
        res = malloc(length);
        sprintf(res, fmt, arg);
    }

    if strprefix(ty, "bool") {
        res = args.yield(bool).to_string();
    }

    // We never yielded any string, ie the caller passed a struct
    // TODO: Arrays would just print the pointer %p for now, however
    // there is a way to print the array properly, for example
    // we can pass the array's length (or nil if it isn't an array)
    // to ElleMeta at each index then we can return [%s, %s...]
    // and to_string() each member of the array if it an array
    if res == nil {
        fprintf(stderr, "Unimplemented expression type '%s'\n", $$...$$, ty);
    }

    return res;
}
