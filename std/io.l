use std/libc/io;
use std/string;
use std/core;

global pub;

local fn __print(i64 args, ElleMeta meta, bool wrap, bool full, bool newline) {
    for i32 i = 0; i < meta.arity; i += 1 {
        string ty = meta.types[i];
        string expr = meta.exprs[i];
        string res = __to_string(ty, args, wrap);

        if full {
            io::cprintf("%s = %s\n", expr, res);
        } else {
            io::cprintf("%s", res);
        }

        free(res); // Don't leak memory
    }

    if newline {
        io::putchar('\n');
    }
}

local fn __err(i64 args, ElleMeta meta, bool newline) {
    FILE *stderr = fdopen(2, "w");
    string fmt = "\x1b[31m[ERROR]: %s\x1b[0m";
    string error = "";

    for i32 i = 0; i < meta.arity; i += 1 {
        string ty = meta.types[i];
        string expr = meta.exprs[i];
        string res = __to_string(ty, args, false);

        error = string::concat(error, res);
        free(res); // Don't leak memory
    }

    io::fprintf(stderr, fmt, error);
    io::fflush(stderr);

    if meta.arity > 0 {
        free(error);
    }

    if newline {
        io::putchar('\n');
    }
}

fn io::dbg(ElleMeta meta, ...) {
    variadic args[meta.arity];
    __print(args, meta, true, true, false);
}

fn io::print(ElleMeta meta, ...) {
    variadic args[meta.arity];
    __print(args, meta, false, false, false);
}

fn io::println(ElleMeta meta, ...) {
    variadic args[meta.arity];
    __print(args, meta, false, false, true);
}

fn io::eprint(ElleMeta meta, ...) {
    variadic args[meta.arity];
    __err(args, meta, false);
}

fn io::eprintln(ElleMeta meta, ...) {
    variadic args[meta.arity];
    __err(args, meta, true);
}

fn io::printf(ElleMeta meta, string fmt, ...) {
    variadic args[meta.arity - 1];
    io::println(__internal_fmt(fmt, meta, args));
}

fn io::assert(ElleMeta meta, bool condition, string message) {
    if !condition {
        string parsed = message || "No additional information was specified.";

        io::eprintln(
            meta.caller, ": Assertion '", meta.exprs[0], "' failed.\n",
            parsed.color("italic").reset()
        );

        abort();
    }
}

fn io::dbg_typeof(ElleMeta meta, ...) {
    for i32 i = 0; i < meta.arity; i += 1 {
        io::printf("typeof \"{}\" = {}", meta.exprs[i], meta.types[i]);
    }
}

const i64 BUF_SIZE = 1024;

fn io::input(string message) -> string {
    string buf = (string)malloc(BUF_SIZE * #size(char));
    defer free(buf);

    io::print(message);
    io::fgets(buf, BUF_SIZE, stdin);

    // I would rather O(n) than O(2n)
    i32 length = buf.len();

    buf[buf.len() - 1] = '\0';
    string result = (string)malloc(buf.len() + 1);
    strcpy(result, buf);

    return result;
}
