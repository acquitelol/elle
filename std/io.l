use std/core;
use std/string;

global pub;

const i64 BUF_SIZE = 1024;

def FILE {
    i32 fd;                 // File descriptor
    string buffer;          // Buffer for file I/O
    i64 buffer_size;        // Size of the buffer
    i64 position;           // Current position in the buffer
    i64 file_size;          // Total size of the file
    i32 mode;               // Mode in which the file is opened
}

external fn fdopen(i64 descriptor, string mode) -> FILE *;

const FILE *stdin = fdopen(0, "r");
const FILE *stdout = fdopen(1, "w");
const FILE *stderr = fdopen(2, "w");

external fn printf(string formatter, ...) -> i32;
external fn fprintf(FILE *fd, string formatter, ...) -> i32;
external fn sprintf(string str, string formatter, ...) -> i32;
external fn snprintf(string str, i32 size, string formatter, ...);

external fn fscanf(FILE *stream, string format, ...) -> i32;
external fn scanf(string format, ...) -> i32;
external fn sscanf(string str, string format, ...) -> i32;

external fn fgetc(FILE *stream) -> i32;
external fn fgets(string str, i32 n, i64 stream) -> string;
external fn fputc(char character, i64 stream) -> i32;
external fn fputs(string str, i64 stream) -> i32;

external fn fflush(FILE *stream) -> i32;

external fn getc(FILE *stream) -> i32;
external fn getchar() -> i32;
external fn gets(string str) -> string;

external fn putc(char character, i64 stream) -> i32;
external fn putchar(char character) -> i32;
external fn puts(string str) -> i32;

external fn fopen(string filename, string mode) -> i64;
external fn fclose(FILE *stream) -> i32;
external fn fread(void *ptr, i64 size, i64 nmemb, FILE *stream) -> i64;
external fn fwrite(void *ptr, i64 size, i64 nmemb, FILE *stream) -> i64;

fn input(string message) -> string {
    string buf = malloc(BUF_SIZE * #size(char));
    defer free(buf);

    printf(message);
    fgets(buf, BUF_SIZE, stdin);
    buf[strlen(buf) - 1] = '\0';

    string result = malloc(strlen(buf) + 1);
    strcpy(result, buf);
    return result;
}

fn dbg(ElleMeta meta, ...) {
    variadic args[meta.arity]; // Arity does NOT include meta

    for i32 i = 0; i < meta.arity; i += 1 {
        string ty = meta.types[i];
        string expr = meta.exprs[i];
        string res = to_string(ty, args, true);

        printf("%s = %s\n", expr, res);
    }
}

fn print(ElleMeta meta, ...) {
    variadic args[meta.arity]; // Arity does NOT include meta

    for i32 i = 0; i < meta.arity; i += 1 {
        string ty = meta.types[i];
        string expr = meta.exprs[i];
        string res = to_string(ty, args, false);

        if i == meta.arity - 1 {
            puts(res);
        } else {
            printf("%s ", res);
        }
    }
}
