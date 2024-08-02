use std/core@{malloc, free};
use std/string@{strlen, strcpy};

pub const i64 BUF_SIZE = 1024;

pub external fn fdopen(i64 descriptor, string mode) -> i64;

// FILE struct doesn't exist yet
pub const void *stdin = (void *)fdopen(0, "r");
pub const void *stdout = (void *)fdopen(1, "w");
pub const void *stderr = (void *)fdopen(2, "w");

pub external fn printf(string formatter, ...) -> i32;
pub external fn fprintf(void *fd, string formatter, ...) -> i32;
pub external fn sprintf(string str, string formatter, ...) -> i32;
pub external fn snprintf(string str, i32 size, string formatter, ...);

pub external fn fscanf(void *stream, string format, ...) -> i32;
pub external fn scanf(string format, ...) -> i32;
pub external fn sscanf(string str, string format, ...) -> i32;

pub external fn fgetc(void *stream) -> i32;
pub external fn fgets(string str, i32 n, i64 stream) -> string;
pub external fn fputc(char character, i64 stream) -> i32;
pub external fn fputs(string str, i64 stream) -> i32;

pub external fn fflush(void *stream) -> i32;

pub external fn getc(void *stream) -> i32;
pub external fn getchar() -> i32;
pub external fn gets(string str) -> string;

pub external fn putc(char character, i64 stream) -> i32;
pub external fn putchar(char character) -> i32;
pub external fn puts(string str) -> i32;

pub external fn fopen(string filename, string mode) -> i64;
pub external fn fclose(void *stream) -> i32;
pub external fn fread(void *ptr, i64 size, i64 nmemb, void *stream) -> i64;
pub external fn fwrite(void *ptr, i64 size, i64 nmemb, void *stream) -> i64;

pub fn input(string message) -> string {
    string buf = malloc(BUF_SIZE * #size(char));
    defer free(buf);

    printf(message);
    fgets(buf, BUF_SIZE, stdin);
    buf[strlen(buf) - 1] = '\0';

    string result = malloc(strlen(buf) + 1);
    strcpy(result, buf);
    return result;
}
