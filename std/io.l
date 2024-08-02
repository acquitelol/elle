use std/core@{malloc, free};
use std/string@{strlen, strcpy};

pub const long BUF_SIZE = 1024;

pub external fn fdopen(long descriptor, string mode) -> long;

pub const long stdin = (long)fdopen(0, "r");
pub const long stdout = (long)fdopen(1, "w");
pub const long stderr = (long)fdopen(2, "w");

pub external fn printf(string formatter, ...) -> int;
pub external fn fprintf(long fd, string formatter, ...) -> int;
pub external fn sprintf(string str, string formatter, ...) -> int;
pub external fn snprintf(string str, int size, string formatter, ...);

pub external fn fscanf(long stream, string format, ...) -> int;
pub external fn scanf(string format, ...) -> int;
pub external fn sscanf(string str, string format, ...) -> int;

pub external fn fgetc(long stream) -> int;
pub external fn fgets(string str, int n, long stream) -> string;
pub external fn fputc(char character, long stream) -> int;
pub external fn fputs(string str, long stream) -> int;

pub external fn fflush(long stream) -> int;

pub external fn getc(long stream) -> int;
pub external fn getchar() -> int;
pub external fn gets(string str) -> string;

pub external fn putc(char character, long stream) -> int;
pub external fn putchar(char character) -> int;
pub external fn puts(string str) -> int;

pub external fn fopen(string filename, string mode) -> long;
pub external fn fclose(long stream) -> int;
pub external fn fread(long ptr, long size, long nmemb, long stream) -> long;
pub external fn fwrite(long ptr, long size, long nmemb, long stream) -> long;

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
