global pub;

struct FILE {
    i32 fd;                 // File descriptor
    string buffer;          // Buffer for file I/O
    i64 buffer_size;        // Size of the buffer
    i64 position;           // Current position in the buffer
    i64 file_size;          // Total size of the file
    i32 mode;               // Mode in which the file is opened
};

local external fn fdopen(i64 descriptor, string mode) -> FILE *;

const FILE *stdin = fdopen(0, "r");
const FILE *stdout = fdopen(1, "w");
const FILE *stderr = fdopen(2, "w");

external fn printf(string formatter, ...) @alias("io::cprintf") -> i32;
external fn fprintf(FILE *fd, string formatter, ...) @alias("io::fprintf") -> i32;
external fn sprintf(string str, string formatter, ...) @alias("io::sprintf") -> i32;
external fn snprintf(string str, i32 size, string formatter, ...) @alias("io::snprintf");

external fn fscanf(FILE *stream, string format, ...) @alias("io::fscanf") -> i32;
external fn scanf(string format, ...) @alias("io::scanf") -> i32;
external fn sscanf(string str, string format, ...) @alias("io::sscanf") -> i32;

external fn fgetc(FILE *stream) @alias("io::fgetc") -> i32;
external fn fgets(string str, i32 n, i64 stream) @alias("io::fgets") -> string;
external fn fputc(char character, i64 stream) @alias("io::fputc") -> i32;
external fn fputs(string str, i64 stream) @alias("io::fputs") -> i32;

external fn fflush(FILE *stream) @alias("io::fflush") -> i32;

external fn getc(FILE *stream) @alias("io::getc") -> i32;
external fn getchar() @alias("io::getchar") -> i32;
external fn gets(string str) @alias("io::gets") -> string;

external fn putc(char character, i64 stream) @alias("io::putc") -> i32;
external fn putchar(char character) @alias("io::putchar") -> i32;
external fn puts(string str) @alias("io::puts") -> i32;

external fn fopen(string filename, string mode) @alias("io::fopen") -> i64;
external fn fclose(FILE *stream) @alias("io::fclose") -> i32;
external fn fread(void *ptr, i64 size, i64 nmemb, FILE *stream) @alias("io::fread") -> i64;
external fn fwrite(void *ptr, i64 size, i64 nmemb, FILE *stream) @alias("io::fwrite") -> i64;

namespace io;
