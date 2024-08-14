use std/io;

fn main(i32 argc, string *argv) {
    for i32 i = 0; i < argc; i += 1 {
        printf("argv[%d] = %s\n", i, argv[i]);
    }
}
