use std/io;

fn other(i32 *a, string *str) {
    printf("(fn other)\n\ta = %d\n\tstr = %s\n", *a, *str);
    *a = 542;
}

fn main() {
    i32 a = 39;
    string str = "Hello world!";

    other(&a, &str);
    printf("(fn main)\n\ta = %d\n", a);
}
