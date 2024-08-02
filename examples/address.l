use std/io;

fn other(int *a, string *str) {
    printf("(fn other)\n\ta = %d\n\tstr = %s\n", *a, *str);
    *a = 542;
}

fn main() {
    int a = 39;
    string str = "Hello world!";

    other(&a, &str);
    printf("(fn main)\n\ta = %d\n", a);
}
