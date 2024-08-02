use std/io;

fn main() {
    long a = 0xDEADBEEF;
    int b = 0o273451456;
    int c = 0b111010011011111010010100101;
    long d = 1.2e9;
    double e = 2.7182818e2;

    printf(
        "a = %X\nb = %d\nc = %d\nd = %ld\ne = %f\n",
        a, b, c, d, e
    );
}
