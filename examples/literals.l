use std/io;

fn main() {
    i64 *a = 0xDEADBEEF;
    i32 b = 0o273451456;
    i32 c = 0b111010011011111010010100101;
    i64 d = 1.2e9;
    f64 e = 2.7182818e2;

    dbg(a, b, c, d, e);
}
