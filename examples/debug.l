use std/io;

fn main() {
    string foo = "this is a string";
    i32 a = 16;
    f32 b = 1.2;
    bool c = false;

    dbg(foo, a, b, c, &a, 'z', 1e5, true);
}
