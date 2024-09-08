use std/io;

fn main() {
    i32 i = 0;
    defer io::println(i);

    {
        defer io::println(i);
        i += 2;
    }

    i *= i;
}
