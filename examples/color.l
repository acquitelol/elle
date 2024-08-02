use std/io;

fn main() {
    i32 color = 0xFFABE9FF;

    i32 r = (color >> 24) & 0xFF;
    i32 g = (color >> 16) & 0xFF;
    i32 b = (color >> 8) & 0xFF;
    i32 a = color & 0xFF;

    printf(
        "r = %u\ng = %u\nb = %u\na = %u\n",
        r, g, b, a
    );
}
