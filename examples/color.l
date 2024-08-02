use std/io;

fn main() {
    int color = 0xFFABE9FF;

    int r = (color >> 24) & 0xFF;
    int g = (color >> 16) & 0xFF;
    int b = (color >> 8) & 0xFF;
    int a = color & 0xFF;

    printf(
        "r = %u\ng = %u\nb = %u\na = %u\n",
        r, g, b, a
    );
}
