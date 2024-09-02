use std/math;
use std/io;

fn main() {
    f64 x = 2;
    f64 a = x;

    for i32 i = 0; i < 100; i += 1 {
        a = a - (a * a - x) / (2 * a);
    }

    io::printf("sqrt({}) ≈ {}", x, a);
    io::printf("precision: {}", sqrt(x) - a);
}
