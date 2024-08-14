use std/io;
use std/math;

fn main() {
    f64 x = 2;
    f64 a = x;

    for i32 i = 0; i < 100; i += 1 {
        a = a - (a * a - x) / (2 * a);
    }

    printf("sqrt(%.0f) ≈ %.50f\n", x, a);
    printf("precision: %.50f\n", sqrt(x) - a);
}
