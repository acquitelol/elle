use std/io;
use std/math;

fn get_e() -> f64 {
    f64 res = 0.0;

    for i32 i = 0; i < 21; i++ {
        res += 1.0 / fact(i);
    }

    return res;
}

fn main() {
    f64 e = get_e();
    printf("e = %.50f\n", e);
}
