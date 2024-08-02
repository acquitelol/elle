use std/io;

fn fact(long n) -> long {
    if n <= 1 {
        return 1;
    }

    return n * fact(n - 1);
}

fn get_e() -> double {
    double res = 0.0;

    for long i = 0; i < 21; i++ {
        res += 1.0 / fact(i);
    }

    return res;
}

fn main() {
    double e = get_e();
    printf("e = %.50f\n", e);
}
