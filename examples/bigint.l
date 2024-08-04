use std/io;
use std/math;

def i128 {
    i64 high;
    i64 low;
}

fn add_i128(i128 a, i128 b) -> i128 {
    i128 result = i128 {
        low = a.low + b.low,
        high = a.high + b.high
    };

    result.high += result.low < a.low;
    return result;
}

fn print_i128(i128 value) {
    if !value.high {
        printf("%llu\n", value.low);
    } else {
        printf("%llu%019llu\n", value.high, value.low);
    }
}

fn main() {
    i128 a = i128 { high = 0, low = I64_MAX };
    i128 b = i128 { high = 0, low = 1 };
    i128 sum = add_i128(a, b);

    printf("Sum: ");
    print_i128(sum);
}
