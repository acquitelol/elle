use std/io;
use std/math;

fn other(i64 *arr, i32 val, i32 size) {
    io::dbg(arr[0], val, size);
}

fn main() {
    i64 *test = [i64::max(), i64::min(), -39];

    for i32 i = 0; i < #arrlen(test); i += 1 {
        io::printf("test[%d] = %ld\n", i, test[i]);
    }

    test[2] *= 10;
    io::println("\ntest[2] * 10 = ", test[2], "\n");
    other([i64::max()], [123][0], #size([1, 2, 3]));
}
