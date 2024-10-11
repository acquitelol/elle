use std/math;
use std/io;

fn other(i64 *arr, i32 val, i32 size) {
    io::dbg(arr[0], val, size);
}

// We can get the size of arrays in other functions now
fn len_in_other_fn(i64 *arr) {
    io::printf("The length of the array is {}", #len(arr));
}

fn main() {
    i64 *test = [i64::max(), i64::min(), -39];

    for i32 i = 0; i < #len(test); i += 1 {
        io::printf("test[{}] = {}", i, test[i]);
    }

    len_in_other_fn(test);

    test[2] *= 10;
    io::printf("\ntest[2] * 10 = {}\n", test[2]);
    other([i64::max()], [123][0], #size([1, 2, 3]));
}
