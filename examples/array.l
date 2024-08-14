use std/io;
use std/math;

fn other(i64 *arr, i32 val, i32 size) {
    printf(
        "\narr[0] = %ld\nval = %d\nsize = %d\n",
        arr[0], val, size
    );
}

fn main() {
    i64 *test = [I64_MAX, I64_MIN, -39];

    for i32 i = 0; i < #arrlen(test); i += 1 {
        printf("test[%d] = %ld\n", i, test[i]);
    }

    test[2] *= 10;
    printf("\ntest[2] * 10 = %ld\n", test[2]);

    other([I64_MAX], [123][0], #size([1, 2, 3]));
}
