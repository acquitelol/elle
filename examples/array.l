use std/io;
use std/math;

fn other(i64 *arr, i32 val, i32 size) {
    printf(
        "\narr[0] = %ld\nval = %d\nsize = %d\n",
        arr[0], val, size
    );
}

fn main() {
    i64 *test = [LONG_MAX, LONG_MIN, -39];

    for (i32 i = 0; i < #arrlen(test); i++) {
        printf("test[%d] = %ld\n", i, test[i]);
    }

    other([LONG_MAX], [123][0], #size([1, 2, 3]));
}
