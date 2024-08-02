use std/io;
use std/math;

fn other(long *arr, int val, int size) {
    printf(
        "\narr[0] = %ld\nval = %d\nsize = %d\n",
        arr[0], val, size
    );
}

fn main() {
    long *test = [LONG_MAX, LONG_MIN, -39];

    for (int i = 0; i < #arrlen(test); i++) {
        printf("test[%d] = %ld\n", i, test[i]);
    }

    other([LONG_MAX], [123][0], #size([1, 2, 3]));
}
