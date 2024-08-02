use std/io;

fn add(i32 size, ...) {
    variadic args[size * #size(i32)];
    i32 res = 0;

    for i32 i = 0; i < size; i++ {
        res += args yield i32;
    }

    return res;
}

fn main() {
    i32 res = add.(1, 2, 3, 4, 5);
    printf("sum = %d\n", res);
}
