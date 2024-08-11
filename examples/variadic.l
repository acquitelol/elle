use std/io;

fn add(ElleMeta meta, ...) {
    variadic args[meta.arity * #size(i32)];
    i32 res = 0;

    for i32 i = 0; i < meta.arity; i++ {
        res += args yield i32;
    }

    return res;
}

fn main() {
    i32 res = add(1, 2, 3, 4, 5);
    printf("sum = %d\n", res);
}
