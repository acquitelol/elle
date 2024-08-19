use std/io;

fn add(ElleMeta meta, ...) {
    variadic args[meta.arity];
    i32 res = 0;

    for i32 i = 0; i < meta.arity; i += 1 {
        res += args.yield(i32);
    }

    return res;
}

fn main() {
    i32 sum = add(1, 2, 3, 4, 5);
    io::dbg(sum);
}
