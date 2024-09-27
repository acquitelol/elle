global pub;

struct rand @namespace {};

external fn srand(i32 seed) @alias("rand::seed");
external fn rand() @alias("rand::rand") -> i32;

fn rand::random(i32 lower, i32 upper) {
    return rand::rand() % (upper - lower + 1) + lower;
}
