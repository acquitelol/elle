global pub;

external fn srand(i32 seed);
external fn rand() -> i32;

fn random(i32 lower, i32 upper) {
    return rand() % (upper - lower + 1) + lower;
}
