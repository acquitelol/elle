pub external fn srand(i32 seed);
pub external fn rand() -> i32;

pub fn random(i32 lower, i32 upper) {
    return rand() % (upper - lower + 1) + lower;
}
