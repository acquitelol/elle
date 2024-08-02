pub external fn srand(int seed);
pub external fn rand() -> int;

pub fn random(int lower, int upper) {
    return rand() % (upper - lower + 1) + lower;
}
