use std/io;
use std/rand;
use std/time;
use std/cast;

fn main() {
    srand(time(NULL));

    int lower = int_cast(input("Enter the lower bound: "));
    int upper = int_cast(input("Enter the upper bound: "));

    printf("Rolled %d\n", random(lower, upper));
}
