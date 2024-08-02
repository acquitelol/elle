use std/io;
use std/rand;
use std/time;
use std/cast;

fn main() {
    srand(time(NULL));

    i32 lower = int_cast(input("Enter the lower bound: "));
    i32 upper = int_cast(input("Enter the upper bound: "));

    printf("Rolled %d\n", random(lower, upper));
}
