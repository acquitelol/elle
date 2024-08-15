use std/io;
use std/rand;
use std/time;
use std/cast;

fn main() {
    srand(time(nil));

    i32 lower = i32_cast(input("Enter the lower bound: "));
    i32 upper = i32_cast(input("Enter the upper bound: "));

    printf("Rolled %d\n", random(lower, upper));
}
