use std/io;
use std/rand;
use std/time;
use std/cast;

fn main() {
    srand(time(nil));

    i32 lower = i32_cast(io::input("Enter the lower bound: "));
    i32 upper = i32_cast(io::input("Enter the upper bound: "));

    io::printf("Rolled {}", random(lower, upper));
}
