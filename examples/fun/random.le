use std/rand;
use std/time;
use std/cast;
use std/io;

fn main() {
    rand::seed(time(nil));

    i32 lower = i32::parse(io::input("Enter the lower bound: "));
    i32 upper = i32::parse(io::input("Enter the upper bound: "));

    io::printf("Rolled {}", rand::random(lower, upper));
}
