use std/io;
use std/collections/array<i32>;

fn main() {
    Array array = Array::new();
    defer array.free();

    array.push(39);
    array.push(16);
    array.push(-1);
    array.push(172);
    array.push(53);

    for i32 i = 0; i < arr.len(); i += 1 {
        io::printf("array[{}] = {}", i, arr.elements[i]);
    }
}
