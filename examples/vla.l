use std/collections/array;
use std/io;

fn main() {
    Array<i32> a = Array::new(1, 2, 3);
    defer a.free();

    a.push(39);
    a.push(16);
    a.push(-1);

    Array<string> b = Array::new();
    defer b.free();

    b.push("hello");
    b.push("world");
    b.push("meow");
    b.push("abcd");

    a.dbg();
    io::print('\n');

    Array<i32> c = Array::new();
    defer c.free();

    // Test extension
    c.push(172);
    c.push(53);
    a.extend(c);

    // Test logging
    a.dbg();
    io::print('\n');

    b.dbg();
    io::print('\n');

    // Test popping
    io::dbg(b.pop(), b.len(), b.capacity);
    io::print('\n');

    // Test to_string
    io::println(b.to_string());
    io::println(b.join(", "));
    io::print('\n');

    // Test clearing
    b.clear();
    b.push("a");
    io::dbg(b.to_string());
    io::print('\n');

    // Test from_string
    Array<char> hello_world = Array::from_string("hello world!");
    io::dbg(hello_world.join(""));
}
