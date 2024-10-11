use std/collections/array;
use std/io;

fn main() {
    Array<i32> *a = Array::new(1, 2);
    defer a.free();

    a.push(39);
    a.push(-1);

    Array<string> *b = Array::new();
    defer b.free();

    b.push("hello");
    b.push("world");
    b.push("meow");
    b.push("abcd");

    a.dbg();
    io::print('\n');

    Array<i32> *c = Array::new(172);
    defer c.free();

    // Test extension
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
    Array<char> *hello_world = Array::from_string("hello world!");
    defer hello_world.free();

    io::dbg(hello_world.join(""));

    // Test contains
    Array<string> *d = Array::new("a", "b", "c");
    defer d.free();

    io::dbg(d.contains("a"), d.contains("foo"));

    // Generates [0, 1, 2, 3, 4]
    Array<i32> *e = Array::range(0, 5);
    defer e.free();

    // Squares every number in `e`
    Array<i32> *e_mapped = e.map(fn(i32 x) -> x * x);
    defer e_mapped.free();

    io::print('\n');
    e_mapped.dbg();

    Array<i32> *empty = Array::range(0, 0);
    defer empty.free();

    io::print('\n');
    io::dbg(empty.len());
    io::assert(
        empty.len() == 0,
        "The length of Array::range(0, 0) should be 0"
    );
}
