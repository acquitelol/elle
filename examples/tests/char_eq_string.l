use std/io;

fn main() {
    io::assert('a' == "a", nil); // Will use 'a' == "a"[0]
    io::assert("a" != 'b', nil); // Will use "a"[0] == 'b'
    io::assert("a" == 'a', nil); // Will use "a"[0] == 'a'
    io::assert('a' == "abc", nil); // Will use 'a' == "abc"[0]

    io::println("All string to character tests have passed!".color("green").reset());
}
