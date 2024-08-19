use std/io;

fn main() {
    string foo = "this is a string";
    i32 a = 16;
    f32 b = 1.2;
    bool c = false;
    i32 *arr = [1, 2, 3];

    io::dbg(foo, a, b, c, &a, 'z', 1e5, true, arr, ["a", "meow"]);
    io::print("this is a string literal and ", foo, '\n');
    io::println("this is a string literal and ", foo, " (now with a newline!)\n");
    io::eprintln("i failed to do something... :3");
}
