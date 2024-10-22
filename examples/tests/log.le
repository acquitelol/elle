use std/io;

struct Foo {
    i32 a;
};

fn main() {
    string foo = "this is a string";
    i32 a = 16;
    f32 b = 1.2;
    bool c = false;
    i32 *arr = [1, 2, 3];

    io::dbg(foo, a, b, c, &a, 'z', 1e5, true, arr, ["a", "meow"]);
    io::dbg(Foo { a = 5 });
    io::print("this is a string literal and ", foo, '\n');
    io::println("this is a string literal and ", foo, " (now with a newline!)");

    // extra newline (printf adds a newline automatically)
    io::printf("meow? i think that is so {}! {}!!!\n", (bool)1, true);

    io::eprintln("i failed to do something... :3");
}
