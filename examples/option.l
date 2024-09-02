use std/option<string>;
use std/io;

fn get_something(i32 input) {
    if (input == 3) {
        return None;
    }

    return Some("meow");
}

fn main() {
    Option a = get_something(1);
    Option b = get_something(3);

    io::assert(a.unwrap_or("").equals("meow"), nil);
    io::assert(b.unwrap_or("Fallback :3").equals("Fallback :3"), nil);

    io::println("All option tests have passed!".color("green").reset());
}
