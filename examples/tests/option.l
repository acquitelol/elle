use std/io;
use std/option;

fn get_meow(i32 input) -> Option<string> {
    if (input == 3) {
        return Option::None();
    }

    return Option::Some("meow");
}

fn get_miku_number(string input) -> Option<i32> {
    if input == "mikudayo" {
        return Option::Some(39);
    }

    return Option::None();
}

fn main() {
    Option<string> a = get_meow(1);
    Option<string> b = get_meow(3);

    io::dbg(a.unwrap_or("Fallback :3"));
    io::dbg(b.unwrap_or("Fallback :3"));

    io::assert(a.unwrap_or("Fallback :3") == "meow", nil);
    io::assert(b.unwrap_or("Fallback :3") == "Fallback :3", nil);

    Option<i32> c = get_miku_number("aaa :3");
    Option<i32> d = get_miku_number("mikudayo");

    io::dbg(c.unwrap_or(-1));
    io::dbg(d.unwrap_or(-1));

    io::assert(c.unwrap_or(-1) == -1, nil);
    io::assert(d.unwrap_or(-1) == 39, nil);

    io::println("All option tests have passed!".color("green").reset());
}
