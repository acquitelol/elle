use std/string;
use std/math;
use std/io;

fn main() {
    string a = string::ascii_lower();

    io::assert(
        a.to_upper() == "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
        string::format("result = \"{}\"", a.to_upper())
    );

    a.insert(11, '9');

    io::assert(
        a == "abcdefghijk9lmnopqrstuvwxyz",
        string::format("result = \"{}\"", a)
    );

    string b = "";
    io::assert(b.is_empty(), "b should be empty");
    io::assert(string::is_empty(b), "b should be empty");

    string c = "test";

    io::assert(
        c.len() == 4,
        string::format("result = \"{}\" & c.len() = {}", c, c.len())
    );

    string d = "meow";

    io::assert(d.ends_with("ow"), d <> " should end with \"ow\"");
    io::assert(!d.starts_with("ow"), d <> " should not start with \"ow\"");
    io::assert(d.contains("eo"), d <> " should contain \"eo\"");

    string e = "ewew";

    io::assert(
        e.concat(e) == "ewewewew" && e <> e == "ewewewew" && e <> e == e.concat(e),
        string::format("result = \"{}\"", e.concat(e))
    );

    string f = "awa";

    io::assert(
        f.repeat(4) == "awaawaawaawa",
        string::format("result = \"{}\"", f.repeat(4))
    );

    string g = "test123AWA";

    io::assert(
        g.to_upper() == "TEST123AWA",
        string::format("result = \"{}\"", g.to_upper())
    );

    io::assert(
        g.to_lower() == "test123awa",
        string::format("result = \"{}\"", g.to_lower())
    );

    string h = string::ascii_upper();
    io::assert(
        h.slice(0, 3) == "ABC",
        string::format("result = \"{}\"", h.slice(0, 3))
    );

    string i = "me";
    i.extend("wo");

    io::assert(
        i == "mewo",
        string::format("result = \"{}\"", i)
    );

    char ch = i.pop();

    io::assert(ch == 'o', string::format("ch = {}", ch));
    io::assert(i == "mew", string::format("result = \"{}\"", i));

    i.push('e');
    i.push('w');

    io::assert(i == "mewew", string::format("result = \"{}\"", i));

    string j = string::format(
        "abc {}, {}, escaped {{}, mrow {} and a char but i32: {}",
        1, true, ":3", (i32)'a'
    );

    io::assert(
        j == "abc 1, true, escaped {}, mrow :3 and a char but i32: 97",
        string::format("result = \"{}\"", j)
    );

    io::println("All string tests have passed!".color("green").reset());
}
