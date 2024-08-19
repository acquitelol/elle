use std/io;
use std/string;
use std/math;

fn main() {
    string a = string::ascii_lower();

    io::assert(
        a.to_upper().equals(string::ascii_upper()),
        "uppercased `a` should match the uppercase alphabet"
    );

    a.insert(11, '9');

    io::assert(
        a.equals("abcdefghijk9lmnopqrstuvwxyz"),
        "a should have 9 appended at the 11th index"
    );

    string b = "";
    io::assert(b.is_empty(), "b should be empty");
    io::assert(string::is_empty(b), "b should be empty");

    string c = "test";
    io::assert(c.len() == 4, "c is ".concat(c));
    io::assert(string::len(c) == 4, "c is ".concat(c));

    string d = "meow";
    io::assert(d.ends_with("ow"), d.concat(" should end with \"ow\""));
    io::assert(!d.starts_with("ow"), d.concat(" should not start with \"ow\""));
    io::assert(d.contains("eo"), d.concat(" should contain \"eo\""));

    string e = "ewew";
    io::assert(e.concat(e).equals("ewewewew"), "result is ".concat(e.concat(e)));

    string f = "awa";
    io::assert(f.repeat(4).equals("awaawaawaawa"), "result is ".concat(f.repeat(4)));

    string g = "test123AWA";
    io::assert(g.to_upper().equals("TEST123AWA"), "result is ".concat(g.to_upper()));
    io::assert(g.to_lower().equals("test123awa"), "result is ".concat(g.to_lower()));

    string h = string::ascii_upper();
    io::assert(h.slice(0, 3).equals("ABC"), "result is ".concat(h.slice(0, 3)));

    string i = "me";
    i.extend("wo");
    io::assert(i.equals("mewo"), "result is ".concat(i));

    char ch = i.pop();
    io::assert(ch == 'o', "ch should be 'o'");
    io::assert(i.equals("mew"), "i is ".concat(i));

    i.push('e');
    i.push('w');
    io::assert(i.equals("mewew"), "i is ".concat(i));

    io::println("All string tests have passed!".color("green").reset());
}
