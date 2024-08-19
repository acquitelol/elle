use std/io;

fn rhs(string name, string state, string operator) {
    io::printf(
        "%s is %s in a logical %s so the rhs branch was taken\n",
        name, state, operator
    );
}

fn main() {
    bool a = true;
    bool b = false;

    a && rhs("a", a.to_string(), "AND");
    b || rhs("b", a.to_string(), "OR");

    a = !a;
    b = !b;

    // Wouldn't run due to short-circuiting
    // `a` is false in &&, `b` is true in ||
    a && rhs("a", a.to_string(), "AND");
    b || rhs("b", b.to_string(), "OR");
}
