use std/io;

fn rhs(string name, bool state, string operator) {
    io::printf(
        "{} is {} in a logical {} so the rhs branch was taken",
        name, state, operator
    );
}

fn main() {
    bool a = true;
    bool b = false;

    a && rhs("a", a, "AND");
    b || rhs("b", b, "OR");

    a = !a;
    b = !b;

    // Wouldn't run due to short-circuiting
    // `a` is false in &&, `b` is true in ||
    a && rhs("a", a, "AND");
    b || rhs("b", b, "OR");
}
