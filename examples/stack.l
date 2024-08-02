use std/io;

fn direction(int *other) {
    int x = 0;

    if !other {
        return direction(&x);
    } else {
        return &x > other;
    }
}

fn formattedDirection() -> string {
    if direction(NULL) {
        return "up";
    } else {
        return "down";
    }
}

fn main() {
    printf(
        "The stack is growing %swards for your architecture.\n",
        formattedDirection()
    );
}
