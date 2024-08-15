use std/io;

fn direction(i32 *other) {
    i32 x = 0;

    if !other {
        return direction(&x);
    } else {
        return &x > other;
    }
}

fn formattedDirection() -> string {
    if direction(nil) {
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
