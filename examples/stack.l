use std/io;

fn direction(i32 *other) -> bool {
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
    io::printf(
        "The stack is growing {}wards for your architecture.",
        formattedDirection()
    );
}
