use std/io;

fn main() {
    string name = io::input("Enter your first name -> ");

    if name {
        io::printf("Hello, {}!", name);
    }
}
