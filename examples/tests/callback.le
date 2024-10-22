use std/io;

fn loop(string str, fn *callback) -> void {
    for i32 i = 0; i < str.len(); i += 1 {
        callback(str[i]);
    }
}

fn formattedPrint(char character) {
    io::print(character, " ");
}

fn main() {
    string test = "Hello World!";

    // Expected result: "H e l l o   W o r l d !"
    loop(test, fn(char character) -> io::print(character, " "));
    io::print('\n');

    loop(test, formattedPrint);
    io::print('\n');
}
