use std/io;

fn loop(string str, fun *callback) -> void {
    for (i32 i = 0; i < strlen(str); i++) {
        callback(str[i]);
    }
}

fn formattedPrint(char character) {
    printf("%c ", character);
}

fn main() {
    string test = "Hello World!";

    // Expected result: "H e l l o   W o r l d !"
    loop(test, formattedPrint);

    printf("\n");
}
