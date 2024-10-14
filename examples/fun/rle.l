// RLE = Run-Length Encoding
// https://en.wikipedia.org/wiki/Run-length_encoding
use std/types;
use std/cast;
use std/io;

namespace RLE;

fn RLE::to(string input) {
    string result = "";

    for i32 i = 0; i < input.len(); i += 1 {
        i32 count = 1;

        while i + 1 < input.len() && input[i] == input[i + 1] {
            i += 1;
            count += 1;
        }

        if count > 1 {
            result <>= "{}".format(count);
        }
        result <>= "{}".format(input[i]);
    }

    return result;
}

fn RLE::from(string input) {
    string result = "";

    for i32 i = 0; i < input.len(); i += 1 {
        string length = "";

        while input[i].is_number() {
            length <>= "{}".format(input[i]);
            i += 1;
        }

        i32 repeat = i32::parse(length) || 1;

        io::assert(
            !input[i].is_number(),
            "Next character should not be numeric"
        );

        result <>= "{}".format(input[i]).repeat(repeat);
    }

    return result;
}

fn main() {
    string input = "once upon a time... in a land faaar faaaaaar away.....";

    io::printf("To RLE: {}", RLE::to(input));
    io::printf("From RLE: {}", RLE::from(RLE::to(input)));
}
