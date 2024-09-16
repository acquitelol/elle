use std/io;
use std/cast;

fn fahrenheit_to_celsius(f32 degrees) {
    return (degrees - 32) * 5 / 9;
}

fn main() {
    string input = io::input("Enter a temperature in °F -> ");
    f64 fahrenheit;

    if f64::try_parse(&fahrenheit, input) {
        io::printf("{}°F = {}°C", fahrenheit, fahrenheit_to_celsius(fahrenheit));
    } else {
        io::printf("Invalid input (must be a number): {}", input);
    }
}
