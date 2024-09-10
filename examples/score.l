use std/string;
use std/cast;
use std/io;

fn main() {
    i32 highestScore = 0;
    string winner = (string)malloc(BUF_SIZE * #size(char));
    defer free(winner);

    while true {
        string person = io::input("Enter a person's name (or 'stop'): ");
        defer free(person);

        if person.equals("stop") {
            break;
        }

        i32 score = i32_cast(io::input(string::format("Enter {}'s score: ", person)));

        if score > highestScore {
            highestScore = score;
            strcpy(winner, person);
        }
    }

    io::printf("The winner is {} with a score of {}", winner, highestScore);
}
