use std/io;
use std/string;
use std/cast;

fn main() {
    i32 highestScore = 0;
    string winner = malloc(BUF_SIZE * #size(char));
    defer free(winner);

    while true {
        string person = io::input("Enter a person's name (or 'stop'): ");
        defer free(person);

        if person.equals("stop") {
            break;
        }

        io::printf("Enter %s's score: ", person);
        i32 score = i32_cast(io::input(""));

        if score > highestScore {
            highestScore = score;
            strcpy(winner, person);
        }
    }

    io::printf("The winner is %s with a score of %d\n", winner, highestScore);
}
