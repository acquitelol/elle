use std/io;
use std/string;
use std/cast;

fn main() {
    int highestScore = 0;
    string winner = malloc(BUF_SIZE * #size(char));
    defer free(winner);

    while true {
        string person = input("Enter a person's name: ");
        defer free(person);

        if (streq(person, "stop")) {
            break;
        }

        printf("Enter %s's score: ", person);
        int score = int_cast(input(""));

        if score > highestScore {
            highestScore = score;
            strcpy(winner, person);
        }
    }

    printf("The winner is %s with a score of %d\n", winner, highestScore);
}
