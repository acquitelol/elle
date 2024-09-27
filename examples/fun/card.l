use std/io;
use std/rand;
use std/time;

fn get_card() {
    string *ranks = ["Ace", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King"];
    string *suits = ["Clubs", "Diamonds", "Hearts", "Spades"];

    i32 number = rand::random(0, 51);
    string rank = ranks[number % 13];
    string suit = suits[number / 13];

    return rank <> " of " <> suit;
}

fn main() {
    rand::seed(time(nil));

    string card = get_card();
    io::printf("Your card is the {}", card);
}
