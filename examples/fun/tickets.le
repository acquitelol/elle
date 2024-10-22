use std/collections/array;
use std/string;
use std/cast;
use std/io;

fn main() {
    f64 price = 11.50;
    Array<string> *extras = Array::new();

    string ticket_type = io::input("Enter the ticket type -> ").default("Adult");
    bool member = bool::parse(io::input("Is the ticket holder a member? (true or false) -> "));

    if member {
        price *= 0.9;
        extras.push("Premier seats");
    }

    while true {
        defer break;

        if ticket_type == "Infant" {
            price = 0;
            break;
        }

        if ticket_type == "Student" || ticket_type == "Senior" {
            price *= 0.8;

            if member {
                extras.push("Popcorn");
            }

            break;
        }

        ticket_type = "Adult";
    }

    io::printf("Ticket type: {}", ticket_type);
    io::printf("Membership: {}", member);
    io::printf("Ticket cost: £{}", price);

    string joined = extras.join(", ").default("None");
    io::printf("Extras: {}", joined);
}
