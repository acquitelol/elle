use std/collections/sll<i32>;
use std/io;

fn main() {
    Node *head = nil;

    Node::add(&head, 10);
    Node::add(&head, 23);
    Node::add(&head, 2);
    Node::add(&head, 42);
    Node::add(&head, 39);

    while head {
        io::print(head.data);
        head = head.next;

        if head {
            io::print(" -> ");
        }
    }

    io::print('\n');
}
