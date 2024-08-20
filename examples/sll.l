use std/io;

struct Node {
    void *next;
    i32 data;
};

fn Node::add(Node **head, i32 data) {
    Node *new = malloc(#size(Node *));
    *new = malloc(#size(Node));

    if !new {
        return nil;
    }

    new.data = data;
    new.next = *head;
    *head = new;
}

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
