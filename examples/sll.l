use std/io;

struct Node {
    void *next;
    i32 data;
};

fn Node::add_node(Node **head, i32 data) {
    Node *new = malloc(#size(Node *));
    *new = malloc(#size(Node));

    if !new {
        return nil;
    }

    new.data = data;

    if !*head {
        new.next = nil;
    } else {
        new.next = *head;
    }

    *head = new;
}

fn main() {
    Node *head = nil;

    Node::add_node(&head, 10);
    Node::add_node(&head, 23);
    Node::add_node(&head, 2);
    Node::add_node(&head, 42);
    Node::add_node(&head, 39);

    while head {
        io::print(head.data);
        head = head.next;

        if head {
            io::print(" > ");
        }
    }

    io::print('\n');
}
