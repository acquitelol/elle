generic T;
global pub;

struct Node {
    void *next;
    T data;
};

fn Node::add(Node **head, T data) {
    Node *new = malloc(#size(Node *));
    *new = malloc(#size(Node));

    if !new {
        return nil;
    }

    new.data = data;
    new.next = *head;
    *head = new;
}
