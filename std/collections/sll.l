global pub;

struct Node<T> {
    void *next;
    T data;
};

struct LinkedList<T> {
    Node<T> *head;
};

fn LinkedList::new<T>() -> LinkedList<T> {
    return LinkedList { head = nil };
}

fn LinkedList::add<T>(LinkedList<T> *self, T data) {
    Node<T> *new = malloc(#size(Node<T>));
    *new = malloc(#size(Node<T>));

    if !new {
        return;
    }

    new.data = data;
    new.next = self.head;
    self.head = new;
}

fn LinkedList::free<T>(LinkedList<T> *self) {
    while self.head {
        Node<T> *next = self.head.next;
        free(self.head);
        self.head = next;
    }
}
