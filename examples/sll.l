use std/collections/sll;
use std/io;

fn main() {
    LinkedList<i32> list = LinkedList::new();
    defer list.free();

    list.add(10);
    list.add(23);
    list.add(2);
    list.add(42);
    list.add(39);

    while list.head {
        io::print(list.head.data);
        list.head = list.head.next;

        if list.head {
            io::print(" -> ");
        }
    }

    io::print('\n');
}
