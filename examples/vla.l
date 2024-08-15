use std/io;

def Array {
    i32 size;
    i32 capacity;
    i32 *elements;
}

fn Array.init(Array *self, i32 initialCapacity) -> Array * {
    self.elements = (i32 *)malloc(initialCapacity * #size(i32));
    self.size = 0;
    self.capacity = initialCapacity;

    return self;
}

fn Array.resize(Array *self, i32 newCapacity) {
    self.elements = (i32 *)realloc(self.elements, newCapacity * #size(i32));
    self.capacity = newCapacity;
}

fn Array.push(Array *self, i32 element) {
    if self.size >= self.capacity {
        self.resize(self.capacity * 2);
    }

    self.elements[self.size] = element;
    self.size += 1;
}

fn Array.free(Array *self) {
    free(self.elements);
    self.elements = nil;
    self.size = 0;
    self.capacity = 0;
}

fn main() {
    Array arr = Array {};
    arr.init(2);

    arr.push(39);
    arr.push(16);
    arr.push(0);
    arr.push(172);
    arr.push(53);

    for i32 i = 0; i < arr.size; i += 1 {
        printf("arr[%d] = %d\n", i, arr.elements[i]);
    }

    arr.free();
}
