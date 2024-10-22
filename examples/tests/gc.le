use std/collections/array;
use std/io;

struct Object {
    void *data;
    bool marked;
    i32 size;
};

fn Object::new(i32 size) {
    Object *obj = malloc(#size(Object));
    *obj = malloc(#size(Object));

    obj.data = malloc(size);
    obj.marked = false;
    obj.size = size;

    return obj;
}

fn Object::free(Object *self) {
    free(self.data);
    free((void *)(*self));
    free(self);
}

struct GC {
    Array<Object *> *heap;
};

fn GC::new() {
    GC *pool = malloc(#size(GC));
    *pool = malloc(#size(GC));
    pool.heap = Array::new();

    return pool;
}

fn GC::free(GC *self) {
    self.heap.free();
    free((void *)(*self));
    free(self);
}

fn GC::alloc(GC *self, i32 size) {
    Object *obj = Object::new(size);
    self.heap.push(obj);
    return obj.data;
}

fn GC::mark(GC *self, void *ptr) {
    for (i32 i = 0; i < self.heap.len(); i += 1) {
        if self.heap.elements[i].data == ptr {
            self.heap.elements[i].marked = true;
        }
    }
}

fn GC::sweep(GC *self) {
    for (i32 i = 0; i < self.heap.len(); i += 1) {
        if !self.heap.elements[i].marked {
            self.heap.remove(i).free();
        } else {
            self.heap.elements[i].marked = false;
        }
    }
}

fn main() {
    GC *gc = GC::new();
    defer gc.free();

    i32 *num = gc.alloc(#size(i32));
    *num = 42;

    i32 *arr = gc.alloc(10 * #size(i32));
    arr[0] = 5;

    io::dbg(*num, arr[0], gc.heap.len());
    gc.mark(num); // Will mark num, making it not be sweeped

    io::printf("\nSweeping non-marked allocations...\n");
    gc.sweep();

    // The array will be garbage because it was freed
    io::dbg(*num, arr[0], gc.heap.len());

    io::printf("\nSweeping non-marked allocations...\n");
    gc.sweep(); // Sweep without marking the num pointer

    // Everything will be garbage data because it was freed
    io::dbg(*num, arr[0], gc.heap.len());
}
