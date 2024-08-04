use std/io;

def DynamicArray {
    i32 size;
    i32 capacity;
    i32 *elements;
}

fn initArray(DynamicArray *arr, i32 initialCapacity) {
    arr.elements = (i32 *)malloc(initialCapacity * #size(i32));
    arr.size = 0;
    arr.capacity = initialCapacity;
}

fn resizeArray(DynamicArray *arr, i32 newCapacity) {
    arr.elements = (i32 *)realloc(arr.elements, newCapacity * #size(i32));
    arr.capacity = newCapacity;
}

fn addElement(DynamicArray *arr, i32 element) {
    if (arr.size == arr.capacity) {
        resizeArray(arr, arr.capacity * 2);
    }

    arr.elements[arr.size] = element;
    arr.size += 1;
}

fn freeArray(DynamicArray *arr) {
    free(arr.elements);
    arr.elements = NULL;
    arr.size = 0;
    arr.capacity = 0;
}

fn main() {
    DynamicArray arr = DynamicArray {};
    initArray(&arr, 2);

    addElement(&arr, 39);
    addElement(&arr, 16);
    addElement(&arr, 0);
    addElement(&arr, 172);

    for i32 i = 0; i < arr.size; i++ {
        printf("arr[%d] = %d\n", i, arr.elements[i]);
    }

    freeArray(&arr);
}
