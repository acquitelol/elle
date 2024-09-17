use std/io;

fn main() {
    i32 a = 0xffff;

    io::assert(~a == a ^ -1, "~a should equal xor a, -1 but it is {}".format(~a));
    io::assert(~a == -a - 1, "~a should equal -a - 1 but it is {}".format(~a));
    io::assert(~a == ~65535, "~a should equal ~65535 but it is {}".format(~a));
    io::assert(~a == ~0xffff, "~a should equal ~0xffff but it is {}".format(~a));
    io::assert(~a == -65536, "~a should equal -65536 but it is {}".format(~a));

    io::println("All bitwise not tests have passed!".color("green").reset());
}
