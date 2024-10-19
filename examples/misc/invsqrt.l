// This algorithm is essentially a direct translation from the
// (very popular) C-based Quake 3 Fast Inverse Square Root algorithm
//
// https://en.wikipedia.org/wiki/Fast_inverse_square_root
use std/io;
use std/math;

fn fast_inv_sqrt(f32 number) {
	f32 x2 = number * 0.5;
	f32 y = number;

	i64 i = *(i64 *)(&y);
	i = 0x5f3759df - (i >> 1);

	y = *(f32 *)(&i);
	y *= (1.5 - (x2 * y * y));

	return y;
}

fn main() {
    f32 number = 9;
    f32 result = fast_inv_sqrt(number);

    io::printf("1/sqrt({}) ≈ {}", number, result);
}
