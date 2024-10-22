// This algorithm is essentially a direct translation from the
// (very popular) C-based Quake 3 Fast Inverse Square Root algorithm
//
// https://en.wikipedia.org/wiki/Fast_inverse_square_root
use std/io;

fn fast_inv_sqrt(f32 x) {
	f32 y = x;
	i32 i = *(i32 *)(&y);        // Interpret the bits of a float as an integer

	i = 0x5f3759df - (i >> 1);   // "what the fuck?"
	y = *(f32 *)(&i);            // Interpret the result as a float again
	y *= 1.5 - 0.5 * x * y * y;  // 1st iteration of Newton-Raphson
	y *= 1.5 - 0.5 * x * y * y;  // 2nd iteration, can be removed

	return y;
}

fn main() {
    f32 number = 9;
    f32 result = fast_inv_sqrt(number);

    io::printf("1/sqrt({}) ≈ {}", number, result);
}
