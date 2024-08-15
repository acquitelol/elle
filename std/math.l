global pub;

fn i64.max() {
    return 9_223_372_036_854_775_807;
}

fn i64.min() {
    return -i64.max() - 1;
}

fn i32.max() {
    return 2_147_483_647;
}

fn i32.min() {
    return -i32.max() - 1;
}

const f64 E = 2.7182818284590455348848081484902650117;
const f64 PI = 3.1415926535897932384626433832795028841;

external fn acos(f64 x) -> f64;
external fn asin(f64 x) -> f64;
external fn atan(f64 slope) -> f64;
external fn atan2(f64 y, f64 x) -> f64;

fn atan3(f64 x, f64 y) {
    return atan2(y, x);
}

external fn cos(f64 x) -> f64;
external fn cosh(f64 x) -> f64;
external fn sin(f64 x) -> f64;
external fn sinh(f64 x) -> f64;
external fn tan(f64 x) -> f64;
external fn tanh(f64 x) -> f64;
external fn exp(f64 x) -> f64;
external fn frexp(f64 x, i32 *exponent) -> f64;
external fn ldexp(f64 x, i32 exponent) -> f64;
external fn log(f64 x) -> f64;
external fn log10(f64 x) -> f64;
external fn modf(f64 x, f64 *integer) -> f64;
external fn pow(f64 x, f64 y) -> f64;
external fn sqrt(f64 x) -> f64;
external fn ceil(f64 x) -> f64;
external fn fabs(f64 x) -> f64;
external fn floor(f64 x) -> f64;
external fn fmod(f64 x, f64 y) -> f64;
external fn fmin(f64 x, f64 y) -> f64;
external fn fmax(f64 x, f64 y) -> f64;
external fn round(f64 x) -> f64;

fn fact(i64 n) -> i64 {
    if n <= 1 {
        return 1;
    }

    return n * fact(n - 1);
}

fn min(i64 a, i64 b) -> i64 {
    if a < b {
        return a;
    }

    return b;
}

fn max(i64 a, i64 b) -> i64 {
    if a > b {
        return a;
    }

    return b;
}

fn clamp(i64 value, i64 min_value, i64 max_value) -> i64 {
    return max(min_value, min(value, max_value));
}

fn fclamp(f64 value, f64 min_value, f64 max_value) -> f64 {
    return fmax(min_value, fmin(value, max_value));
}

fn next_power_of_2(i64 x) {
	i64 y = 1;

	while y < x {
	   y += y;
	}

	return y;
}
