global pub;

fn i64::max() -> i64 {
    return 9_223_372_036_854_775_807;
}

fn i64::min() -> i64 {
    return -i64::max() - 1;
}

fn i32::max() -> i32 {
    return 2_147_483_647;
}

fn i32::min() -> i32 {
    return -i32::max() - 1;
}

const f64 E = 2.7182818284590455348848081484902650117;
const f64 PI = 3.1415926535897932384626433832795028841;

struct math {};

external fn acos(f64 x) @alias("math::acos") -> f64;
external fn asin(f64 x) @alias("math::asin") -> f64;
external fn atan(f64 slope) @alias("math::atan") -> f64;
external fn atan2(f64 y, f64 x) @alias("math::atan2") -> f64;

fn math::atan3(f64 x, f64 y) {
    return math::atan2(y, x);
}

external fn cos(f64 x) @alias("math::cos") -> f64;
external fn cosh(f64 x) @alias("math::cosh") -> f64;
external fn sin(f64 x) @alias("math::sin") -> f64;
external fn sinh(f64 x) @alias("math::sinh") -> f64;
external fn tan(f64 x) @alias("math::tan") -> f64;
external fn tanh(f64 x) @alias("math::tanh") -> f64;
external fn exp(f64 x) @alias("math::exp") -> f64;
external fn frexp(f64 x, i32 *exponent) @alias("math::frexp") -> f64;
external fn ldexp(f64 x, i32 exponent) @alias("math::ldexp") -> f64;
external fn log(f64 x) @alias("math::log") -> f64;
external fn log10(f64 x) @alias("math::log10") -> f64;
external fn modf(f64 x, f64 *integer) @alias("math::modf") -> f64;
external fn pow(f64 x, f64 y) @alias("math::pow") -> f64;
external fn sqrt(f64 x) @alias("math::sqrt") -> f64;
external fn ceil(f64 x) @alias("math::ceil") -> f64;
external fn fabs(f64 x) @alias("math::fabs") -> f64;
external fn floor(f64 x) @alias("math::floor") -> f64;
external fn fmod(f64 x, f64 y) @alias("math::fmod") -> f64;
external fn fmin(f64 x, f64 y) @alias("math::fmin") -> f64;
external fn fmax(f64 x, f64 y) @alias("math::fmax") -> f64;
external fn round(f64 x) @alias("math::round") -> f64;

fn math::fact(i64 n) -> i64 {
    if n <= 1 {
        return 1;
    }

    return n * math::fact(n - 1);
}

fn math::min(i64 a, i64 b) -> i64 {
    if a < b {
        return a;
    }

    return b;
}

fn math::max(i64 a, i64 b) -> i64 {
    if a > b {
        return a;
    }

    return b;
}

fn math::clamp(i64 value, i64 min_value, i64 max_value) -> i64 {
    return math::max(min_value, math::min(value, max_value));
}

fn math::fclamp(f64 value, f64 min_value, f64 max_value) -> f64 {
    return math::fmax(min_value, math::fmin(value, max_value));
}

fn math::next_power_of_2(i64 x) {
	i64 y = 1;

	while y < x {
	   y += y;
	}

	return y;
}

fn math::sign<T>(T x) -> bool {
    if x > 0 {
        return 1;
    }

    return -1;
}

fn math::abs<T>(T x) -> T {
    if x > 0 {
        return x;
    } else {
        return -x;
    }
}
