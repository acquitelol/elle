pub const i64 LONG_MAX = 9_223_372_036_854_775_807;
pub const i64 LONG_MIN = -LONG_MAX - 1;
pub const i32 INT_MAX = 2_147_483_647;
pub const i32 INT_MIN = -INT_MAX - 1;

pub const i64 PI = 3.141592653589793;

pub external fn acos(f64 x) -> f64;
pub external fn asin(f64 x) -> f64;
pub external fn atan(f64 x) -> f64;
pub external fn atan2(f64 x, f64 y) -> f64;
pub external fn cos(f64 x) -> f64;
pub external fn cosh(f64 x) -> f64;
pub external fn sin(f64 x) -> f64;
pub external fn sinh(f64 x) -> f64;
pub external fn tan(f64 x) -> f64;
pub external fn tanh(f64 x) -> f64;
pub external fn exp(f64 x) -> f64;
pub external fn frexp(f64 x, i32 *exponent) -> f64;
pub external fn ldexp(f64 x, i32 exponent) -> f64;
pub external fn log(f64 x) -> f64;
pub external fn log10(f64 x) -> f64;
pub external fn modf(f64 x, f64 *integer) -> f64;
pub external fn pow(f64 x, f64 y) -> f64;
pub external fn sqrt(f64 x) -> f64;
pub external fn ceil(f64 x) -> f64;
pub external fn fabs(f64 x) -> f64;
pub external fn floor(f64 x) -> f64;
pub external fn fmod(f64 x, f64 y) -> f64;
pub external fn round(f64 x) -> f64;

pub fn fact(i64 n) -> i64 {
    if n <= 1 {
        return 1;
    }

    return n * fact(n - 1);
}
