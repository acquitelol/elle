// Note that any functions which use the `tm` struct are not defined yet
// due to the fact you can't make structs yet in Elle
pub const i64 CLOCKS_PER_SEC = 1000;

// pub external fn asctime(long timeptr) -> string;
pub external fn clock() -> void *;
pub external fn ctime(i64 *timer) -> string;
pub external fn difftime(f64 time1, f64 time2) -> f64;
// pub external fn gmtime(double *timer) -> long;
// pub external fn localtime(double *timer) -> long;
// pub external fn mktime(long timeptr) -> double;
// pub external strftime(string str, double maxsize, string format, long timeptr) -> double;
pub external fn time(i64 *timer) -> f64;
// pub external fn wcsftime(string str, double count, string format, long time) -> double;
