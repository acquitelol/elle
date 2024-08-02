// Note that any functions which use the `tm` struct are not defined yet
// due to the fact you can't make structs yet in Elle
pub const long CLOCKS_PER_SEC = 1000;

// pub external fn asctime(long timeptr) -> string;
pub external fn clock() -> long;
pub external fn ctime(double *timer) -> string; // `double` is the only type that is always 8 bytes in size
pub external fn difftime(double time1, double time2) -> double;
// pub external fn gmtime(double *timer) -> long;
// pub external fn localtime(double *timer) -> long;
// pub external fn mktime(long timeptr) -> double;
// pub external strftime(string str, double maxsize, string format, long timeptr) -> double;
pub external fn time(long *timer) -> double;
// pub external fn wcsftime(string str, double count, string format, long time) -> double;
