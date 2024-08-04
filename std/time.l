global pub;

const i64 CLOCKS_PER_SEC = 1000;

def time_info {
    i32 seconds; // Seconds [0, 60]
    i32 minutes; // Minutes [0, 59]
    i32 hours; // Hour [0, 23]
    i32 days; // Day of the month [1, 31]
    i32 months; // Month of the year [0, 11]
    i32 years; // Years since 1900
    i32 weekdays; // Day of week [0,6] (Sunday =0)
    i32 yeardays; // Day of year [0,365].
    i32 dst; // Daylight Savings flag.
}

external fn asctime(time_info *timeptr) -> string;
external fn asctime_r(time_info *restrict, string restrict) -> string;
external fn clock() -> i64;
external fn ctime(i64 *timer) -> string;
external fn ctime_r(i64 *timer, string a) -> string;
external fn difftime(f64 time1, f64 time2) -> f64;
external fn getdate(string date) -> time_info *;
external fn gmtime(i64 *timer) -> time_info *;
external fn gmtime_r(i64 *restrict, time_info *restrict) -> time_info *;
external fn localtime(i64 *timer) -> time_info *;
external fn localtime_r(i64 *restrict, time_info *restrict) -> time_info *;
external fn mktime(time_info *timeptr) -> i64;
external fn strftime(string str, i64 maxsize, string format, time_info *timeptr) -> i64;
external fn time(i64 *timer) -> i64;
external fn wcsftime(string str, i64 count, string format, time_info *timer) -> i64;
