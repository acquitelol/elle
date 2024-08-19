global pub;

external fn free(void *ptr);
external fn atoi(string str) -> i32;
external fn atol(string str) -> i64;
external fn atof(string str) -> f64;

external fn abort();

fn bool::to_string(bool self) -> string {
    if self {
        return "true";
    } else {
        return "false";
    }
}
