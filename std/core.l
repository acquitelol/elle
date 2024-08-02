pub external fn malloc(i64 size) -> i64;
pub external fn realloc(void *ptr, i64 size) -> i64;
pub external fn free(void *ptr);
pub external fn atoi(string str) -> i32;
pub external fn atol(string str) -> i64;
pub external fn atof(string str) -> f64;

pub fn btos(bool value) -> string {
    if value {
        return "true";
    } else {
        return "false";
    }
}
