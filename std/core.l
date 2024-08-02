pub external fn malloc(long size) -> long;
pub external fn realloc(long ptr, long size) -> long;
pub external fn free(long ptr);
pub external fn atoi(string str) -> int;
pub external fn atol(string str) -> long;
pub external fn atof(string str) -> double;

pub fn btos(bool value) -> string {
    if value {
        return "true";
    } else {
        return "false";
    }
}
