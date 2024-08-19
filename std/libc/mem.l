global pub;

external fn malloc(i64 size) -> i64;
external fn realloc(void *ptr, i64 size) -> i64;
external fn free(void *ptr);
