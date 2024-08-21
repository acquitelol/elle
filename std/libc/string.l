global pub;

external fn memchr(string str, char character, i64 n) -> i64;
external fn memcmp(string str1, string str2, i64 n) -> i32;
external fn memcpy(void *dest, void *src, i64 n) -> i64;
external fn memmove(void *dest, void *src, i64 n) -> i64;
external fn memset(string str, char character, i64 n) -> i64;

external fn strcat(string dest, string src) -> string;
external fn strncat(string dest, string src, i64 n) -> string;
external fn strchr(string str, char character) -> string;
external fn strcmp(string str1, string str2) -> i32;
external fn strncmp(string str1, string str2, i64 n) -> i32;
external fn strcoll(string str1, string str2) -> i32;
external fn strcpy(string dest, string src) -> string;
external fn strncpy(string dest, string src, i64 n) -> string;
external fn strspn(string str1, string str2) -> i64;
external fn strcspn(string str1, string str2) -> i64;
external fn strerror(i32 errnum) -> string;
external fn strlen(string str) -> i64;
external fn strpbrk(string str1, string str2) -> string;
external fn strrchr(string str, char character) -> string;
external fn strstr(string haystack, string needle) -> string;
external fn strtok(string str, string delim) -> string;
external fn strxfrm(string dest, string src, i64 n) -> i64;
