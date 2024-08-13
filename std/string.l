global pub;

external fn malloc(i64 size) -> i64;
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

fn streq(string str1, string str2) {
    return str1 == str2 || strcmp(str1, str2) == 0;
}

fn strprefix(string str, string pre) {
    return strncmp(pre, str, strlen(pre)) == 0;
}

fn concat(ElleMeta meta, ...) {
    variadic args[meta.arity];
    string strings[meta.arity];
    i32 sizes[meta.arity];
    i64 length = 0;

    // Collect the strings and the final string length
    for i32 i = 0; i < meta.arity; i++ {
        strings[i] = args yield string;
        sizes[i] = strlen(strings[i]);
        length += sizes[i] + 1;
    }

    string result = malloc((length + 1) * #size(char));
    i32 index = 0;

    // Construct the final string
    for i32 i = 0; i < meta.arity; i++ {
        string current = strings[i];

        for i32 j = 0; j < sizes[i]; j++ {
            result[index] = current[j];
            index++;
        }

        result[index] = '\n';
        index++;
    }

    // Include null terminator
    result[index] = '\0';
    return result;
}
