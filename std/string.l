pub external fn memchr(string str, char character, i64 n) -> i64;
pub external fn memcmp(string str1, string str2, i64 n) -> i32;
pub external fn memcpy(void *dest, void *src, i64 n) -> i64;
pub external fn memmove(void *dest, void *src, i64 n) -> i64;
pub external fn memset(string str, char character, i64 n) -> i64;

pub external fn strcat(string dest, string src) -> string;
pub external fn strncat(string dest, string src, i64 n) -> string;
pub external fn strchr(string str, char character) -> string;
pub external fn strcmp(string str1, string str2) -> i32;
pub external fn strncmp(string str1, string str2, i64 n) -> i32;
pub external fn strcoll(string str1, string str2) -> i32;
pub external fn strcpy(string dest, string src) -> string;
pub external fn strncpy(string dest, string src, i64 n) -> string;
pub external fn strspn(string str1, string str2) -> i64;
pub external fn strcspn(string str1, string str2) -> i64;
pub external fn strerror(i32 errnum) -> string;
pub external fn strlen(string str) -> i64;
pub external fn strpbrk(string str1, string str2) -> string;
pub external fn strrchr(string str, char character) -> string;
pub external fn strstr(string haystack, string needle) -> string;
pub external fn strtok(string str, string delim) -> string;
pub external fn strxfrm(string dest, string src, i64 n) -> i64;

pub fn streq(string str1, string str2) {
    return strcmp(str1, str2) == 0;
}

pub fn concat(i32 size, ...) {
    variadic args[size * #size(string)];
    string strings[size];
    i32 sizes[size];
    i64 length = 0;

    // Collect the strings and the final string length
    for i32 i = 0; i < size; i++ {
        strings[i] = args yield string;
        sizes[i] = strlen(strings[i]);
        length += sizes[i] + 1;
    }

    string result = malloc((length + 1) * #size(char));
    i32 index = 0;

    // Construct the final string
    for i32 i = 0; i < size; i++ {
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
