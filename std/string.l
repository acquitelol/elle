pub external fn memchr(string str, char character, long n) -> long;
pub external fn memcmp(string str1, string str2, long n) -> int;
pub external fn memcpy(long dest, long src, long n) -> long;
pub external fn memmove(long dest, long src, long n) -> long;
pub external fn memset(string str, char character, long n) -> long;

pub external fn strcat(string dest, string src) -> string;
pub external fn strncat(string dest, string src, long n) -> string;
pub external fn strchr(string str, char character) -> string;
pub external fn strcmp(string str1, string str2) -> int;
pub external fn strncmp(string str1, string str2, long n) -> int;
pub external fn strcoll(string str1, string str2) -> int;
pub external fn strcpy(string dest, string src) -> string;
pub external fn strncpy(string dest, string src, long n) -> string;
pub external fn strspn(string str1, string str2) -> long;
pub external fn strcspn(string str1, string str2) -> long;
pub external fn strerror(int errnum) -> string;
pub external fn strlen(string str) -> long;
pub external fn strpbrk(string str1, string str2) -> string;
pub external fn strrchr(string str, char character) -> string;
pub external fn strstr(string haystack, string needle) -> string;
pub external fn strtok(string str, string delim) -> string;
pub external fn strxfrm(string dest, string src, long n) -> long;

pub fn streq(string str1, string str2) {
    return strcmp(str1, str2) == 0;
}

pub fn concat(int size, ...) {
    variadic args[size * #size(string)];
    string strings[size];
    int sizes[size];
    long length = 0;

    // Collect the strings and the final string length
    for int i = 0; i < size; i++ {
        strings[i] = args yield string;
        sizes[i] = strlen(strings[i]);
        length += sizes[i] + 1;
    }

    string result = malloc((length + 1) * #size(char));
    int index = 0;

    // Construct the final string
    for int i = 0; i < size; i++ {
        string current = strings[i];

        for int j = 0; j < sizes[i]; j++ {
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
