use crate::global;
use std::env;

// Mutable globals
global!(STD_LIB_PATH: &str = ".local/include/elle", get_STD_LIB_PATH);
global!(RUNTIME_PATH: &str = ".local/lib", get_RUNTIME_PATH);
global!(BUILD_PATH: &str = "./.build/", get_BUILD_PATH);
global!(MAIN_ID: &str = "__internal.elle.__main__", get_MAIN_ID);
global!(TARGET: &str = env::consts::OS, _get_TARGET);
global!(ARCH: &str = env::consts::ARCH, _get_ARCH);
global!(POINTER_ID: &str = "__clean_ptr__", get_POINTER_ID);
global!(STATIC_ARRAY_ID: &str = "__arr__", get_STATIC_ARRAY_ID);
global!(RAW_ERRORS: bool = false, get_RAW_ERRORS);
global!(INTROSPECTION_LOCATION: (usize, usize) = (usize::MAX, usize::MAX), get_INTROSPECTION_LOCATION);

// URLS
pub static ISSUE_URL: &str = "https://github.com/acquitelol/elle/issues/new";

// Identifiers
pub static META_STRUCT_NAME: &str = "ElleMeta";
pub static ENV_STRUCT_NAME: &str = "ElleEnv";
pub static PRIMARY_ALOCATOR_NAME: &str = "GCAllocator";
pub static PRIMARY_ALLOCATOR_MODULE: &str = "std/allocators/gc";
pub static BACKUP_ALLOCATOR_NAME: &str = "ArenaAllocator";
pub static BACKUP_ALLOCATOR_MODULE: &str = "std/allocators/arena";
pub static ARBITRARY_ALLOCATOR_NAME: &str = "ArbitraryAllocator";
pub static ARBITRARY_ALLOCATOR_MODULE: &str = "std/allocators/arbitrary";
pub static VOID_POINTER_ID: &str = "__void_ptr__";
pub static ENV_ID: &str = "__internal.elle.__env__";
pub static GC_NOOP: &str = "__internal_gc_noop";
pub static INTERNAL_FORMATTER: &str =
    "__internal_formatter_do_not_use_unless_you_know_what_youre_doing__";

// Generics
pub static GENERIC_IDENTIFIER: &str = "0"; // Start of a generic
pub static GENERIC_END: &str = "1"; // Allowing for nested generic structs
pub static GENERIC_POINTER: &str = "2"; // Pointer to another type
pub static GENERIC_UNKNOWN: &str = "3"; // Unknown type T
pub static GENERIC_SIZE: &str = "96"; // Array with a repr and size
pub static GENERIC_ARRAY: &str = "97"; // Array with a repr and size
pub static GENERIC_FUNCTION: &str = "98"; // Callback encoded in the type system
pub static GENERIC_ENUM: &str = "99"; // Enum with another repr type

// Extensions
pub static SHORT_EXTENSION: &str = ".le";
pub static OBJECT_EXTENSION: &str = ".o";
pub static FILE_EXTENSIONS: &[&str] = &[SHORT_EXTENSION, OBJECT_EXTENSION];

// Dunder methods
pub static FORMAT_CONSTANT: &str = "__fmt__";
pub static LOAD_CONSTANT: &str = "__load__";
pub static LOAD_REF_CONSTANT: &str = "__load_ref__";
pub static DEREF_LOAD_CONSTANT: &str = "__deref_load__";
pub static DEREF_STORE_CONSTANT: &str = "__deref_store__";
pub static STORE_CONSTANT: &str = "__store__";
pub static LEN_CONSTANT: &str = "__len__";
pub static HASH_CONSTANT: &str = "__hash__";
pub static EQUALS_CONSTANT: &str = "__equals__";
pub static TUPLE_CONSTANT: &str = "__tuple__";
pub static TRIPLE_CONSTANT: &str = "__triple__";
pub static ITER_CONSTANT: &str = "__iter__";
pub static PTR_PRIORITY_CONSTANTS: &[&str] = &[FORMAT_CONSTANT];
pub static DUNDER_CONSTANTS: &[&str] = &[
    FORMAT_CONSTANT,
    LOAD_CONSTANT,
    LOAD_REF_CONSTANT,
    DEREF_LOAD_CONSTANT,
    DEREF_STORE_CONSTANT,
    STORE_CONSTANT,
    LEN_CONSTANT,
    HASH_CONSTANT,
    EQUALS_CONSTANT,
    TUPLE_CONSTANT,
    TRIPLE_CONSTANT,
    ITER_CONSTANT,
];

// Keywords
pub static RESERVED_KEYWORDS: &[&str] = &[
    "as", "mut", "match", "static", "super", "do", "macro", "of", "class", "var", "impl", "union",
];

// Miscellaneous
pub static VA_LIST_SIZE_BYTES: usize = 32;
pub static DEAD_CODE_ELIMINATION_PASSES: usize = 6;
pub static DIVIDER_SIZE: usize = 50;
pub static DISPLAY_NESTING_MAX: usize = 3;

#[macro_export]
macro_rules! INTERNAL_VALUE_FORMAT {
    () => {
        "__internal_{}_value{}"
    };
}

#[macro_export]
macro_rules! INTERNAL_ITERATOR_FORMAT {
    () => {
        "__internal_{}_iterator{}"
    };
}

#[macro_export]
macro_rules! INTERNAL_GLOBAL_INIT_FORMAT {
    () => {
        "__internal.elle.init_{}"
    };
}

#[macro_export]
macro_rules! LAMBDA_SHORTHAND_SCHEME {
    () => {
        "x{}"
    };
}
