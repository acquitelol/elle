use crate::global;

// Mutable globals
global!(STD_LIB_PATH: &'static str = ".local/include/elle", get_STD_LIB_PATH);
global!(RUNTIME_PATH: &'static str = ".local/lib", get_RUNTIME_PATH);
global!(BUILD_PATH: &'static str = "./.build/", get_BUILD_PATH);
global!(MAIN_ID: &'static str = "__internal.elle.__main__", get_MAIN_ID);
global!(POINTER_ID: &'static str = "__ptr__", get_POINTER_ID);
global!(RAW_ERRORS: bool = false, get_RAW_ERRORS);
global!(INTROSPECTION_LOCATION: (usize, usize) = (usize::MAX, usize::MAX), get_INTROSPECTION_LOCATION);

// URLS
pub static ISSUE_URL: &'static str = "https://github.com/acquitelol/elle/issues/new";

// Identifiers
pub static META_STRUCT_NAME: &'static str = "ElleMeta";
pub static ENV_STRUCT_NAME: &'static str = "ElleEnv";
pub static PRIMARY_ALOCATOR_NAME: &'static str = "GCAllocator";
pub static PRIMARY_ALLOCATOR_MODULE: &'static str = "std/allocators/gc";
pub static BACKUP_ALLOCATOR_NAME: &'static str = "ArenaAllocator";
pub static BACKUP_ALLOCATOR_MODULE: &'static str = "std/allocators/arena";
pub static ARBITRARY_ALLOCATOR_NAME: &'static str = "ArbitraryAllocator";
pub static ARBITRARY_ALLOCATOR_MODULE: &'static str = "std/allocators/arbitrary";
pub static VOID_POINTER_ID: &'static str = "__void_ptr__";
pub static ENV_ID: &'static str = "__internal.elle.__env__";
pub static GC_NOOP: &'static str = "__internal_gc_noop";
pub static INTERNAL_FORMATTER: &'static str =
    "__internal_formatter_do_not_use_unless_you_know_what_youre_doing__";

// Generics
pub static GENERIC_IDENTIFIER: &str = "0"; // Start of a generic
pub static GENERIC_END: &str = "1"; // Allowing for nested generic structs
pub static GENERIC_POINTER: &str = "2"; // Pointer to another type
pub static GENERIC_UNKNOWN: &str = "3"; // Unknown type T

// Extensions
pub static SHORT_EXTENSION: &str = ".le";
pub static OBJECT_EXTENSION: &str = ".o";
pub static FILE_EXTENSIONS: &[&'static str] = &[SHORT_EXTENSION, OBJECT_EXTENSION];

// Dunder methods
pub static FORMAT_CONSTANT: &'static str = "__fmt__";
pub static LOAD_CONSTANT: &'static str = "__load__";
pub static STORE_CONSTANT: &'static str = "__store__";
pub static LEN_CONSTANT: &'static str = "__len__";
pub static HASH_CONSTANT: &'static str = "__hash__";
pub static EQUALS_CONSTANT: &'static str = "__equals__";
pub static PTR_PRIORITY_CONSTANTS: &[&'static str] = &[FORMAT_CONSTANT];
pub static DUNDER_CONSTANTS: &[&'static str] = &[
    FORMAT_CONSTANT,
    LOAD_CONSTANT,
    STORE_CONSTANT,
    LEN_CONSTANT,
    HASH_CONSTANT,
    EQUALS_CONSTANT,
];

// Keywords
pub static RESERVED_KEYWORDS: &[&'static str] = &[
    "as", "mut", "enum", "match", "static", "super", "do", "macro", "step", "of", "class", "var",
    "impl",
];

// Miscellaneous
pub static VA_LIST_SIZE_BYTES: usize = 32;
pub static DEAD_CODE_ELIMINATION_PASSES: usize = 6;
pub static DIVIDER_SIZE: usize = 50;

#[macro_export]
macro_rules! INTERNAL_IDX_FORMAT {
    () => {
        "__internal_{}_idx"
    };
}

#[macro_export]
macro_rules! INTERNAL_ITERATOR_FORMAT {
    () => {
        "__internal_{}_iterator"
    };
}
