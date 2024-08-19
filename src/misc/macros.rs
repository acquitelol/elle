/// Creates a [`HashMap`] containing the arguments.
///
/// There are three forms of this macro:
///
/// - Create an empty [`HashMap`]:
///
/// ```
/// let map = hashmap!();
/// assert_eq!(map.len(), 0);
/// ```
///
/// - Create a [`HashMap`] with a given key and value type:
///
/// ```
/// let mut map = hashmap!(&str, i32);
/// map.insert("a", 10);
/// assert_eq!(map.get("a"), Some(&10_i32));
/// ```
///
/// - Create a [`HashMap`] with a collection of keys and values:
///
/// ```
/// let map = hashmap!(
///     "a" => 3,
///     "b" => 4,
///     "c" => 5,
/// );
/// assert_eq!(map.get("a"), Some(&3_i32));
/// assert_eq!(map.get("b"), Some(&4_i32));
/// assert_eq!(map.get("c"), Some(&5_i32));
/// ```
///
/// [`HashMap`]: std::collections::HashMap
#[macro_export]
macro_rules! hashmap {
    () => {
        std::collections::HashMap::new()
    };

    ($key:ty, $val:ty) => {
        std::collections::HashMap::<$key, $val>::new()
    };

    ($( $key:expr => $value:expr ),* $(,)?) => {{
        let mut map = std::collections::HashMap::new();
        $(
            map.insert($key, $value);
        )*
        map
    }};
}

/// Removes a symbol (function, constant, struct) named [`name`]
///
/// Sets the [`usable`] and [`imported`] property on the path [`val`]
///
/// [`name`]: $name:expr
/// [`val`]: $val:path
#[macro_export]
macro_rules! override_and_add_node {
    ($val:path, $tree:expr, $name:expr, $symbol:expr, $public:expr, $allow_all:expr, $functions:expr $(,)?) => {
        match existing_definition($tree.clone(), $name.clone()) {
            Some(index) => {
                $tree.remove(index);
            }
            None => {}
        }

        let mut new_symbol = $symbol.clone();
        if let $val {
            ref mut usable,
            ref mut imported,
            ..
        } = new_symbol
        {
            *usable =
                is_valid_insert_context($name.clone(), $public, $allow_all, $functions.clone());

            *imported = true;
        }

        $tree.insert(0, new_symbol);
    };
}

/// Throws an error informing the user that [`self.current_token()`]
/// was not a valid struct or primitive type
#[macro_export]
macro_rules! not_valid_struct_or_type {
    ($self:expr $(,)?) => {{
        let name = $self.current_token().value.get_string_inner().unwrap();

        panic!(
            "{}",
            $self.current_token().location.error(format!(
                "Identifier '{}' isn't a struct or primitive type.\n{}",
                name.clone(),
                if let Some(map) = ValueKind::similar_mapping(name.clone()) {
                    format!(
                        "A similar type exists which might be what you need: '{}'",
                        map
                    )
                } else {
                    format!("Are you sure you spelt '{}' correctly?", name)
                }
            )),
        )
    }};
}

#[macro_export]
macro_rules! unknown_function {
    ($location:expr, $name:expr, $module:expr $(,)?) => {{
        let mut similar_name = None;
        let mut lowest_distance = usize::max_value();

        for func in $module
            .borrow_mut()
            .functions
            .iter()
            .filter(|func| func.name != "nil")
        {
            let contains_name = func.name.contains($name.as_str());
            let distance =
                crate::misc::levenshtein::levenshtein($name.as_str(), func.name.clone().as_str());

            if contains_name && (distance <= lowest_distance || similar_name.is_none()) {
                lowest_distance = distance;
                similar_name = Some(func.name.clone());
            } else if !contains_name && distance < lowest_distance && similar_name.is_none() {
                lowest_distance = distance;
                similar_name = Some(func.name.clone());
            }
        }

        panic!(
            "{}",
            $location.error(format!(
                "Function named '{}' has an unknown interface.{}",
                $name.clone().replace(".", "::"),
                if let Some(similar) = similar_name {
                    format!(
                        "\nA function with a similar name exists: '{}'",
                        similar.replace(".", "::")
                    )
                } else {
                    "".into()
                }
            ))
        )
    }};
}

/// Converts a token [`token`] into an AstNode
///
/// This accounts for [`TrueLiteral`, `FalseLiteral`, `FloatingPoint`]
///
/// [`token`]: $token:expr
#[macro_export]
macro_rules! token_to_node {
    ($token:expr, $self:expr) => {
        match $token.kind {
            TokenKind::TrueLiteral => AstNode::LiteralStatement {
                kind: TokenKind::BoolLiteral,
                value: ValueKind::Number(1),
                location: $token.location,
            },
            TokenKind::FalseLiteral => AstNode::LiteralStatement {
                kind: TokenKind::BoolLiteral,
                value: ValueKind::Number(0),
                location: $token.location,
            },
            TokenKind::FloatingPoint => $self.parse_float($token),
            _ => AstNode::LiteralStatement {
                kind: $token.kind,
                value: $token.value,
                location: $token.location,
            },
        }
    };
}

/// Throws an error if [`is_fn_pointer`] is true
/// and [`found_ptr`] is false.
///
/// This asserts in `get_type()` variants that the type is a
/// function *pointer* not just a function type
///
/// [`is_fn_pointer`]: $is_fn_pointer:expr
/// [`found_ptr`]: $found_ptr:expr
#[macro_export]
macro_rules! ensure_fn_pointer {
    ($self:expr, $is_fn_pointer:expr, $found_ptr:expr $(,)?) => {
        if $is_fn_pointer && !$found_ptr {
            panic!(
                "{}",
                $self.current_token().location.error(
                    "Expected function pointer, got just 'fn'.\nTry 'fn *' instead of 'fn'."
                )
            );
        } else {
            break;
        }
    };
}

/// Increments [`i`] if [`i`] + 1 is less than [`chars`]'s length
///
/// [`i`]: $i:expr
/// [`chars`]: $chars:expr
#[macro_export]
macro_rules! advance {
    ($i:expr, $chars:expr $(,)?) => {
        if $i + 1 < $chars.len() {
            $i += 1;
        }
    };
}

/// Prints a warning if there was a type cast and it wasn't [`explicit`]
///
/// ie this will warn:
///
/// ```
/// fn foo(i64 x) {
///     return x * 2;
/// }
///
/// fn main() {
///     i32 x = 10;
///     foo(x); // Implicit cast from i32 to i64
/// }
/// ```
///
/// but this will not:
///
/// ```
/// fn foo(i64 x) {
///     return x * 2;
/// }
///
/// fn main() {
///     i32 x = 10;
///     foo((i64)x); // No warning
/// }
/// ```
///
/// [`explicit`]: $explicit:expr
#[macro_export]
macro_rules! cast_warning {
    ($explicit:expr, $location:expr, $first:expr, $second:expr, $warnings:expr, $warning:expr $(,)?) => {
        if !$explicit && $warnings.has_warning($warning) {
            println!(
                "{}",
                $location.warning(format!(
                    "Implicit casting from {} to {}",
                    $first.display(),
                    $second.display()
                ))
            );
        }
    };
}
