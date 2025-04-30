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

#[macro_export]
macro_rules! global {
    ($name:ident : $type:ty = $value:expr, $getter:ident) => {
        pub static mut $name: Option<$type> = Some($value);

        macro_rules! $getter {
            () => {
                unsafe { $name.unwrap() }
            };
        }

        #[allow(unused)]
        pub(crate) use $getter;
    };
}

#[macro_export]
macro_rules! elle_error {
    ($loc:expr) => {{
        let _ = std::fs::remove_dir_all(unsafe { $crate::misc::constants::BUILD_PATH.unwrap() });

        // Panic in debug mode so you can see the line number where the error occured in the compiler
        if cfg!(debug_assertions) {
            panic!("{}", $loc);
        }

        eprintln!("{}", $loc);
        std::process::exit(1);
    }};
}

#[macro_export]
macro_rules! is_generic {
    ($name:expr $(,)?) => {
        $name.contains(&format!(".{}.", $crate::GENERIC_IDENTIFIER))
    };
}

#[macro_export]
macro_rules! is_unknown {
    ($name:expr $(,)?) => {
        $name.contains(&format!(".{}.", $crate::GENERIC_UNKNOWN))
    };
}

/// Removes a symbol (function, constant, struct) named [`name`]
///
/// Sets the [`usable`] and [`imported`] property on the path [`val`]
///
/// [`name`]: $name:expr
/// [`val`]: $val:path
#[macro_export]
macro_rules! override_and_add_node {
    ($val:path, $tree:expr, $name:expr, $symbol:expr, $public:expr $(,)?) => {
        if let Some(index) = existing_definition($tree, $name) {
            $tree.remove(index);
        }

        let mut new_symbol = $symbol.clone();
        if let $val(ref mut this) = new_symbol {
            this.usable = $public;
            this.imported = true;
        }

        $tree.insert(0, new_symbol);
        // $tree.push(new_symbol);
    };
}

/// Returns a formatted string with an ANSI color depending on
/// the [`elapsed`] time provided using [`colors`]
///
/// There are three possible results:
///
/// - Green (< 500ms)
/// - Yellow (< 2000ms)
/// - Red (Anything else)
///
/// [`elapsed`]: $elapsed:expr
/// [`colors`]: crate::lexer::colors
#[macro_export]
macro_rules! elapsed_with_color {
    ($elapsed:expr) => {{
        let color = match $elapsed.as_millis() {
            val if val < 1000 => $crate::misc::colors::get_GREEN!(),
            val if val < 3000 => $crate::misc::colors::get_YELLOW!(),
            _ => $crate::misc::colors::get_RED!(),
        };

        format!(
            "{color}{:?}{}",
            $elapsed,
            $crate::misc::colors::get_RESET!()
        )
    }};
}

/// Throws an error informing the user that [`self.current_token()`]
/// was not a valid struct or primitive type
#[macro_export]
macro_rules! not_valid_struct_or_type {
    ($self:expr $(,)?) => {{
        let name = $self.current_token().value.get_string_inner().unwrap();

        elle_error!($self.current_token().location.borrow().error(format!(
            "Identifier '{}' isn't a struct or primitive type.\n{}",
            name.clone(),
            if let Some(map) = ValueKind::similar_mapping(&name) {
                format!(
                    "A similar type exists which might be what you need: '{}'",
                    map
                )
            } else {
                format!("Are you sure you spelt '{}' correctly?", name)
            }
        )))
    }};
}

#[macro_export]
macro_rules! unknown_field {
    ($struct:expr, $struct_name:expr, $name:expr, $location:expr $(,)?) => {{
        let mut similar_name = None;
        let mut lowest_distance = usize::MAX;

        for arg in $struct.1.iter().map(|arg| arg.name.clone()) {
            let contains_name = arg.contains($name.as_str());
            let distance = levenshtein::levenshtein($name.as_str(), arg.clone().as_str());

            if contains_name
                && (distance <= lowest_distance || similar_name.is_none())
                && distance <= 3
            {
                lowest_distance = distance;
                similar_name = Some(arg.clone());
            } else if !contains_name
                && distance < lowest_distance
                && similar_name.is_none()
                && distance <= 3
            {
                lowest_distance = distance;
                similar_name = Some(arg.clone());
            }
        }

        $location.borrow().error(format!(
            "Could not find a field named '{}' for struct '{}'{}",
            $name.clone(),
            $struct_name.display(),
            if let Some(similar) = similar_name {
                format!("\nA field with a similar name exists: '{}'", similar)
            } else {
                "".into()
            }
        ))
    }};
}

#[macro_export]
macro_rules! unknown_function {
    ($location:expr, $name:expr, $module:expr $(,)?) => {{
        let mut similar_name = None;
        let mut lowest_distance = usize::MAX;

        for (_, func) in $module.borrow_mut().functions.iter().filter(|(_, func)| {
            func.name != "nil" && func.name != "main" && (func.usable || func.imported)
        }) {
            let distance = levenshtein::levenshtein($name.as_str(), func.name.clone().as_str());

            if distance <= lowest_distance && distance <= 3 {
                lowest_distance = distance;
                similar_name = Some(func.name.clone());
            }
        }

        elle_error!($location.borrow().error(format!(
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
        )))
    }};
}

#[macro_export]
macro_rules! bool_hover {
    ($token:expr, $location:expr, $value:literal) => {
        if $token.tagged {
            elle_error!(format!(
                "hover\n{}\n{}\n{}: bool", // TODO: is there any way to unhardcode this?
                $location.borrow().display_plain(false),
                $location.borrow().display_plain(true),
                $value
            ));
        }
    };
}

#[macro_export]
macro_rules! struct_hover {
    ($token:expr, $is_namespace:expr, $members:expr) => {
        if $token.tagged {
            if $is_namespace {
                elle_error!(format!(
                    "hover\n{}\n{}\nnamespace {};",
                    $token.location.borrow().display_plain(false),
                    $token.location.borrow().display_plain(true),
                    Type::Struct($token.value.get_string_inner().unwrap()).display(),
                ));
            }

            elle_error!(format!(
                "hover\n{}\n{}\nstruct {} {{\n{}\n}};",
                $token.location.borrow().display_plain(false),
                $token.location.borrow().display_plain(true),
                Type::Struct($token.value.get_string_inner().unwrap()).display(),
                $members
                    .iter()
                    .map(|x| format!("\t{} {};", x.r#type.display(), x.name))
                    .collect::<Vec<String>>()
                    .join("\n")
            ));
        }
    };
}

#[macro_export]
macro_rules! enum_hover {
    ($token:expr, $name:expr, $variants:expr) => {
        if $token.tagged {
            elle_error!(format!(
                "hover\n{}\n{}\nenum {} {{\n{}\n}};",
                $token.location.borrow().display_plain(false),
                $token.location.borrow().display_plain(true),
                $name,
                $variants
                    .iter()
                    .map(|x| format!("\t{} = {}", x.name, x.value.value.wrapped_display()))
                    .collect::<Vec<_>>()
                    .join(",\n")
            ));
        }
    };
}

/// Handy shorthand for setting the end of a location range
#[macro_export]
macro_rules! set_end {
    ($location:expr, $self:expr) => {
        let loc = $self.current_token().location.borrow().end.clone();
        $location.borrow_mut().end = loc;
    };
}

/// Simple macro to report an error when at the end of a token stream but getting an incorrect token
#[macro_export]
macro_rules! expect_eot {
    ($token:expr) => {
        elle_error!($token.location.borrow().error(format!(
            "Expected end of expression but got {:?} instead.\nPerhaps you forgot a delimiter (semicolon, comma) here?",
            $token.kind
        )))
    };
}

#[macro_export]
macro_rules! is_type {
    ($token:expr, $pools:expr, $generics:expr, $restricted:literal) => {{
        let ty_name = $token.value.get_string_inner().unwrap_or("".into());

        $token.kind == TokenKind::Identifier
            && ($token.value.is_base_type()
                || $pools.struct_pool.borrow().contains_key(&ty_name)
                || $pools.enum_pool.borrow().contains_key(&ty_name)
                || $generics.contains(&ty_name))
            || (!$restricted && $token.kind == TokenKind::LeftParenthesis)
            || $token.kind == TokenKind::Function
    }};
}

/// Converts a token [`token`] into an `AstNode`
///
/// This accounts for [`TrueLiteral`, `FalseLiteral`, `FloatingPoint`]
///
/// [`token`]: $token:expr
#[macro_export]
macro_rules! token_to_node {
    ($token:expr, $self:expr) => {
        match $token.kind {
            TokenKind::TrueLiteral => {
                $crate::bool_hover!($token, $self.current_token().location, true);

                AstNode::Literal(Literal {
                    kind: TokenKind::BoolLiteral,
                    value: ValueKind::Number(1),
                    location: $token.location.clone(),
                    tagged: $token.tagged,
                })
            }
            TokenKind::FalseLiteral => {
                $crate::bool_hover!($token, $self.current_token().location, false);

                AstNode::Literal(Literal {
                    kind: TokenKind::BoolLiteral,
                    value: ValueKind::Number(0),
                    location: $token.location.clone(),
                    tagged: $token.tagged,
                })
            }
            TokenKind::FloatingPoint => $self.parse_float($token),
            _ => AstNode::Literal(Literal {
                kind: $token.kind.clone(),
                value: $token.value.clone(),
                location: $token.location.clone(),
                tagged: $token.tagged,
            }),
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
                $self.current_token().location.borrow().error(
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
