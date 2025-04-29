; Identifiers
(identifier) @variable
(qualified_identifier) @variable

; Highlight some identifiers with variable.special
((identifier) @variable.special
    (#any-of? @variable.special
        "self"))

; Highlight the ElleMeta one
(parameter
  type: (type) @type
  name: (identifier) @variable.special
  (#eq? @type "ElleMeta"))

(attribute_parameter
  type: (type) @type
  name: (identifier) @variable.special
  (#eq? @type "ElleMeta"))

; Assume uppercase names are structs
((identifier) @type
    (#match? @type "^[A-Z]"))

; Assume all-caps names are constants
((identifier) @constant
    (#match? @constant "^[A-Z][A-Z\\d_]+$"))

; If the regex doesn't match, at least it's right once
(enum_definition (identifier) @type)
(struct_definition (identifier) @type)
(constant_definition (identifier) @constant)

; Types
(type) @type
(type (identifier) @type)
(array_type (type) @type)
(pointer_type) @type
(tuple_type) @type

(generic_type) @type
(generic_parameters (identifier) @type)

; hmm for primitives, @type or @keyword?
"void" @type
"bool" @type
"char" @type
"i8" @type
"i16" @type
"i32" @type
"i64" @type
"f32" @type
"f64" @type
"string" @type
"any" @type

; Constants and literals
(numeric_literal) @number
(string_literal) @string
(character_literal) @string
(boolean_literal) @boolean
(nil_literal) @keyword
(escape_sequence) @string.escape

; Comments
(comment) @comment
(shebang) @comment

; Keywords
"pub" @keyword
"!pub" @keyword
"external" @keyword
"!external" @keyword
"fn" @keyword
"if" @keyword
"else" @keyword
"while" @keyword
"for" @keyword
"in" @keyword
"defer" @keyword
"return" @keyword
"break" @keyword
"continue" @keyword
"struct" @keyword
"const" @keyword
"use" @keyword
"namespace" @keyword
"global" @keyword
"let" @keyword
"variadic" @keyword
"enum" @keyword
"type" @keyword

; Function definition
(function_definition name: (identifier) @function)
(function_definition name: (qualified_identifier) @function)
(function_definition name: (exact_literal) @function)

; Function calls
(call_expression function: (identifier) @function)
(call_expression function: (qualified_identifier) @function)
(call_expression function: (member_expression property: (identifier) @function))
(call_expression function: (exact_literal) @function)

; Directives and attributes
(lsp_type_definition directive: (directive_expression name: _ @function))
(lsp_type_definition (identifier) @type)
(directive_expression name: _ @function)
(attribute (identifier) @keyword)

(qualified_identifier (identifier) @function)
(qualified_identifier
    module: (identifier) @type
    (identifier) @function)

; Struct fields
(struct_field (identifier) @property)
(enum_variant (identifier) @property)
(struct_field_initializer (identifier) @property)

; Struct literals
(struct_literal name: (identifier) @type)

; Member access
(member_expression property: (identifier) @property)
(yield_expression property: "yield" @property)

; Operators
[
  "+"
  "-"
  "*"
  "/"
  "%"
  "&"
  "|"
  "^"
  "=="
  "!="
  "<"
  ">"
  "<="
  ">="
  "<<"
  ">>"
  "&&"
  "||"
  "<>"
  "!"
  "~"
  "?"
  ":"
] @operator

; Assignment operators
[
  "="
  "+="
  "-="
  "*="
  "/="
  "<>="
  ":="
  "%="
  "^="
  "|="
  "&="
  ">>="
  "<<="
] @operator

; Punctuation
[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

[
  ","
  "."
  "::"
] @punctuation.delimiter

"..." @punctuation.special

[
  "#"
  ";"
  "->"
  "@"
] @punctuation

; Import statements
(import_statement (module_path) @primary)
(import_statement (module_path "/" @punctuation.delimiter))
(import_statement (module_path "." @punctuation.delimiter))
(import_statement (module_path ".." @punctuation.delimiter))
