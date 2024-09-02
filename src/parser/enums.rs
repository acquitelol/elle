use crate::{
    compiler::enums::Type,
    lexer::enums::{Location, Token, TokenKind, ValueKind},
};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum AstNode {
    /// Holds identifiers, literals, inline IR
    LiteralStatement {
        kind: TokenKind,
        value: ValueKind,
        location: Location,
    },
    /// A declaration of name `name` with type `r#type` to value `value
    DeclareStatement {
        name: String,
        r#type: Option<Type>,
        value: Box<AstNode>,
        location: Location,
    },
    /// Allocates stack memory of size `size`, assigns it to `name`, and calls `vastart` on it
    VariadicStatement {
        name: String,
        size: Box<AstNode>,
        location: Location,
    },
    /// Yields a new argument of type `r#type` from `name`
    NextStatement {
        name: String,
        r#type: Option<Type>,
        location: Location,
    },
    /// Returns value `value`
    ReturnStatement {
        value: Box<AstNode>,
        location: Location,
    },
    /// Calls function `name` with parameters `parameters`
    FunctionCall {
        name: String,
        parameters: Vec<(Location, AstNode)>,
        type_method: bool,
        ignore_no_def: bool,
        location: Location,
    },
    /// Performs an arithmetic operation with `operator` using `left` and `right
    ArithmeticOperation {
        left: Box<AstNode>,
        right: Box<AstNode>,
        operator: TokenKind,
        location: Location,
    },
    /// Runs `body` if condition `condition` is true, otherwise runs `else_body`
    IfStatement {
        condition: Box<AstNode>,
        body: Vec<AstNode>,
        else_body: Vec<AstNode>,
        location: Location,
    },
    /// Runs `body` while condition `condition` is true, using step `step`
    /// (`step` is used for easy merging between while loops and for loops)
    WhileLoop {
        condition: Box<AstNode>,
        step: Option<Box<AstNode>>,
        body: Vec<AstNode>,
        location: Location,
    },
    /// Declares a buffer named `name` with an inner type `r#type` and size `size`
    BufferStatement {
        name: String,
        r#type: Option<Type>,
        size: Box<AstNode>,
        location: Location,
    },
    /// Declares an array literal of size `size` and values `values` and returns a pointer to the start of it
    ArrayStatement {
        size: Box<AstNode>,
        values: Vec<(Location, AstNode)>,
        location: Location,
    },
    /// Declares a struct named `name` with values `values`
    StructStatement {
        name: String,
        values: Vec<(String, Box<AstNode>)>,
        location: Location,
    },
    /// Accesses the fields of a struct, optionally assigning a value to the result
    FieldStatement {
        left: Box<AstNode>,
        right: Box<AstNode>,
        value: Option<Box<AstNode>>,
        location: Location,
    },
    /// Loads or stores information from a pointer through pointer arithmetic
    /// In an expression like a[10], left is `a` and right is `10`
    MemoryStatement {
        left: Box<AstNode>,
        right: Box<AstNode>,
        value: Option<Box<AstNode>>,
        left_location: Location,
        right_location: Location,
        value_location: Location,
    },
    /// Only executes code from value `value` when the current scope is about to exit
    /// This can be function return or an implicit scope exit through `break` or `continue`
    DeferStatement {
        value: Box<AstNode>,
        location: Location,
    },
    /// A standalone block that executes code in its scope
    /// This can be useful for micro-managing memory allocation with defer
    BlockStatement {
        body: Vec<AstNode>,
        location: Location,
    },
    /// Takes value `value` and negates it (compares it to 0)
    NotStatement {
        value: Box<AstNode>,
        location: Location,
    },
    /// Returns the address of a some `value`
    AddressStatement {
        value: Box<AstNode>,
        location: Location,
    },
    /// Performs an explicit conversion of value `value` to type `r#type`
    ConversionStatement {
        r#type: Option<Type>,
        value: Box<AstNode>,
        location: Location,
    },
    /// Returns the size (in bytes) or length, depending on if `standalone` is set to true
    /// The result is used to allow for getting the size of both expressions and types
    SizeStatement {
        value: Result<Type, Box<AstNode>>,
        standalone: bool,
        location: Location,
    },
}

impl AstNode {
    pub fn token_to_literal(token: Token) -> AstNode {
        Self::LiteralStatement {
            kind: token.kind,
            value: token.value,
            location: token.location,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Primitive {
    Use {
        module: String,
        generics: Vec<Type>,
        location: Location,
    },
    Struct {
        name: String,
        public: bool,
        usable: bool,
        imported: bool,
        members: Vec<Argument>,
        location: Location,
    },
    Function {
        name: String,
        public: bool,
        usable: bool,
        imported: bool,
        variadic: bool,
        manual: bool,
        external: bool,
        builtin: bool,
        volatile: bool,
        unaliased: Option<String>,
        arguments: Vec<Argument>,
        r#return: Option<Type>,
        body: Vec<AstNode>,
        location: Location,
    },
    Constant {
        name: String,
        public: bool,
        usable: bool,
        imported: bool,
        r#type: Option<Type>,
        value: Box<AstNode>,
        location: Location,
    },
}

#[derive(Debug, Clone)]
pub struct Case {
    pub condition: Vec<AstNode>,
    pub body: Box<AstNode>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Argument {
    pub name: String,
    pub r#type: Type,
}
