use crate::{
    compiler::enums::Type,
    lexer::enums::{Location, Token, TokenKind, ValueKind},
};

#[derive(Debug, Clone)]
pub enum AstNode {
    LiteralStatement {
        kind: TokenKind,
        value: ValueKind,
        location: Location,
    },
    DeclareStatement {
        name: String,
        r#type: Option<Type>,
        value: Box<AstNode>,
        location: Location,
    },
    VariadicStatement {
        name: String,
        size: Box<AstNode>,
        location: Location,
    },
    NextStatement {
        name: String,
        r#type: Option<Type>,
        location: Location,
    },
    ReturnStatement {
        value: Box<AstNode>,
        location: Location,
    },
    FunctionCall {
        name: String,
        parameters: Vec<(Location, AstNode)>,
        location: Location,
    },
    ArithmeticOperation {
        left: Box<AstNode>,
        right: Box<AstNode>,
        operator: TokenKind,
        location: Location,
    },
    IfStatement {
        condition: Box<AstNode>,
        body: Vec<AstNode>,
        else_body: Vec<AstNode>,
        location: Location,
    },
    WhileLoop {
        condition: Box<AstNode>,
        step: Option<Box<AstNode>>,
        body: Vec<AstNode>,
        location: Location,
    },
    BufferStatement {
        name: String,
        r#type: Option<Type>,
        size: Box<AstNode>,
        location: Location,
    },
    ArrayStatement {
        size: Box<AstNode>,
        values: Vec<AstNode>,
        location: Location,
    },
    StructStatement {
        name: String,
        values: Vec<(String, Box<AstNode>)>,
        location: Location,
    },
    // Ideally: FieldStatement<Foo, FieldStatement<Bar, Baz>>
    FieldStatement {
        left: Box<AstNode>,
        right: Box<AstNode>,
        value: Option<Box<AstNode>>,
        location: Location,
    },
    MemoryStatement {
        left: Box<AstNode>,
        right: Box<AstNode>,
        value: Option<Box<AstNode>>,
        location: Location,
    },
    DeferStatement {
        value: Box<AstNode>,
        location: Location,
    },
    BlockStatement {
        body: Vec<AstNode>,
        location: Location,
    },
    NotStatement {
        value: Box<AstNode>,
        location: Location,
    },
    AddressStatement {
        name: String,
        location: Location,
    },
    ConversionStatement {
        r#type: Option<Type>,
        value: Box<AstNode>,
        location: Location,
    },
    SizeStatement {
        // Easy way to return 2 values without a special enum
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

#[derive(Debug, Clone)]
pub enum Primitive {
    Use {
        module: String,
        functions: Vec<String>,
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

#[derive(Debug, Clone)]
pub struct Argument {
    pub name: String,
    pub r#type: Type,
}
