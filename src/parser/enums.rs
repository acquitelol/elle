use crate::{
    compiler::enums::Type,
    lexer::enums::{TokenKind, ValueKind},
};

#[derive(Debug, Clone)]
pub enum AstNode {
    LiteralStatement {
        kind: TokenKind,
        value: ValueKind,
    },
    DeclareStatement {
        name: String,
        r#type: Option<Type>,
        value: Box<AstNode>,
    },
    VariadicStatement {
        name: String,
        size: Box<AstNode>,
    },
    NextStatement {
        name: String,
        r#type: Option<Type>,
    },
    ReturnStatement {
        value: Box<AstNode>,
    },
    FunctionCall {
        name: String,
        parameters: Vec<AstNode>,
    },
    ArithmeticOperation {
        left: Box<AstNode>,
        right: Box<AstNode>,
        operator: TokenKind,
    },
    IfStatement {
        condition: Box<AstNode>,
        body: Vec<AstNode>,
        else_body: Vec<AstNode>,
    },
    WhileLoop {
        condition: Box<AstNode>,
        body: Vec<AstNode>,
    },
    BufferStatement {
        name: String,
        r#type: Option<Type>,
        size: ValueKind,
    },
    StoreStatement {
        name: String,
        offset: Box<AstNode>,
        value: Box<AstNode>,
    },
    LoadStatement {
        name: String,
        offset: Box<AstNode>,
    },
    DeferStatement {
        value: Box<AstNode>,
    },
    BlockStatement {
        body: Vec<AstNode>,
    },
    Conversion {
        r#type: Option<Type>,
        value: Box<AstNode>,
    },
}

#[derive(Debug, Clone)]
pub enum Primitive {
    Use {
        library: String,
        module: String,
        functions: Vec<String>,
    },
    Operation {
        name: String,
        public: bool,
        variadic: bool,
        manual: bool,
        arguments: Vec<Argument>,
        r#return: Option<Type>,
        body: Vec<AstNode>,
    },
    Constant {
        name: String,
        public: bool,
        r#type: Option<Type>,
        value: ValueKind,
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
