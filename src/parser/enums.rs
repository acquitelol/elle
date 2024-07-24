use crate::{
    compiler::enums::Type,
    lexer::enums::{Token, TokenKind, ValueKind},
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
        step: Option<Box<AstNode>>,
        body: Vec<AstNode>,
    },
    BufferStatement {
        name: String,
        r#type: Option<Type>,
        size: Box<AstNode>,
    },
    ArrayStatement {
        size: Box<AstNode>,
        values: Vec<AstNode>,
    },
    StoreStatement {
        left: Box<AstNode>,
        right: Box<AstNode>,
        value: Box<AstNode>,
    },
    LoadStatement {
        left: Box<AstNode>,
        right: Box<AstNode>,
    },
    DeferStatement {
        value: Box<AstNode>,
    },
    BlockStatement {
        body: Vec<AstNode>,
    },
    NotStatement {
        value: Box<AstNode>,
    },
    AddressStatement {
        name: String,
    },
    ConversionStatement {
        r#type: Option<Type>,
        value: Box<AstNode>,
    },
    SizeStatement {
        // Easy way to return 2 values without a special enum
        value: Result<Type, Box<AstNode>>,
        standalone: bool,
    },
}

impl AstNode {
    pub fn token_to_literal(token: Token) -> AstNode {
        Self::LiteralStatement {
            kind: token.kind,
            value: token.value,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Primitive {
    Use {
        library: String,
        module: String,
        functions: Vec<String>,
    },
    Function {
        name: String,
        public: bool,
        variadic: bool,
        manual: bool,
        external: bool,
        arguments: Vec<Argument>,
        r#return: Option<Type>,
        body: Vec<AstNode>,
    },
    Constant {
        name: String,
        public: bool,
        r#type: Option<Type>,
        value: Box<AstNode>,
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
