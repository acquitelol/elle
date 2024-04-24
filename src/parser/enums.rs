use crate::lexer::enums::{Token, TokenKind, ValueKind};

#[derive(Debug, Clone)]
pub enum AstNode {
    LiteralStatement {
        kind: TokenKind,
        value: ValueKind,
    },
    DeclareStatement {
        name: String,
        r#type: String,
        value: Box<AstNode>,
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
        body: Box<AstNode>,
        else_body: Box<AstNode>,
    },
    ForStatement {
        iterator: Token,
        enumerator: Box<AstNode>,
        body: Box<AstNode>,
    },
    WhileStatement {
        condition: Box<AstNode>,
        body: Box<AstNode>,
    },
    MatchStatement {
        identifier: Box<AstNode>,
        cases: Vec<Case>,
    },
    BufferStatement {
        name: String,
        r#type: String,
        size: ValueKind,
    },
    StoreStatement {
        name: String,
        r#type: String,
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
        arguments: Vec<Argument>,
        r#return: String,
        body: Vec<AstNode>,
    },
    Constant {
        name: String,
        public: bool,
        r#type: String,
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
    pub r#type: String,
}
