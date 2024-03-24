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
        operator: char,
    },
    IfStatement {
        condition: Box<AstNode>,
        trueBody: Box<AstNode>,
        falseBody: Box<AstNode>,
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
}

#[derive(Debug)]
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
}

#[derive(Debug, Clone)]
pub struct Case {
    pub condition: Vec<AstNode>,
    pub body: Box<AstNode>,
}

#[derive(Debug)]
pub struct Argument {
    pub name: String,
    pub r#type: String,
}
