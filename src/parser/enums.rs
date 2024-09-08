use std::collections::HashMap;

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
        generics: Vec<Type>,
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

pub fn modify_type_in_ast(
    ast_nodes: Vec<AstNode>,
    generics: &Vec<String>,
    known_generics: &HashMap<String, Type>,
) -> Vec<AstNode> {
    ast_nodes
        .into_iter()
        .map(|node| modify_type_in_node(node, generics, known_generics))
        .collect()
}

fn modify_type_in_node(
    mut node: AstNode,
    generics: &Vec<String>,
    known_types: &HashMap<String, Type>,
) -> AstNode {
    match &mut node {
        AstNode::DeclareStatement { r#type, value, .. } => {
            if let Some(ty) = r#type {
                *ty = modify_type(ty.clone(), generics, known_types);
            }
            let new_value = modify_type_in_node(*value.clone(), generics, known_types);
            *value = Box::new(new_value);
        }
        AstNode::NextStatement { r#type, .. } => {
            if let Some(ty) = r#type {
                *ty = modify_type(ty.clone(), generics, known_types);
            }
        }
        AstNode::BufferStatement { r#type, size, .. } => {
            if let Some(ty) = r#type {
                *ty = modify_type(ty.clone(), generics, known_types);
            }
            let new_size = modify_type_in_node(*size.clone(), generics, known_types);
            *size = Box::new(new_size);
        }
        AstNode::FunctionCall {
            parameters,
            generics: base_generics,
            ..
        } => {
            for (_, param) in parameters {
                let new_param = modify_type_in_node(param.clone(), generics, known_types);
                *param = new_param;
            }

            for generic in base_generics {
                *generic = modify_type(generic.clone(), generics, known_types);
            }
        }
        AstNode::ArithmeticOperation { left, right, .. } => {
            let new_left = modify_type_in_node(*left.clone(), generics, known_types);
            *left = Box::new(new_left);
            let new_right = modify_type_in_node(*right.clone(), generics, known_types);
            *right = Box::new(new_right);
        }
        AstNode::IfStatement {
            condition,
            body,
            else_body,
            ..
        } => {
            let new_condition = modify_type_in_node(*condition.clone(), generics, known_types);
            *condition = Box::new(new_condition);
            *body = modify_type_in_ast(body.clone(), generics, known_types);
            *else_body = modify_type_in_ast(else_body.clone(), generics, known_types);
        }
        AstNode::WhileLoop {
            condition,
            step,
            body,
            ..
        } => {
            let new_condition = modify_type_in_node(*condition.clone(), generics, known_types);
            *condition = Box::new(new_condition);
            if let Some(step_node) = step {
                let new_step = modify_type_in_node(*step_node.clone(), generics, known_types);
                *step_node = Box::new(new_step);
            }
            *body = modify_type_in_ast(body.clone(), generics, known_types);
        }
        AstNode::ArrayStatement { size, values, .. } => {
            let new_size = modify_type_in_node(*size.clone(), generics, known_types);
            *size = Box::new(new_size);
            for (_, value) in values {
                let new_value = modify_type_in_node(value.clone(), generics, known_types);
                *value = new_value;
            }
        }
        AstNode::StructStatement { values, .. } => {
            for (_, value) in values {
                let new_value = modify_type_in_node(*value.clone(), generics, known_types);
                *value = Box::new(new_value);
            }
        }
        AstNode::FieldStatement {
            left, right, value, ..
        } => {
            let new_left = modify_type_in_node(*left.clone(), generics, known_types);
            *left = Box::new(new_left);
            let new_right = modify_type_in_node(*right.clone(), generics, known_types);
            *right = Box::new(new_right);
            if let Some(val) = value {
                let new_value = modify_type_in_node(*val.clone(), generics, known_types);
                *value = Some(Box::new(new_value));
            }
        }
        AstNode::MemoryStatement {
            left, right, value, ..
        } => {
            let new_left = modify_type_in_node(*left.clone(), generics, known_types);
            *left = Box::new(new_left);
            let new_right = modify_type_in_node(*right.clone(), generics, known_types);
            *right = Box::new(new_right);
            if let Some(val) = value {
                let new_value = modify_type_in_node(*val.clone(), generics, known_types);
                *value = Some(Box::new(new_value));
            }
        }
        AstNode::DeferStatement { value, .. } => {
            let new_value = modify_type_in_node(*value.clone(), generics, known_types);
            *value = Box::new(new_value);
        }
        AstNode::BlockStatement { body, .. } => {
            *body = modify_type_in_ast(body.clone(), generics, known_types);
        }
        AstNode::NotStatement { value, .. } => {
            let new_value = modify_type_in_node(*value.clone(), generics, known_types);
            *value = Box::new(new_value);
        }
        AstNode::AddressStatement { value, .. } => {
            let new_value = modify_type_in_node(*value.clone(), generics, known_types);
            *value = Box::new(new_value);
        }
        AstNode::ConversionStatement { r#type, value, .. } => {
            if let Some(ty) = r#type {
                *ty = modify_type(ty.clone(), generics, known_types);
            }
            let new_value = modify_type_in_node(*value.clone(), generics, known_types);
            *value = Box::new(new_value);
        }
        AstNode::SizeStatement { value, .. } => match value {
            Ok(ty) => {
                *ty = modify_type(ty.clone(), generics, known_types);
            }
            Err(ast_node) => {
                let new_ast_node = modify_type_in_node(*ast_node.clone(), generics, known_types);
                *ast_node = Box::new(new_ast_node);
            }
        },
        _ => {}
    }
    node
}

fn modify_type(ty: Type, generics: &Vec<String>, known_types: &HashMap<String, Type>) -> Type {
    ty.unknown_to_known(None, None, generics.clone(), known_types.clone())
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Primitive {
    Use {
        module: String,
        location: Location,
    },
    Struct {
        name: String,
        public: bool,
        usable: bool,
        imported: bool,
        generics: Vec<String>,
        known_generics: HashMap<String, Type>,
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
        generics: Vec<String>,
        arguments: Vec<Argument>,
        r#return: Option<Type>,
        body: Vec<AstNode>,
        location: Location,
        return_location: Location,
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
