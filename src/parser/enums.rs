use std::{cell::RefCell, collections::HashMap};

use crate::{
    compiler::enums::Type,
    lexer::enums::{Location, Token, TokenKind, ValueKind},
};

use super::parser::StructPool;

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
        value_location: Location,
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
        treat_as_string: bool,
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
    /// Declares an array literal of size `values.len()` and values `values` and returns a pointer to the start of it
    ArrayStatement {
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
        is_deref: bool,
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
    /// Takes value `value` and flips all its bits
    BitwiseNotStatement {
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
        location: Location,
    },
    /// Creates a capturing closure that takes in some number of arguments
    /// and returns a single line statement result
    LambdaStatement {
        arguments: Vec<Argument>,
        value: Vec<AstNode>,
        location: Location,
    },
    /// Calculates the array length of an Elle-generated array
    /// Uses the formula *(array_ptr - #size(i32))
    ArrayLengthStatement {
        value: Box<AstNode>,
        location: Location,
    },
    /// An expression which allows you to declare a value to something conditionally.
    TernaryStatement {
        condition: Box<AstNode>,
        if_true: Box<AstNode>,
        if_false: Box<AstNode>,
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
    struct_pool: Option<&RefCell<StructPool>>,
    tree: Option<&RefCell<Vec<Primitive>>>,
) -> Vec<AstNode> {
    ast_nodes
        .into_iter()
        .map(|node| modify_type_in_node(node, generics, known_generics, struct_pool, tree))
        .collect()
}

fn modify_type_in_node(
    mut node: AstNode,
    generics: &Vec<String>,
    known_types: &HashMap<String, Type>,
    struct_pool: Option<&RefCell<StructPool>>,
    tree: Option<&RefCell<Vec<Primitive>>>,
) -> AstNode {
    match &mut node {
        AstNode::LiteralStatement { .. } => {}
        AstNode::DeclareStatement { r#type, value, .. } => {
            if let Some(ty) = r#type {
                *ty = modify_type(ty.clone(), generics, known_types, struct_pool, tree);
            }

            let new_value =
                modify_type_in_node(*value.clone(), generics, known_types, struct_pool, tree);
            *value = Box::new(new_value);
        }
        AstNode::LambdaStatement {
            arguments, value, ..
        } => {
            for arg in arguments.iter_mut() {
                arg.r#type =
                    modify_type(arg.r#type.clone(), generics, known_types, struct_pool, tree);
            }

            *value = modify_type_in_ast(value.clone(), generics, known_types, struct_pool, tree);
        }
        AstNode::ReturnStatement { value, .. } => {
            let new_value =
                modify_type_in_node(*value.clone(), generics, known_types, struct_pool, tree);
            *value = Box::new(new_value);
        }
        AstNode::NextStatement { r#type, .. } => {
            if let Some(ty) = r#type {
                *ty = modify_type(ty.clone(), generics, known_types, struct_pool, tree);
            }
        }
        AstNode::VariadicStatement { size, .. } => {
            let new_size =
                modify_type_in_node(*size.clone(), generics, known_types, struct_pool, tree);
            *size = Box::new(new_size);
        }
        AstNode::ArrayLengthStatement { value, .. } => {
            let new_value =
                modify_type_in_node(*value.clone(), generics, known_types, struct_pool, tree);
            *value = Box::new(new_value);
        }
        AstNode::BufferStatement { r#type, size, .. } => {
            if let Some(ty) = r#type {
                *ty = modify_type(ty.clone(), generics, known_types, struct_pool, tree);
            }
            let new_size =
                modify_type_in_node(*size.clone(), generics, known_types, struct_pool, tree);
            *size = Box::new(new_size);
        }
        AstNode::FunctionCall {
            parameters,
            generics: base_generics,
            ..
        } => {
            for (_, param) in parameters {
                let new_param =
                    modify_type_in_node(param.clone(), generics, known_types, struct_pool, tree);
                *param = new_param;
            }

            for generic in base_generics {
                *generic = modify_type(generic.clone(), generics, known_types, struct_pool, tree);
            }
        }
        AstNode::ArithmeticOperation { left, right, .. } => {
            let new_left =
                modify_type_in_node(*left.clone(), generics, known_types, struct_pool, tree);
            *left = Box::new(new_left);
            let new_right =
                modify_type_in_node(*right.clone(), generics, known_types, struct_pool, tree);
            *right = Box::new(new_right);
        }
        AstNode::TernaryStatement {
            condition,
            if_true,
            if_false,
            ..
        } => {
            let new_condition =
                modify_type_in_node(*condition.clone(), generics, known_types, struct_pool, tree);
            *condition = Box::new(new_condition);
            let new_if_true =
                modify_type_in_node(*if_true.clone(), generics, known_types, struct_pool, tree);
            *if_true = Box::new(new_if_true);
            let new_if_false =
                modify_type_in_node(*if_false.clone(), generics, known_types, struct_pool, tree);
            *if_false = Box::new(new_if_false);
        }
        AstNode::IfStatement {
            condition,
            body,
            else_body,
            ..
        } => {
            let new_condition =
                modify_type_in_node(*condition.clone(), generics, known_types, struct_pool, tree);
            *condition = Box::new(new_condition);
            *body = modify_type_in_ast(body.clone(), generics, known_types, struct_pool, tree);
            *else_body =
                modify_type_in_ast(else_body.clone(), generics, known_types, struct_pool, tree);
        }
        AstNode::WhileLoop {
            condition,
            step,
            body,
            ..
        } => {
            let new_condition =
                modify_type_in_node(*condition.clone(), generics, known_types, struct_pool, tree);
            *condition = Box::new(new_condition);
            if let Some(step_node) = step {
                let new_step = modify_type_in_node(
                    *step_node.clone(),
                    generics,
                    known_types,
                    struct_pool,
                    tree,
                );
                *step_node = Box::new(new_step);
            }
            *body = modify_type_in_ast(body.clone(), generics, known_types, struct_pool, tree);
        }
        AstNode::ArrayStatement { values, .. } => {
            for (_, value) in values {
                let new_value =
                    modify_type_in_node(value.clone(), generics, known_types, struct_pool, tree);
                *value = new_value;
            }
        }
        AstNode::StructStatement { values, .. } => {
            for (_, value) in values {
                let new_value =
                    modify_type_in_node(*value.clone(), generics, known_types, struct_pool, tree);
                *value = Box::new(new_value);
            }
        }
        AstNode::FieldStatement {
            left, right, value, ..
        } => {
            let new_left =
                modify_type_in_node(*left.clone(), generics, known_types, struct_pool, tree);
            *left = Box::new(new_left);
            let new_right =
                modify_type_in_node(*right.clone(), generics, known_types, struct_pool, tree);
            *right = Box::new(new_right);
            if let Some(val) = value {
                let new_value =
                    modify_type_in_node(*val.clone(), generics, known_types, struct_pool, tree);
                *value = Some(Box::new(new_value));
            }
        }
        AstNode::MemoryStatement {
            left, right, value, ..
        } => {
            let new_left =
                modify_type_in_node(*left.clone(), generics, known_types, struct_pool, tree);
            *left = Box::new(new_left);
            let new_right =
                modify_type_in_node(*right.clone(), generics, known_types, struct_pool, tree);
            *right = Box::new(new_right);
            if let Some(val) = value {
                let new_value =
                    modify_type_in_node(*val.clone(), generics, known_types, struct_pool, tree);
                *value = Some(Box::new(new_value));
            }
        }
        AstNode::DeferStatement { value, .. } => {
            let new_value =
                modify_type_in_node(*value.clone(), generics, known_types, struct_pool, tree);
            *value = Box::new(new_value);
        }
        AstNode::BlockStatement { body, .. } => {
            *body = modify_type_in_ast(body.clone(), generics, known_types, struct_pool, tree);
        }
        AstNode::NotStatement { value, .. } => {
            let new_value =
                modify_type_in_node(*value.clone(), generics, known_types, struct_pool, tree);
            *value = Box::new(new_value);
        }
        AstNode::BitwiseNotStatement { value, .. } => {
            let new_value =
                modify_type_in_node(*value.clone(), generics, known_types, struct_pool, tree);
            *value = Box::new(new_value);
        }
        AstNode::AddressStatement { value, .. } => {
            let new_value =
                modify_type_in_node(*value.clone(), generics, known_types, struct_pool, tree);
            *value = Box::new(new_value);
        }
        AstNode::ConversionStatement { r#type, value, .. } => {
            if let Some(ty) = r#type {
                *ty = modify_type(ty.clone(), generics, known_types, struct_pool, tree);
            }
            let new_value =
                modify_type_in_node(*value.clone(), generics, known_types, struct_pool, tree);
            *value = Box::new(new_value);
        }
        AstNode::SizeStatement { value, .. } => match value {
            Ok(ty) => {
                *ty = modify_type(ty.clone(), generics, known_types, struct_pool, tree);
            }
            Err(ast_node) => {
                let new_ast_node = modify_type_in_node(
                    *ast_node.clone(),
                    generics,
                    known_types,
                    struct_pool,
                    tree,
                );
                *ast_node = Box::new(new_ast_node);
            }
        },
    }

    node
}

fn modify_type(
    ty: Type,
    generics: &Vec<String>,
    known_types: &HashMap<String, Type>,
    struct_pool: Option<&RefCell<StructPool>>,
    tree: Option<&RefCell<Vec<Primitive>>>,
) -> Type {
    ty.unknown_to_known(struct_pool, tree, generics.clone(), known_types.clone())
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
        keyword_location: Location,
        location: Location,
        ignore_empty: bool,
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
        format: bool,
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
#[allow(unused)]
pub struct Case {
    pub condition: Vec<AstNode>,
    pub body: Box<AstNode>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Argument {
    pub name: String,
    pub r#type: Type,
    pub manual: bool,
}
