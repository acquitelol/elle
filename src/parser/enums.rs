use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    compiler::enums::Type,
    lexer::enums::{Location, Token, TokenKind, ValueKind},
};

use super::parser::StructPool;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Declare {
    pub name: String,
    pub r#type: Option<Type>,
    pub value: Option<Box<AstNode>>,
    pub location: Rc<Location>,
    pub value_location: Rc<Location>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Return {
    pub value: Box<AstNode>,
    pub location: Rc<Location>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BinaryOperation {
    pub left: Box<AstNode>,
    pub right: Box<AstNode>,
    pub operator: TokenKind,
    pub treat_as_string: bool,
    pub dunder_methods: bool,
    pub location: Rc<Location>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Literal {
    pub kind: TokenKind,
    pub value: ValueKind,
    pub location: Rc<Location>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionCall {
    pub name: String,
    pub generics: Vec<Type>,
    pub parameters: Vec<(Rc<Location>, AstNode)>,
    pub type_method: bool,
    pub ignore_no_def: bool,
    pub location: Rc<Location>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Buffer {
    pub name: String,
    pub r#type: Option<Type>,
    pub size: Box<AstNode>,
    pub location: Rc<Location>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MemoryOperation {
    pub left: Box<AstNode>,
    pub right: Box<AstNode>,
    pub value: Option<Box<AstNode>>,
    pub left_location: Rc<Location>,
    pub right_location: Rc<Location>,
    pub value_location: Rc<Location>,
    pub is_deref: bool,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct IfStatement {
    pub condition: Box<AstNode>,
    pub body: Vec<AstNode>,
    pub else_body: Vec<AstNode>,
    pub location: Rc<Location>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct WhileLoopStatement {
    pub condition: Box<AstNode>,
    pub step: Option<Box<AstNode>>,
    pub body: Vec<AstNode>,
    pub location: Rc<Location>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VariadicStart {
    pub name: String,
    pub location: Rc<Location>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VariadicArgument {
    pub name: String,
    pub r#type: Option<Type>,
    pub location: Rc<Location>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Environment {
    pub value: Option<Box<AstNode>>,
    pub location: Rc<Location>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SetAllocator {
    pub value: Box<AstNode>,
    pub location: Rc<Location>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BlockStatement {
    pub body: Vec<AstNode>,
    pub location: Rc<Location>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum AstNode {
    /// Holds identifiers, literals, inline IR
    Literal(Literal),
    /// A declaration of name `name` with type `r#type` to value `value
    Declare(Declare),
    /// Allocates stack memory of size `valist`, assigns it to `name`, and calls `vastart` on it
    VariadicStart(VariadicStart),
    /// Yields a new argument of type `r#type` from `name`
    VariadicArgument(VariadicArgument),
    /// Returns value `value`
    Return(Return),
    /// Calls function `name` with parameters `parameters`
    FunctionCall(FunctionCall),
    /// Performs an arithmetic operation with `operator` using `left` and `right
    BinaryOperation(BinaryOperation),
    /// Runs `body` if condition `condition` is true, otherwise runs `else_body`
    IfStatement(IfStatement),
    /// Runs `body` while condition `condition` is true, using step `step`
    /// (`step` is used for easy merging between while loops and for loops)
    WhileLoopStatement(WhileLoopStatement),
    /// Declares a buffer named `name` with an inner type `r#type` and size `size`
    Buffer(Buffer),
    /// Declares an array literal of size `values.len()` and values `values` and returns a pointer to the start of it
    ArrayLiteral {
        explicit_inner: Option<Type>,
        known_generics: Vec<Type>,
        values: Vec<(Rc<Location>, AstNode)>,
        location: Rc<Location>,
        dynamic: bool,
    },
    /// Declares a struct named `name` with values `values`
    StructLiteral {
        name: String,
        values: Vec<(String, Box<AstNode>)>,
        location: Rc<Location>,
    },
    /// Accesses the fields of a struct, optionally assigning a value to the result
    FieldAccess {
        left: Box<AstNode>,
        right: Box<AstNode>,
        value: Option<Box<AstNode>>,
        location: Rc<Location>,
    },
    /// Loads or stores information from a pointer through pointer arithmetic
    /// In an expression like a[10], left is `a` and right is `10`
    MemoryOperation(MemoryOperation),
    /// Only executes code from value `value` when the current scope is about to exit
    /// This can be function return or an implicit scope exit through `break` or `continue`
    DeferStatement {
        value: Box<AstNode>,
        location: Rc<Location>,
    },
    /// A standalone block that executes code in its scope
    /// This can be useful for micro-managing memory allocation with defer
    BlockStatement(BlockStatement),
    /// Takes value `value` and negates it (compares it to 0)
    LogicalNot {
        value: Box<AstNode>,
        location: Rc<Location>,
    },
    /// Takes value `value` and flips all its bits
    BitWiseNot {
        value: Box<AstNode>,
        location: Rc<Location>,
    },
    /// Returns the address of some value `value`
    Address {
        value: Box<AstNode>,
        location: Rc<Location>,
    },
    /// Performs an explicit conversion of value `value` to type `r#type`
    Conversion {
        r#type: Option<Type>,
        value: Box<AstNode>,
        location: Rc<Location>,
        explicit: bool,
    },
    /// Returns the size (in bytes) or length, depending on if `standalone` is set to true
    /// The result is used to allow for getting the size of both expressions and types
    Size {
        value: Result<Type, Box<AstNode>>,
        location: Rc<Location>,
    },
    /// Creates a capturing closure that takes in some number of arguments
    /// and returns a single line statement result
    Lambda {
        arguments: Vec<Argument>,
        value: Vec<AstNode>,
        location: Rc<Location>,
    },
    /// Calculates the array length of an Elle-generated array
    /// Uses the formula *(array_ptr - #size(i32))
    ArrayLength {
        value: Box<AstNode>,
        location: Rc<Location>,
    },
    /// An expression which allows you to declare a value to something conditionally.
    Ternary {
        condition: Box<AstNode>,
        if_true: Box<AstNode>,
        if_false: Box<AstNode>,
        location: Rc<Location>,
    },
    /// Allows for getting and setting the global environment pointer, in static memory
    Environment(Environment),
    /// Allows to set the current allocator to something other than the default allocator
    /// automatically uses the expression's type to get the correct functions as this
    /// needs to be done through polymorphism at runtime
    SetAllocator(SetAllocator),
}

impl AstNode {
    pub fn token_to_literal(token: Token) -> AstNode {
        Self::Literal(Literal {
            kind: token.kind,
            value: token.value,
            location: token.location,
        })
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
        AstNode::Literal { .. } => {}
        AstNode::Declare(Declare { r#type, value, .. }) => {
            if let Some(ty) = r#type {
                *ty = modify_type(ty.clone(), generics, known_types, struct_pool, tree);
            }

            if let Some(value) = value {
                let new_value =
                    modify_type_in_node(*value.clone(), generics, known_types, struct_pool, tree);
                *value = Box::new(new_value);
            }
        }
        AstNode::Lambda {
            arguments, value, ..
        } => {
            for arg in arguments.iter_mut() {
                arg.r#type =
                    modify_type(arg.r#type.clone(), generics, known_types, struct_pool, tree);
            }

            *value = modify_type_in_ast(value.clone(), generics, known_types, struct_pool, tree);
        }
        AstNode::Return(Return { value, .. }) => {
            let new_value =
                modify_type_in_node(*value.clone(), generics, known_types, struct_pool, tree);
            *value = Box::new(new_value);
        }
        AstNode::VariadicArgument(VariadicArgument { r#type, .. }) => {
            if let Some(ty) = r#type {
                *ty = modify_type(ty.clone(), generics, known_types, struct_pool, tree);
            }
        }
        AstNode::VariadicStart { .. } => {}
        AstNode::ArrayLength { value, .. } => {
            let new_value =
                modify_type_in_node(*value.clone(), generics, known_types, struct_pool, tree);
            *value = Box::new(new_value);
        }
        AstNode::Buffer(Buffer { r#type, size, .. }) => {
            if let Some(ty) = r#type {
                *ty = modify_type(ty.clone(), generics, known_types, struct_pool, tree);
            }
            let new_size =
                modify_type_in_node(*size.clone(), generics, known_types, struct_pool, tree);
            *size = Box::new(new_size);
        }
        AstNode::FunctionCall(FunctionCall {
            parameters,
            generics: base_generics,
            ..
        }) => {
            for (_, param) in parameters {
                let new_param =
                    modify_type_in_node(param.clone(), generics, known_types, struct_pool, tree);
                *param = new_param;
            }

            for generic in base_generics {
                *generic = modify_type(generic.clone(), generics, known_types, struct_pool, tree);
            }
        }
        AstNode::BinaryOperation(BinaryOperation { left, right, .. }) => {
            let new_left =
                modify_type_in_node(*left.clone(), generics, known_types, struct_pool, tree);
            *left = Box::new(new_left);
            let new_right =
                modify_type_in_node(*right.clone(), generics, known_types, struct_pool, tree);
            *right = Box::new(new_right);
        }
        AstNode::Ternary {
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
        AstNode::IfStatement(IfStatement {
            condition,
            body,
            else_body,
            ..
        }) => {
            let new_condition =
                modify_type_in_node(*condition.clone(), generics, known_types, struct_pool, tree);
            *condition = Box::new(new_condition);
            *body = modify_type_in_ast(body.clone(), generics, known_types, struct_pool, tree);
            *else_body =
                modify_type_in_ast(else_body.clone(), generics, known_types, struct_pool, tree);
        }
        AstNode::WhileLoopStatement(WhileLoopStatement {
            condition,
            step,
            body,
            ..
        }) => {
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
        AstNode::ArrayLiteral { values, .. } => {
            for (_, value) in values {
                let new_value =
                    modify_type_in_node(value.clone(), generics, known_types, struct_pool, tree);
                *value = new_value;
            }
        }
        AstNode::StructLiteral { values, .. } => {
            for (_, value) in values {
                let new_value =
                    modify_type_in_node(*value.clone(), generics, known_types, struct_pool, tree);
                *value = Box::new(new_value);
            }
        }
        AstNode::FieldAccess {
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
        AstNode::MemoryOperation(MemoryOperation {
            left, right, value, ..
        }) => {
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
        AstNode::BlockStatement(BlockStatement { body, .. }) => {
            *body = modify_type_in_ast(body.clone(), generics, known_types, struct_pool, tree);
        }
        AstNode::LogicalNot { value, .. } => {
            let new_value =
                modify_type_in_node(*value.clone(), generics, known_types, struct_pool, tree);
            *value = Box::new(new_value);
        }
        AstNode::BitWiseNot { value, .. } => {
            let new_value =
                modify_type_in_node(*value.clone(), generics, known_types, struct_pool, tree);
            *value = Box::new(new_value);
        }
        AstNode::Address { value, .. } => {
            let new_value =
                modify_type_in_node(*value.clone(), generics, known_types, struct_pool, tree);
            *value = Box::new(new_value);
        }
        AstNode::Conversion { r#type, value, .. } => {
            if let Some(ty) = r#type {
                *ty = modify_type(ty.clone(), generics, known_types, struct_pool, tree);
            }
            let new_value =
                modify_type_in_node(*value.clone(), generics, known_types, struct_pool, tree);
            *value = Box::new(new_value);
        }
        AstNode::Size { value, .. } => match value {
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
        AstNode::Environment(Environment { value, .. }) => {
            if let Some(val) = value {
                let new_value =
                    modify_type_in_node(*val.clone(), generics, known_types, struct_pool, tree);
                *value = Some(Box::new(new_value));
            }
        }
        AstNode::SetAllocator(SetAllocator { value, .. }) => {
            let new_value =
                modify_type_in_node(*value.clone(), generics, known_types, struct_pool, tree);
            *value = Box::new(new_value);
        }
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
        location: Rc<Location>,
    },
    Struct {
        name: String,
        public: bool,
        usable: bool,
        imported: bool,
        generics: Vec<String>,
        known_generics: HashMap<String, Type>,
        members: Vec<Argument>,
        keyword_location: Rc<Location>,
        location: Rc<Location>,
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
        location: Rc<Location>,
        return_location: Rc<Location>,
    },
    Constant {
        name: String,
        public: bool,
        usable: bool,
        imported: bool,
        r#type: Option<Type>,
        value: Box<AstNode>,
        location: Rc<Location>,
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
    pub no_fmt: bool,
}
