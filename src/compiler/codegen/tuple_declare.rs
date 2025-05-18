use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        qbe::{r#type::Type, value::Value},
    },
    elle_error,
    lexer::enums::{Token, TokenKind, ValueKind},
    misc::constants::{TRIPLE_CONSTANT, TUPLE_CONSTANT},
    parser::enums::{AstNode, Declare, FieldAccess, FunctionCall, Literal, TupleDeclare},
};

impl Codegen<'_> for TupleDeclare {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let tuple_name = gen.new_temporary(None, false).get_string_inner();
        let dunder_name = if self.third.is_some() {
            TRIPLE_CONSTANT
        } else {
            TUPLE_CONSTANT
        };

        let node = AstNode::Declare(Declare {
            name: Token::from_ident(&tuple_name),
            r#type: Some(Type::Infer),
            value: Some(Box::new(AstNode::FunctionCall(FunctionCall {
                namespace_token: Token::from_ident(""),
                name_token: Token::from_ident(dunder_name),
                name: dunder_name.into(),
                generics: vec![],
                parameters: vec![(self.location.clone(), *self.value)],
                type_method: true,
                ignore_no_def: false,
                location: self.location.clone(),
            }))),
            location: self.location.clone(),
            value_location: self.value_location.clone(),
        });

        node.compile(gen, ctx).unwrap_or_else(|| {
            elle_error!(self
                .value_location
                .borrow()
                .error("Unexpected error when compiling the value of a tuple unwrap"))
        });

        macro_rules! compile_part {
            ($part:expr, $name:literal) => {{
                AstNode::Declare(Declare {
                    name: $part,
                    r#type: self.ty.clone(),
                    value: Some(Box::new(AstNode::FieldAccess(FieldAccess {
                        left: Box::new(AstNode::Literal(Literal {
                            kind: TokenKind::Identifier,
                            value: ValueKind::String(tuple_name.clone()),
                            location: self.location.clone(),
                            tagged: false,
                        })),
                        right: Box::new(AstNode::Literal(Literal {
                            kind: TokenKind::Identifier,
                            value: ValueKind::String($name.into()),
                            location: self.location.clone(),
                            tagged: false,
                        })),
                        value: None,
                        addr_only: false,
                        location: self.location.clone(),
                    }))),
                    location: self.location.clone(),
                    value_location: self.value_location.clone(),
                })
            }
            .compile(gen, ctx)
            .unwrap_or_else(|| {
                elle_error!(self
                    .value_location
                    .borrow()
                    .error("Unexpected error when compiling the left side of a tuple unwrap"))
            })};
        }

        if let Some(third) = self.third {
            compile_part!(third, "z");
        }

        compile_part!(self.second, "y");
        Some(compile_part!(self.first, "x"))
    }
}
