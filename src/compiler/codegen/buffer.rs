use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        enums::{Instruction, Type, Value},
    },
    lexer::enums::{TokenKind, ValueKind},
    parser::enums::{AstNode, BinaryOperation, Buffer, Literal},
};

impl Codegen<'_> for Buffer {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let buf_ty = Type::Pointer(Box::new(self.r#type.clone().unwrap()));
        let node = if let Some(ref ty) = self.r#type {
            AstNode::BinaryOperation(BinaryOperation {
                left: self.size,
                right: Box::new(AstNode::Literal(Literal {
                    kind: TokenKind::LongLiteral,
                    value: ValueKind::Number(ty.size(ctx.module) as i128),
                    location: self.location.clone(),
                })),
                operator: TokenKind::Multiply,
                treat_as_string: false,
                dunder_methods: true,
                location: self.location.clone(),
            })
        } else {
            AstNode::Literal(Literal {
                kind: TokenKind::LongLiteral,
                value: ValueKind::Number(0),
                location: self.location.clone(),
            })
        };

        let (ty, val) = node
            .compile(
                gen,
                &CodegenContext {
                    value: None,
                    is_return: false,
                    ..ctx.clone()
                },
            )
            .expect(&self.location.error(format!(
                "Unexpected error when trying to compile size for a buffer named '{}'",
                self.name
            )));

        let tmp = gen.new_variable(&buf_ty, &self.name, Some(ctx.func), true, false);

        let (_, converted_val) = gen.convert_to_type(
            ctx.func,
            ty,
            Type::Long,
            val,
            &self.location,
            &self.location,
            true,
        );

        ctx.func.borrow_mut().assign_instruction(
            &tmp,
            &buf_ty,
            Instruction::Alloc8(converted_val.clone()),
        );

        gen.buf_metadata.insert(
            tmp.clone(),
            (buf_ty.get_pointer_inner().unwrap(), converted_val),
        );

        Some((Type::Pointer(Box::new(buf_ty)), tmp))
    }
}
