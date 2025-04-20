/// ! EXCLUSIVELY FOR STATIC ARRAYS !
/// This essentially returns `*(array_buf - #size(i32))`
///
/// Static arrays created in elle preallocate an extra
/// integer and store the size there, then return the
/// pointer + #size(i32). When accessing the size,
/// we simply shift back and return the integer value.
///
/// THIS DOESNT WORK FOR DYNAMIC ARRAYS
/// THEY USE STRUCTS, STATIC ARRAYS ARE JUST
/// A FAT POINTER (pointer + header)
use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        qbe::{instruction::Instruction, r#type::Type, value::Value},
    },
    elle_error,
    lexer::enums::{TokenKind, ValueKind},
    parser::enums::{ArrayLength, AstNode, BinaryOperation, Literal},
};

impl Codegen<'_> for ArrayLength {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let node = AstNode::BinaryOperation(BinaryOperation {
            left: self.value,
            right: Box::new(AstNode::Literal(Literal {
                kind: TokenKind::IntegerLiteral,
                value: ValueKind::Number(Type::Word.size(ctx.module) as i128),
                location: self.location.clone(),
                tagged: false,
            })),
            operator: TokenKind::Subtract,
            treat_as_string: false,
            dunder_methods: true,
            location: self.location.clone(),
        });

        let (_, val) = node.compile(gen, ctx).unwrap_or_else(|| {
            elle_error!(self.location.borrow().error(
                "Unexpected error when trying to compile the formula for getting the array length",
            ))
        });

        let temp = gen.new_temporary(Some("array.length"), true);

        ctx.func.borrow_mut().assign_instruction(
            &temp,
            &Type::Word,
            Instruction::Load(Type::Word, val),
        );

        Some((Type::Word, temp))
    }
}
