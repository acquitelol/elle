use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        lib::convert::convert_to_type,
        qbe::{instruction::Instruction, r#type::Type, value::Value},
    },
    elle_error,
    lexer::enums::{TokenKind, ValueKind},
    parser::enums::{AstNode, BinaryOperation, Buffer, Literal},
};

impl Codegen<'_> for Buffer {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        let plain_name = self.name.value.get_string_inner().unwrap();
        let buf_ty = Type::Pointer(Box::new(self.r#type.clone().unwrap()));

        let node = if let Some(ref ty) = self.r#type {
            AstNode::BinaryOperation(BinaryOperation {
                left: self.size,
                right: Box::new(AstNode::Literal(Literal {
                    kind: TokenKind::LongLiteral,
                    value: ValueKind::Number(ty.size(ctx.module) as i128),
                    location: self.location.clone(),
                    tagged: false,
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
                tagged: false,
            })
        };

        let (ty, val) = node.compile(gen, &ctx.to_nnf()).unwrap_or_else(|| {
            elle_error!(self.location.borrow().error(format!(
                "Unexpected error when trying to compile size for a buffer named '{}'",
                plain_name
            )))
        });

        let tmp = gen.new_variable(&buf_ty, &plain_name, Some(ctx.func), true, false);

        let (_, converted_val) = convert_to_type(
            gen,
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

        let res = (buf_ty, tmp);

        if self.name.tagged {
            elle_error!(format!(
                "hover\n{}\n{}\nlet {plain_name}[]: {}",
                self.name.location.borrow().display_plain(false),
                self.name.location.borrow().display_plain(true),
                res.0.display()
            ));
        }

        Some(res)
    }
}
