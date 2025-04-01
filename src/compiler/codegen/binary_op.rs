use crate::{
    compiler::{
        compiler::{Codegen, CodegenContext, Compiler},
        enums::{Comparison, Instruction, Type, Value},
        lib::{
            meta_struct::generate_meta_struct, short_circuit::handle_short_circuiting_operation,
        },
    },
    elle_error, get_BOLD, get_GREEN, get_RESET,
    lexer::enums::{TokenKind, ValueKind},
    parser::enums::{AstNode, BinaryOperation, FunctionCall, Literal, LogicalNot},
    BOLD, EQUALS_CONSTANT, GREEN, RESET,
};

impl Codegen<'_> for BinaryOperation {
    fn compile(self, gen: &mut Compiler, ctx: &CodegenContext<'_>) -> Option<(Type, Value)> {
        // Implement conditional short circuiting for logical AND and OR
        if matches!(self.operator, TokenKind::And | TokenKind::Or) {
            return Some(handle_short_circuiting_operation(
                gen,
                self.left,
                self.right,
                ctx.func,
                ctx.module,
                ctx.ty.clone(),
                ctx.is_return,
                self.location,
                self.operator,
            ));
        }

        if matches!(self.operator, TokenKind::Range | TokenKind::RangeEqual) {
            let node = AstNode::FunctionCall(FunctionCall {
                name: "Array.range".into(),
                generics: vec![],
                parameters: vec![
                    (self.location.clone(), *self.left),
                    (self.location.clone(), *self.right),
                    (
                        self.location.clone(),
                        AstNode::Literal(Literal {
                            kind: TokenKind::IntegerLiteral,
                            value: ValueKind::Number(if self.operator == TokenKind::RangeEqual {
                                1
                            } else {
                                0
                            }),
                            location: self.location.clone(),
                        }),
                    ),
                ],
                type_method: false,
                ignore_no_def: false,
                location: self.location.clone(),
            });

            let (ty, val) = node
                .compile(
                    gen,
                    &CodegenContext {
                        value: None,
                        ..ctx.clone()
                    },
                )
                .expect(&self.location.error(
                    "Unexpected error when trying to parse left side of an arithmetic operation",
                ));

            return Some((ty, val));
        }

        let (mut left_ty, left_val_unparsed) =
            self.left
                .clone()
                .compile(gen, ctx)
                .expect(&self.location.error(
                    "Unexpected error when trying to parse left side of an arithmetic operation",
                ));

        let (mut right_ty, right_val_unparsed) =
            self.right
                .clone()
                .compile(gen, ctx)
                .expect(&self.location.error(
                    "Unexpected error when trying to parse right side of an arithmetic operation",
                ));

        let mut left_val = left_val_unparsed.clone();
        let mut right_val = right_val_unparsed.clone();

        if self.operator != TokenKind::Concat {
            if left_ty.is_string() && right_ty == Type::Char {
                let char_tmp = gen.new_temporary(None, true);

                ctx.func.borrow_mut().assign_instruction(
                    &char_tmp,
                    &Type::Char,
                    Instruction::Load(Type::Char, left_val),
                );

                left_ty = Type::Char;
                left_val = char_tmp;
            }

            if right_ty.is_string() && left_ty == Type::Char {
                let char_tmp = gen.new_temporary(None, true);

                ctx.func.borrow_mut().assign_instruction(
                    &char_tmp,
                    &Type::Char,
                    Instruction::Load(Type::Char, right_val),
                );

                right_ty = Type::Char;
                right_val = char_tmp;
            }
        }

        if left_ty.weight() > right_ty.weight() {
            let (_, val) = gen.convert_to_type(
                ctx.func,
                right_ty.clone(),
                left_ty.clone(),
                right_val_unparsed,
                &self.location,
                &self.location,
                false,
            );

            right_val = val;
        } else if left_ty.weight() < right_ty.weight() {
            let (ty, val) = gen.convert_to_type(
                ctx.func,
                left_ty,
                right_ty.clone(),
                left_val_unparsed,
                &self.location,
                &self.location,
                false,
            );

            left_ty = ty;
            left_val = val;
        }

        if (!left_ty.is_primitive() || !right_ty.is_primitive())
            && [TokenKind::EqualTo, TokenKind::NotEqualTo].contains(&self.operator)
            && self.dunder_methods
        {
            let mut node = AstNode::FunctionCall(FunctionCall {
                name: EQUALS_CONSTANT.into(),
                generics: vec![],
                parameters: vec![
                    (self.location.clone(), *self.left),
                    (self.location.clone(), *self.right),
                ],
                type_method: true,
                ignore_no_def: false,
                location: self.location.clone(),
            });

            if self.operator == TokenKind::NotEqualTo {
                node = AstNode::LogicalNot(LogicalNot {
                    value: Box::new(node),
                    location: self.location.clone(),
                })
            }

            let (ty, val) = node.compile(gen, ctx).expect(
                &self
                    .location
                    .error("Unexpected error when trying to parse an equals arithmetic operation"),
            );

            return Some((ty, val));
        }

        if left_ty.is_string() && right_ty.is_string() && self.treat_as_string {
            let mut kind = None;

            match self.operator {
                // Token => (Name, HasMeta, Type),
                TokenKind::Concat => {
                    kind = Some(("concat", true, Type::Pointer(Box::new(Type::Char))))
                }
                _ => {}
            }

            if let Some((kind, has_meta, ty)) = kind {
                // TODO: extend this idea to more than just strings?
                // ideally add a .equals method on any primitive to make it equatable, and implement it for each
                // same for any struct, define a .equals method to allow it to be ran with == directly
                let func_name = format!("string.{kind}");
                let module_ref = ctx.module.borrow();

                let tmp_function_option = module_ref
                    .functions
                    .iter()
                    .find(|func| func.name == func_name);

                if tmp_function_option.is_none() {
                    elle_error!(self.location.error(format!(
                        "Cannot use the '{}' operator because the string module is not imported.\nPlease import it with {GREEN}{BOLD}use std/string;{RESET} at the top of this file.",
                        self.operator,
                        GREEN = get_GREEN!(),
                        BOLD = get_BOLD!(),
                        RESET = get_RESET!()
                    )))
                }

                let tmp_function = tmp_function_option.unwrap().clone();
                let mut params = vec![((left_ty, left_val), false), ((right_ty, right_val), false)];

                if has_meta {
                    let meta = generate_meta_struct(
                        ctx.func,
                        &params,
                        vec![
                            (self.location.clone(), *self.left),
                            (self.location.clone(), *self.right),
                        ],
                        self.location.clone(),
                    );

                    let res =
                        meta.compile(gen, ctx).expect(&self.location.error(
                            "Unexpected error when trying to compile the Elle metadata struct",
                        ));

                    params.insert(0, (res, false));
                }

                if tmp_function.variadic {
                    let node = AstNode::Literal(Literal {
                        kind: TokenKind::ExactLiteral,
                        value: ValueKind::String("...".into()),
                        location: self.location.clone(),
                    });

                    let res = node
                        .compile(gen, &ctx.to_nnf())
                        .expect(&self.location.error(
                            "Unexpected error when trying to compile the variadic literal '...'",
                        ));

                    params.insert(tmp_function.arguments.len(), (res, false));
                }

                let instr = Instruction::Call(
                    Value::Global(func_name),
                    params.into_iter().map(|x| x.0).collect(),
                );
                let op_temp = gen.new_temporary(None, true);
                ctx.func
                    .borrow_mut()
                    .assign_instruction(&op_temp, &ty, instr);

                return Some((ty, op_temp));
            }
        }

        if self.operator == TokenKind::Concat && self.treat_as_string {
            elle_error!(self.location.error(format!(
                "Cannot use the '<>' operator on non-string types {} and {}",
                left_ty.display(),
                right_ty.display()
            )))
        }

        if [
            TokenKind::BitwiseXor,
            TokenKind::BitwiseOr,
            TokenKind::BitwiseAnd,
            TokenKind::ShiftLeft,
            TokenKind::ShiftRight,
        ]
        .contains(&self.operator)
            && (left_ty.is_float() || right_ty.is_float())
        {
            elle_error!(
                self.location.error(format!(
                    "Cannot use the '{:?}' operator on non-integer type '{}'.\nYou can cast it to an integer if you need this functionality.",
                    self.operator,
                    if left_ty.is_float() {
                        left_ty.display()
                    } else {
                        right_ty.display()
                    }
                ))
            )
        }

        let instruction_ty = left_ty;
        let cloned_ty = instruction_ty.clone();

        let res = match self.operator.clone() {
            TokenKind::Add => Instruction::Add(left_val, right_val),
            TokenKind::Subtract => Instruction::Subtract(left_val, right_val),
            TokenKind::Multiply => Instruction::Multiply(left_val, right_val),
            TokenKind::Divide => Instruction::Divide(left_val, right_val),
            TokenKind::Modulus => Instruction::Modulus(left_val, right_val),
            TokenKind::GreaterThan => {
                Instruction::Compare(cloned_ty, Comparison::GreaterThan, left_val, right_val)
            }
            TokenKind::GreaterThanEqual => {
                Instruction::Compare(cloned_ty, Comparison::GreaterThanEqual, left_val, right_val)
            }
            TokenKind::LessThan => {
                Instruction::Compare(cloned_ty, Comparison::LessThan, left_val, right_val)
            }
            TokenKind::LessThanEqual => {
                Instruction::Compare(cloned_ty, Comparison::LessThanEqual, left_val, right_val)
            }
            TokenKind::EqualTo => {
                Instruction::Compare(cloned_ty, Comparison::Equal, left_val, right_val)
            }
            TokenKind::NotEqualTo => {
                Instruction::Compare(cloned_ty, Comparison::NotEqual, left_val, right_val)
            }
            TokenKind::BitwiseAnd => Instruction::BitwiseAnd(left_val, right_val),
            TokenKind::BitwiseOr => Instruction::BitwiseOr(left_val, right_val),
            TokenKind::BitwiseXor => Instruction::BitwiseXor(left_val, right_val),
            TokenKind::ShiftLeft => Instruction::ShiftLeft(left_val, right_val),
            TokenKind::ShiftRight => Instruction::ArithmeticShiftRight(left_val, right_val),
            _ => elle_error!(self
                .location
                .error(format!("Invalid operator token: {:?}", self.operator))),
        };

        let op_temp = gen.new_temporary(None, true);

        let final_ty = if self.operator.is_comparative() {
            Type::Boolean
        } else {
            instruction_ty
        };

        ctx.func
            .borrow_mut()
            .assign_instruction(&op_temp, &final_ty, res);

        Some((final_ty, op_temp))
    }
}
