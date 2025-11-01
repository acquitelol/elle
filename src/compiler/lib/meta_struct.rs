use std::cell::RefCell;

use crate::{
    advance,
    compiler::qbe::{function::Function, r#type::Type, value::Value},
    get_MAIN_ID,
    lexer::enums::{Location, MutRc, Token, TokenKind, ValueKind},
    parser::enums::{ArrayLiteral, AstNode, Literal, StructLiteral},
    MAIN_ID, META_STRUCT_NAME,
};

pub fn generate_meta_struct(
    func: &RefCell<Function>,
    params: &[((Type, Value), bool)],
    parameters: &[(MutRc<Location>, AstNode)],
    location: MutRc<Location>,
) -> AstNode {
    let node = AstNode::StructLiteral(StructLiteral {
        name: Token::from_ident(META_STRUCT_NAME),
        values: vec![
            (
                "exprs".into(),
                Box::new(AstNode::ArrayLiteral(ArrayLiteral {
                    values: params
                        .iter()
                        .enumerate()
                        .map(|(i, _)| {
                            let location = parameters.get(i).unwrap().0.clone();
                            let ctx = format!("{},", location.borrow().get_expr_lead());
                            let mut res = String::new();

                            let mut paren_nesting = 0;
                            let mut block_nesting = 0;
                            let mut curly_nesting = 0;

                            let chars = ctx
                                .as_bytes()
                                .iter()
                                .map(|x| *x as char)
                                .collect::<Vec<char>>();
                            let mut i = 0;

                            loop {
                                if i + 1 >= chars.len() {
                                    if paren_nesting > 0 || block_nesting > 0 || curly_nesting > 0 {
                                        res.pop();
                                    }

                                    break;
                                }

                                // Wrapped statement, deref, nested function call
                                if chars[i] == '(' {
                                    paren_nesting += 1;
                                }

                                // Inline array
                                if chars[i] == '[' {
                                    block_nesting += 1;
                                }

                                // Struct init
                                if chars[i] == '{' {
                                    curly_nesting += 1;
                                }

                                res.push(chars[i]);
                                advance!(i, chars);

                                if chars[i] == ',' {
                                    if paren_nesting > 0 || block_nesting > 0 || curly_nesting > 0 {
                                        res.push(chars[i]);
                                        advance!(i, chars);
                                        continue;
                                    }

                                    break;
                                }

                                if chars[i] == ')' {
                                    if paren_nesting > 0 {
                                        paren_nesting -= 1;
                                    } else {
                                        break;
                                    }
                                }

                                if chars[i] == ']' {
                                    if block_nesting > 0 {
                                        block_nesting -= 1;
                                    } else {
                                        break;
                                    }
                                }

                                if chars[i] == '}' {
                                    if curly_nesting > 0 {
                                        curly_nesting -= 1;
                                    } else {
                                        break;
                                    }
                                }
                            }

                            (
                                location.clone(),
                                AstNode::Literal(Literal {
                                    kind: TokenKind::StringLiteral,
                                    value: ValueKind::String(
                                        res.replace('\\', "\\\\").replace('\"', "\\\""),
                                    ),
                                    location,
                                    tagged: false,
                                }),
                            )
                        })
                        .collect(),
                    location: location.clone(),
                    explicit_inner: None,
                    known_generics: vec![],
                    dynamic: false,
                })),
            ),
            (
                "types".into(),
                Box::new(AstNode::ArrayLiteral(ArrayLiteral {
                    values: params
                        .iter()
                        .map(|param| {
                            let inner = param.0 .0.id();

                            (
                                location.clone(),
                                AstNode::Literal(Literal {
                                    kind: TokenKind::StringLiteral,
                                    value: ValueKind::String(inner),
                                    location: location.clone(),
                                    tagged: false,
                                }),
                            )
                        })
                        .collect(),
                    location: location.clone(),
                    explicit_inner: None,
                    known_generics: vec![],
                    dynamic: false,
                })),
            ),
            (
                "arity".into(),
                Box::new(AstNode::Literal(Literal {
                    kind: TokenKind::IntegerLiteral,
                    value: ValueKind::Number(params.len() as i128),
                    location: location.clone(),
                    tagged: false,
                })),
            ),
            (
                "caller".into(),
                Box::new(AstNode::Literal(Literal {
                    kind: TokenKind::StringLiteral,
                    value: ValueKind::String({
                        let name = func.borrow_mut().name.clone();

                        if name == get_MAIN_ID!() {
                            "main".into()
                        } else {
                            name
                        }
                    }),
                    location: location.clone(),
                    tagged: false,
                })),
            ),
            (
                "file".into(),
                Box::new(AstNode::Literal(Literal {
                    kind: TokenKind::StringLiteral,
                    value: ValueKind::String(
                        location
                            .clone()
                            .borrow()
                            .file
                            .clone()
                            .split('/')
                            .next_back()
                            .unwrap()
                            .to_string(),
                    ),
                    location: location.clone(),
                    tagged: false,
                })),
            ),
            (
                "line".into(),
                Box::new(AstNode::Literal(Literal {
                    kind: TokenKind::IntegerLiteral,
                    value: ValueKind::Number((location.clone().borrow().start.row + 1) as i128),
                    location: location.clone(),
                    tagged: false,
                })),
            ),
            (
                "column".into(),
                Box::new(AstNode::Literal(Literal {
                    kind: TokenKind::IntegerLiteral,
                    value: ValueKind::Number((location.clone().borrow().start.column + 1) as i128),
                    location: location.clone(),
                    tagged: false,
                })),
            ),
        ],
        spreads: vec![],
        location,
        allow_empty: false
    });

    node
}
