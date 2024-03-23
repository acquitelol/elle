use std::vec::Vec;

use crate::lexer::{Token, TokenKind, ValueKind};

#[derive(Debug)]
enum Expression {
    Require(Require),
    Operation(Operation),
}

#[derive(Debug)]
enum Statement {
    Function(FunctionStatement),
    Declare(DeclareStatement),
    Return(ReturnStatement),
}

#[derive(Debug)]
struct Require {
    library: String,
    module: String,
    functions: Vec<String>,
}

#[derive(Debug)]
struct Operation {
    name: String,
    exposed: bool,
    arguments: Vec<Argument>,
    return_type: String,
    body: Vec<Statement>,
}

#[derive(Debug)]
struct Argument {
    name: String,
    r#type: String,
}

#[derive(Debug)]
struct DeclareStatement {
    name: String,
    r#type: String,
    value: Vec<Token>,
}

#[derive(Debug)]
struct FunctionStatement {
    name: String,
    arguments: Vec<Vec<Token>>,
}

#[derive(Debug)]
struct ReturnStatement {
    expr: Vec<Token>,
}

pub struct Parser {
    input: Vec<Token>,
    position: usize,
}

impl Parser {
    pub fn new(input: Vec<Token>) -> Self {
        Parser { input, position: 0 }
    }

    fn current_token(&self) -> Token {
        self.input[self.position].clone()
    }

    fn advance(&mut self) {
        if self.is_eof() {
            println!("The position of {:?} is beyond the Token Stack's length ({:?}). Staying at the same position.", self.position, self.input.len() - 1);
        } else {
            self.position += 1;
        }
    }

    fn is_eof(&mut self) -> bool {
        self.position >= self.input.len() - 1
    }

    fn match_token(&mut self, expected: TokenKind, advance: bool) -> bool {
        if std::mem::discriminant(&self.current_token().kind)
            == std::mem::discriminant(&expected.clone())
        {
            match advance {
                true => self.advance(),
                _ => {}
            };

            true
        } else {
            false
        }
    }

    fn expect_token(&self, expected: TokenKind) {
        if std::mem::discriminant(&self.current_token().kind)
            != std::mem::discriminant(&expected.clone())
        {
            panic!(
                "[{}] Expected {:?}, found {:?}",
                self.current_token().location.display(),
                expected,
                self.current_token().kind
            );
        }
    }

    pub fn parse(&mut self) {
        let mut tree: Vec<Expression> = vec![];

        while self.position != self.input.len() {
            if self.match_token(TokenKind::Require, true) {
                tree.push(Expression::Require(self.parse_require()));
                continue;
            }

            if self.match_token(TokenKind::Operation, true) {
                tree.push(Expression::Operation(self.parse_operation(false)));
                continue;
            }

            if self.match_token(TokenKind::Expose, true) {
                if self.match_token(TokenKind::Operation, true) {
                    tree.push(Expression::Operation(self.parse_operation(true)));
                    continue;
                }
            }

            break;
        }

        dbg!(tree);
    }

    fn parse_operation(&mut self, exposed: bool) -> Operation {
        self.expect_token(TokenKind::Identifier);

        let name_kind = match self.current_token().kind {
            TokenKind::Identifier => self.current_token().value,
            _ => todo!(),
        };

        let name = match name_kind {
            ValueKind::String(value) => value,
            _ => todo!(),
        };

        self.advance();

        self.expect_token(TokenKind::LeftParentheis);

        self.advance();

        let mut arguments = vec![];

        if self.match_token(TokenKind::Type, false) {
            while self.current_token().kind != TokenKind::RightParenthesis {
                self.expect_token(TokenKind::Type);

                let type_kind = match self.current_token().kind {
                    TokenKind::Type => self.current_token().value,
                    _ => todo!(),
                };

                let r#type = match type_kind {
                    ValueKind::String(value) => value,
                    _ => todo!(),
                };

                self.advance();

                self.expect_token(TokenKind::Identifier);

                let name_kind = match self.current_token().kind {
                    TokenKind::Identifier => self.current_token().value,
                    _ => todo!(),
                };

                let name = match name_kind {
                    ValueKind::String(value) => value,
                    _ => todo!(),
                };

                self.advance();
                self.match_token(TokenKind::Comma, true);

                arguments.push(Argument { r#type, name })
            }
        }

        self.expect_token(TokenKind::RightParenthesis);
        self.advance();

        let mut return_type: String = "Void".to_owned();

        if self.match_token(TokenKind::Arrow, true) {
            self.expect_token(TokenKind::Type);

            let return_type_kind = match self.current_token().kind {
                TokenKind::Type => self.current_token().value,
                _ => todo!(),
            };

            return_type = match return_type_kind {
                ValueKind::String(value) => value,
                _ => todo!(),
            };

            self.advance();
        }

        self.expect_token(TokenKind::LeftCurlyBrace);

        let mut body: Vec<Statement> = vec![];

        loop {
            self.advance();

            let current = self.current_token();

            match current.kind {
                TokenKind::RightCurlyBrace => {
                    self.advance();
                    break;
                }
                TokenKind::Return => {
                    self.advance();
                    let mut expr = vec![];

                    while self.current_token().kind != TokenKind::Semicolon {
                        expr.push(self.current_token());
                        self.advance();
                    }

                    body.push(Statement::Return(ReturnStatement { expr }));
                }
                TokenKind::Declare => {
                    self.advance();

                    self.expect_token(TokenKind::Identifier);

                    let name_kind = match self.current_token().kind {
                        TokenKind::Identifier => self.current_token().value,
                        _ => todo!(),
                    };

                    let name = match name_kind {
                        ValueKind::String(value) => value,
                        _ => todo!(),
                    };

                    self.advance();

                    self.expect_token(TokenKind::Colon);
                    self.advance();

                    self.expect_token(TokenKind::Type);

                    let type_kind = match self.current_token().kind {
                        TokenKind::Type => self.current_token().value,
                        _ => todo!(),
                    };

                    let r#type = match type_kind {
                        ValueKind::String(value) => value,
                        _ => todo!(),
                    };

                    self.advance();
                    self.expect_token(TokenKind::Equal);
                    self.advance();

                    let mut value = vec![];

                    while self.current_token().kind != TokenKind::Semicolon {
                        value.push(self.current_token());
                        self.advance();
                    }

                    body.push(Statement::Declare(DeclareStatement {
                        name,
                        r#type,
                        value,
                    }));
                }
                _ => {
                    let next = self.input[self.position + 1].clone();

                    // We're calling a function!
                    if current.kind == TokenKind::Identifier
                        && next.kind == TokenKind::LeftParentheis
                    {
                        let name_kind = match current.kind {
                            TokenKind::Identifier => current.value,
                            _ => todo!(),
                        };

                        let name = match name_kind {
                            ValueKind::String(value) => value,
                            _ => todo!(),
                        };

                        self.advance(); // This is the left parenthesis
                        self.advance();
                        let mut arguments = vec![];

                        while self.current_token().kind != TokenKind::RightParenthesis
                            && self.current_token().kind != TokenKind::Semicolon
                            && !self.is_eof()
                        {
                            let mut temp_tokens = vec![];

                            temp_tokens.push(self.current_token());
                            self.advance();

                            while self.current_token().kind != TokenKind::Comma
                                && self.current_token().kind != TokenKind::RightParenthesis
                                && !self.is_eof()
                            {
                                temp_tokens.push(self.current_token());
                                self.advance();
                            }

                            arguments.push(temp_tokens);
                            self.advance();
                        }

                        self.expect_token(TokenKind::Semicolon);

                        body.push(Statement::Function(FunctionStatement { name, arguments }))
                    }
                }
            };
        }

        Operation {
            exposed,
            name,
            arguments,
            return_type,
            body,
        }
    }

    fn parse_require(&mut self) -> Require {
        self.expect_token(TokenKind::Identifier);

        let library_kind = match self.current_token().kind {
            TokenKind::Identifier => self.current_token().value,
            _ => todo!(),
        };

        let library = match library_kind {
            ValueKind::String(value) => value,
            _ => todo!(),
        };

        self.advance();

        self.expect_token(TokenKind::Colon);
        self.advance();

        self.expect_token(TokenKind::Identifier);

        let module_kind = match self.current_token().kind {
            TokenKind::Identifier => self.current_token().value,
            _ => todo!(),
        };

        let module = match module_kind {
            ValueKind::String(value) => value,
            _ => todo!(),
        };

        self.advance();

        let mut functions = vec![];

        if self.match_token(TokenKind::AtMark, true) {
            self.expect_token(TokenKind::LeftCurlyBrace);
            self.advance();

            while self.current_token().kind != TokenKind::RightCurlyBrace {
                self.expect_token(TokenKind::Identifier);

                let function_kind = match self.current_token().kind {
                    TokenKind::Identifier => self.current_token().value,
                    _ => todo!(),
                };

                let function = match function_kind {
                    ValueKind::String(value) => value,
                    _ => todo!(),
                };

                functions.push(function);

                self.advance();

                if self.current_token().kind == TokenKind::Comma {
                    self.advance();
                    continue;
                }
            }

            self.expect_token(TokenKind::RightCurlyBrace);
            self.advance();
        }

        self.expect_token(TokenKind::Semicolon);
        self.advance();

        Require {
            library,
            module,
            functions,
        }
    }
}
