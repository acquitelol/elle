use crate::{
    compiler::enums::Type,
    lexer::enums::{Token, TokenKind, ValueKind},
    parser::{constant::Constant, function::Function, r#struct::Struct},
};

use super::{enums::Primitive, r#use::Use};

pub struct Parser {
    pub tokens: Vec<Token>,
    pub position: usize,
    pub tree: Vec<Primitive>,
    pub struct_pool: Vec<String>,
    pub global_public: bool,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, struct_pool: Vec<String>) -> Self {
        Parser {
            tokens,
            position: 0,
            tree: vec![],
            struct_pool,
            global_public: false,
        }
    }

    pub fn current_token(&self) -> Token {
        self.tokens[self.position].clone()
    }

    fn next_token(&mut self) -> Option<Token> {
        match self.is_eof() {
            true => None,
            false => Some(self.tokens[self.position + 1].clone()),
        }
    }

    pub fn advance(&mut self) {
        if self.is_eof() {
            #[cfg(debug_assertions)]
            println!("The position of {:?} is the last index of the token stack. Staying at the same position.", self.position);
        } else {
            self.position += 1;
        }
    }

    pub fn advance_opt(&mut self) -> Option<()> {
        if self.is_eof() {
            None
        } else {
            self.position += 1;
            Some(())
        }
    }

    pub fn is_eof(&mut self) -> bool {
        self.position >= self.tokens.len() - 1
    }

    pub fn match_token(&mut self, expected: TokenKind, advance: bool) -> bool {
        if self.current_token().kind == expected {
            match advance {
                true => self.advance(),
                _ => {}
            };

            true
        } else {
            false
        }
    }

    pub fn expect_tokens(&self, expected: Vec<TokenKind>) {
        if !expected.contains(&self.current_token().kind) {
            panic!(
                "{}",
                self.current_token().location.error(format!(
                    "Expected one of [{}], got {:?}.",
                    expected
                        .iter()
                        .map(|kind| format!("{:?}", kind))
                        .collect::<Vec<String>>()
                        .join(", "),
                    self.current_token().kind
                )),
            )
        }
    }

    pub fn get(&mut self, expected: TokenKind) -> String {
        self.expect_tokens(vec![expected.clone()]);

        let identifier = if let Token {
            value: ValueKind::String(identifier),
            ..
        } = self.current_token()
        {
            identifier.clone()
        } else {
            panic!(
                "Expected {:?} for function name, got {:?}",
                expected.clone(),
                self.current_token()
            );
        };

        identifier
    }

    pub fn get_identifier(&mut self) -> String {
        self.get(TokenKind::Identifier)
    }

    pub fn get_type(&mut self) -> Type {
        let name = self.get(TokenKind::Identifier);

        let is_valid =
            self.struct_pool.contains(&name) || ValueKind::String(name.clone()).is_base_type();

        if !is_valid {
            panic!(
                "{}",
                self.current_token().location.error(format!(
                    "Type named {} could not be found. Are you sure you spelt it correctly?",
                    name
                ))
            )
        }

        let mut ty = ValueKind::String(name).to_type_string().unwrap();

        loop {
            let tmp = self.next_token();

            if tmp.is_some() {
                match tmp.unwrap().kind {
                    TokenKind::Multiply => {
                        ty = Type::Pointer(Box::new(ty));
                        self.advance();
                    }
                    _ => break,
                }
            }
        }

        ty
    }

    pub fn parse(
        &mut self,
        imports_only: bool,
        new_struct_pool: Option<Vec<String>>,
    ) -> (Vec<Primitive>, Vec<String>) {
        if new_struct_pool.is_some() {
            self.struct_pool = new_struct_pool.unwrap();
        }

        let mut public = false;
        let mut local = false;
        let mut external = false;

        let mut global_public = self.global_public;

        loop {
            match self.current_token().kind {
                TokenKind::Global => {
                    self.advance();

                    match self.current_token().kind {
                        TokenKind::Public => {
                            global_public = true;
                        }
                        _ => panic!(
                            "{}",
                            self.current_token().location.error(format!(
                                "Invalid global specifier '{}'",
                                self.current_token()
                                    .value
                                    .get_string_inner()
                                    .unwrap_or(self.current_token().kind.to_string())
                            ))
                        ),
                    }

                    self.advance();
                    self.expect_tokens(vec![TokenKind::Semicolon]);
                    self.advance();
                }
                TokenKind::Public if !imports_only => {
                    public = true;
                    self.advance();
                }
                TokenKind::Local if !imports_only => {
                    local = true;
                    self.advance();
                }
                TokenKind::External if !imports_only => {
                    external = true;
                    self.advance();
                }
                TokenKind::Use if imports_only => {
                    let mut r#use = Use::new(self);
                    let statement = r#use.parse();
                    self.tree.push(statement);

                    public = false;
                    local = false;
                    external = false;
                }
                TokenKind::Function if !imports_only => {
                    if local && public {
                        panic!(
                            "{}",
                            self.current_token().location.error(
                                "Cannot specify a function as both private and public".to_string()
                            )
                        );
                    }

                    let mut function = Function::new(self);
                    let statement = function.parse(
                        if local {
                            false
                        } else {
                            global_public || public
                        },
                        external,
                    );
                    self.tree.push(statement);

                    public = false;
                    local = false;
                    external = false;
                }
                TokenKind::Constant if !imports_only => {
                    if external {
                        panic!("{}", self.current_token().location.error("Cannot have an external constant. Please remove the `external` keyword.".to_string()))
                    }

                    if local && public {
                        panic!(
                            "{}",
                            self.current_token().location.error(
                                "Cannot specify a constant as both private and public".to_string()
                            )
                        );
                    }

                    let mut constant = Constant::new(self);
                    let statement = constant.parse(if local {
                        false
                    } else {
                        global_public || public
                    });
                    self.tree.push(statement);

                    public = false;
                    local = false;
                    external = false;
                }
                TokenKind::Define if !imports_only => {
                    if external {
                        panic!("{}", self.current_token().location.error("Cannot have an external struct. Please remove the `external` keyword.".to_string()))
                    }

                    if local && public {
                        panic!(
                            "{}",
                            self.current_token().location.error(
                                "Cannot specify a struct as both private and public".to_string()
                            )
                        );
                    }

                    let mut r#struct = Struct::new(self); // For now all defines are structs
                    let statement = r#struct.parse(if local {
                        false
                    } else {
                        global_public || public
                    });
                    self.tree.push(statement);

                    public = false;
                    local = false;
                    external = false;
                }
                _ => {
                    break;
                }
            }
        }

        self.global_public = global_public;
        return (self.tree.clone(), self.struct_pool.clone());
    }
}
