[src/main.rs:218] &tree = [
    Function {
        name: "main",
        public: false,
        usable: true,
        imported: false,
        variadic: false,
        manual: false,
        external: false,
        builtin: false,
        volatile: false,
        unaliased: None,
        generics: [],
        arguments: [],
        return: None,
        body: [
            ReturnStatement {
                value: ArithmeticOperation {
                    left: LiteralStatement {
                        kind: IntegerLiteral,
                        value: Number(
                            1,
                        ),
                        location: ./testing/a.l:2:13,
                    },
                    right: LiteralStatement {
                        kind: IntegerLiteral,
                        value: Number(
                            1,
                        ),
                        location: ./testing/a.l:2:17,
                    },
                    operator: Subtract,
                    location: ./testing/a.l:2:17,
                },
                location: ./testing/a.l:2:18,
            },
        ],
        location: ./testing/a.l:1:8,
        return_location: ./testing/a.l:1:12,
    },
]
