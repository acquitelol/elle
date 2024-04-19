// Roughly references https://github.com/garritfra/qbe-rs/blob/main/src/lib.rs
// https://github.com/garritfra/qbe-rs/blob/main/LICENSE-MIT
use std::fmt;

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Copy)]
pub enum Comparison {
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Equal,
    NotEqual,
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Instruction {
    Add(Value, Value),
    Subtract(Value, Value),
    Multiply(Value, Value),
    Divide(Value, Value),
    Modulus(Value, Value),
    BitwiseAnd(Value, Value),
    BitwiseOr(Value, Value),
    Compare(Type, Comparison, Value, Value),
    Copy(Value),
    Return(Option<Value>),
    JumpNonZero(Value, String, String),
    Jump(String),
    Call(String, Vec<(Type, Value)>),

    // Useful for when we implement arrays
    Alloc4(u32),
    Alloc8(u64),
    Alloc16(u128),
    Store(Type, Value, Value),
    Load(Type, Value),
}

impl fmt::Display for Instruction {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Add(lhs, rhs) => write!(formatter, "add {}, {}", lhs, rhs),
            Self::Subtract(lhs, rhs) => write!(formatter, "sub {}, {}", lhs, rhs),
            Self::Multiply(lhs, rhs) => write!(formatter, "mul {}, {}", lhs, rhs),
            Self::Divide(lhs, rhs) => write!(formatter, "div {}, {}", lhs, rhs),
            Self::Modulus(lhs, rhs) => write!(formatter, "rem {}, {}", lhs, rhs),
            Self::Compare(r#type, comparison, lhs, rhs) => {
                write!(
                    formatter,
                    "c{}{} {}, {}",
                    match comparison {
                        Comparison::LessThan => "slt",
                        Comparison::LessThanEqual => "sle",
                        Comparison::GreaterThan => "sgt",
                        Comparison::GreaterThanEqual => "sge",
                        Comparison::Equal => "eq",
                        Comparison::NotEqual => "ne",
                    },
                    r#type,
                    lhs,
                    rhs,
                )
            }
            Self::BitwiseAnd(lhs, rhs) => write!(formatter, "and {}, {}", lhs, rhs),
            Self::BitwiseOr(lhs, rhs) => write!(formatter, "or {}, {}", lhs, rhs),
            Self::Copy(val) => write!(formatter, "copy {}", val),
            Self::Return(val) => match val {
                Some(val) => write!(formatter, "ret {}", val),
                None => write!(formatter, "ret"),
            },
            Self::JumpNonZero(val, if_nonzero, if_zero) => {
                write!(formatter, "jnz {}, @{}, @{}", val, if_nonzero, if_zero)
            }
            Self::Jump(label) => write!(formatter, "jmp @{}", label),
            Self::Call(name, args) => {
                write!(
                    formatter,
                    "call ${}({})",
                    name,
                    args.iter()
                        .map(|(ty, temp)| match ty {
                            Type::Null => format!("{}", temp),
                            _ => format!("{} {}", ty, temp),
                        })
                        .collect::<Vec<String>>()
                        .join(", "),
                )
            }
            Self::Alloc4(size) => write!(formatter, "alloc4 {}", size),
            Self::Alloc8(size) => write!(formatter, "alloc8 {}", size),
            Self::Alloc16(size) => write!(formatter, "alloc16 {}", size),
            Self::Store(r#type, dest, value) => {
                write!(formatter, "store{} {}, {}", r#type, value, dest)
            }
            Self::Load(r#type, src) => {
                write!(formatter, "load{} {}", r#type, src)
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Type {
    Word,
    Long,
    Single,
    Double,
    Byte,
    Halfword,
    Null,
}

impl Type {
    pub fn into_abi(self) -> Self {
        match self {
            Self::Byte | Self::Halfword => Self::Word,
            other => other,
        }
    }

    pub fn into_base(self) -> Self {
        match self {
            Self::Byte | Self::Halfword => Self::Word,
            other => other,
        }
    }

    /// Returns number of bytes
    pub fn size(&self) -> u64 {
        match self {
            Self::Byte => 1,
            Self::Halfword => 2,
            Self::Word | Self::Single => 4,
            Self::Long | Self::Double => 8,
            _ => 0,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Byte => write!(formatter, "b"),
            Self::Halfword => write!(formatter, "h"),
            Self::Word => write!(formatter, "w"),
            Self::Long => write!(formatter, "l"),
            Self::Single => write!(formatter, "s"),
            Self::Double => write!(formatter, "d"),
            Self::Null => write!(formatter, ""),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Value {
    Temporary(String),
    Global(String),
    Const(i64),
    Literal(String),
}

impl fmt::Display for Value {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        // Value prefixes based on sigils
        match self {
            Self::Temporary(name) => write!(formatter, "%{}", name),
            Self::Global(name) => write!(formatter, "${}", name),
            Self::Const(value) => write!(formatter, "{}", value),
            Self::Literal(value) => write!(formatter, "{}", value),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Default)]
pub struct Data {
    pub linkage: Linkage,
    pub name: String,
    pub align: Option<u64>,
    pub items: Vec<(Type, DataItem)>,
}

impl Data {
    pub fn new(
        linkage: Linkage,
        name: String,
        align: Option<u64>,
        items: Vec<(Type, DataItem)>,
    ) -> Self {
        Self {
            linkage,
            name,
            align,
            items,
        }
    }
}

impl fmt::Display for Data {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "{}data ${} = ", self.linkage, self.name)?;

        if let Some(align) = self.align {
            write!(formatter, "align {} ", align)?;
        }
        write!(
            formatter,
            "{{ {} }}",
            self.items
                .iter()
                .map(|(ty, item)| format!("{} {}", ty, item))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum DataItem {
    Str(String),
    Const(i64),
}

impl fmt::Display for DataItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Str(string) => write!(f, "\"{}\"", string),
            Self::Const(val) => write!(f, "{}", val),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Statement {
    Assign(Value, Type, Instruction),
    Volatile(Instruction),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Assign(temp, ty, instr) => {
                assert!(matches!(temp, Value::Temporary(_)));
                write!(f, "{} ={} {}", temp, ty, instr)
            }
            Self::Volatile(instr) => write!(f, "{}", instr),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Default)]
pub struct Block {
    pub label: String,
    pub statements: Vec<Statement>,
}

impl Block {
    pub fn add_instruction(&mut self, instr: Instruction) {
        self.statements.push(Statement::Volatile(instr));
    }

    pub fn assign_instruction(&mut self, temp: Value, r#type: Type, instruction: Instruction) {
        self.statements
            .push(Statement::Assign(temp, r#type.into_base(), instruction));
    }

    /// Returns true if the block's last instruction is a jump
    pub fn jumps(&self) -> bool {
        let last = self.statements.last();

        if let Some(Statement::Volatile(instruction)) = last {
            matches!(
                instruction,
                Instruction::Return(_) | Instruction::Jump(_) | Instruction::JumpNonZero(..)
            )
        } else {
            false
        }
    }
}

impl fmt::Display for Block {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        writeln!(formatter, "@{}", self.label)?;

        write!(
            formatter,
            "{}",
            self.statements
                .iter()
                .map(|instr| format!("\t{}", instr))
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Default)]
pub struct Function {
    pub linkage: Linkage,
    pub name: String,
    pub arguments: Vec<(Type, Value)>,
    pub return_type: Option<Type>,
    pub blocks: Vec<Block>,
}

impl Function {
    pub fn new(
        linkage: Linkage,
        name: impl Into<String>,
        arguments: Vec<(Type, Value)>,
        return_type: Option<Type>,
    ) -> Self {
        Function {
            linkage,
            name: name.into(),
            arguments,
            return_type,
            blocks: Vec::new(),
        }
    }

    pub fn add_block(&mut self, label: impl Into<String>) -> &mut Block {
        self.blocks.push(Block {
            label: label.into(),
            statements: Vec::new(),
        });

        self.blocks.last_mut().unwrap()
    }

    pub fn last_block(&mut self) -> &Block {
        self.blocks
            .last()
            .expect("Function must have at least one block")
    }

    pub fn add_instruction(&mut self, instruction: Instruction) {
        self.blocks
            .last_mut()
            .expect("Couldn't find last block!")
            .add_instruction(instruction);
    }

    pub fn assign_instruction(&mut self, temp: Value, r#type: Type, instruction: Instruction) {
        self.blocks
            .last_mut()
            .expect("Couldn't find last block!")
            .assign_instruction(temp, r#type, instruction);
    }

    pub fn returns(&mut self) -> bool {
        let last = self.last_block().statements.last();

        last.map_or(false, |i| {
            matches!(i, Statement::Volatile(Instruction::Return(_)))
        })
    }
}

impl fmt::Display for Function {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "{}function", self.linkage)?;

        if let Some(r#type) = &self.return_type {
            write!(formatter, " {}", r#type)?;
        }

        writeln!(
            formatter,
            " ${name}({args}) {{",
            name = self.name,
            args = self
                .arguments
                .iter()
                .map(|(r#type, temp)| format!("{} {}", r#type, temp))
                .collect::<Vec<String>>()
                .join(", "),
        )?;

        for blk in self.blocks.iter() {
            writeln!(formatter, "{}", blk)?;
        }

        write!(formatter, "}}")
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Default)]
pub struct Linkage {
    pub exported: bool,
    pub section: Option<String>,
    pub secflags: Option<String>,
}

impl Linkage {
    pub fn private() -> Linkage {
        Linkage {
            exported: false,
            section: None,
            secflags: None,
        }
    }

    pub fn private_with_section(section: impl Into<String>) -> Linkage {
        Linkage {
            exported: false,
            section: Some(section.into()),
            secflags: None,
        }
    }

    pub fn public() -> Linkage {
        Linkage {
            exported: true,
            section: None,
            secflags: None,
        }
    }

    pub fn public_with_section(section: impl Into<String>) -> Linkage {
        Linkage {
            exported: true,
            section: Some(section.into()),
            secflags: None,
        }
    }
}

impl fmt::Display for Linkage {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        if self.exported {
            write!(formatter, "export ")?;
        }
        if let Some(section) = &self.section {
            write!(formatter, "section \"{}\"", section)?;

            if let Some(secflags) = &self.secflags {
                write!(formatter, " \"{}\"", secflags)?;
            }

            write!(formatter, " ")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Default)]
pub struct Module {
    pub functions: Vec<Function>,
    pub types: Vec<Type>,
    pub data: Vec<Data>,
}

impl Module {
    pub fn new() -> Module {
        Module {
            functions: Vec::new(),
            types: Vec::new(),
            data: Vec::new(),
        }
    }

    pub fn add_function(&mut self, function: Function) -> &mut Function {
        self.functions.push(function);
        return self.functions.last_mut().unwrap();
    }

    pub fn add_type(&mut self, def: Type) -> &mut Type {
        self.types.push(def);
        self.types.last_mut().unwrap()
    }

    pub fn add_data(&mut self, data: Data) -> &mut Data {
        self.data.push(data);
        self.data.last_mut().unwrap()
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for func in self.functions.iter() {
            writeln!(f, "{}", func)?;
        }

        for r#type in self.types.iter() {
            writeln!(f, "{}", r#type)?;
        }

        for data in self.data.iter() {
            writeln!(f, "{}", data)?;
        }

        Ok(())
    }
}
