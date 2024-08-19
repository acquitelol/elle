// Roughly references https://github.com/garritfra/qbe-rs/blob/main/src/lib.rs
// https://github.com/garritfra/qbe-rs/blob/main/LICENSE-MIT
use std::{
    cell::RefCell,
    collections::HashSet,
    fmt::{self},
    mem,
};

use crate::lexer::enums::Location;

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
    BitwiseXor(Value, Value),
    Compare(Type, Comparison, Value, Value),
    Copy(Value),
    // Location in AST for reporting inconsistent return types
    Return(Option<(Type, Value, Location)>),
    JumpNonZero(Value, String, String),
    Jump(String),
    Call(Value, Vec<(Type, Value)>),
    Cast(Value),
    VAArg(Value),
    VAStart(Value),
    // Alloc4(Type, Value),
    Alloc8(Value),
    // Alloc16(u128),
    Store(Type, Value, Value),
    Load(Type, Value),
    Literal(Value),
    Conversion(Type, Type, Value),
    Extension(Type, Value),
    Truncate(Value),
    ShiftLeft(Value, Value),
    ArithmeticShiftRight(Value, Value),
    LogicalShiftRight(Value, Value),
    Comment(String),
}

impl Instruction {
    fn is_global_used(&self, global_name: &str) -> bool {
        match self {
            Self::Add(v1, v2)
            | Self::Subtract(v1, v2)
            | Self::Multiply(v1, v2)
            | Self::Divide(v1, v2)
            | Self::Modulus(v1, v2)
            | Self::BitwiseAnd(v1, v2)
            | Self::BitwiseOr(v1, v2)
            | Self::BitwiseXor(v1, v2)
            | Self::Compare(_, _, v1, v2)
            | Self::Store(_, v1, v2)
            | Self::ShiftLeft(v1, v2)
            | Self::ArithmeticShiftRight(v1, v2)
            | Self::LogicalShiftRight(v1, v2) => {
                matches!(v1, Value::Global(name) if name == global_name)
                    || matches!(v2, Value::Global(name) if name == global_name)
            }
            Self::Load(_, v)
            | Self::Conversion(_, _, v)
            | Self::Extension(_, v)
            | Self::Truncate(v)
            | Self::Cast(v)
            | Self::VAArg(v)
            | Self::VAStart(v)
            | Self::Literal(v)
            | Self::Copy(v)
            | Self::JumpNonZero(v, _, _)
            | Self::Alloc8(v) => matches!(v, Value::Global(name) if name == global_name),
            Self::Return(val) => match val {
                Some((_, v, _)) => matches!(v, Value::Global(name) if name == global_name),
                None => false,
            },
            Self::Call(v, args) => {
                let found = matches!(v, Value::Global(name) if name == global_name);

                if found {
                    found
                } else {
                    for arg in args.iter().cloned() {
                        if matches!(arg.1, Value::Global(name) if name == global_name) {
                            return true;
                        }
                    }

                    false
                }
            }
            Self::Comment(_) | Self::Jump(_) => false,
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Add(lhs, rhs) => write!(formatter, "add {}, {}", lhs, rhs),
            Self::Subtract(lhs, rhs) => write!(formatter, "sub {}, {}", lhs, rhs),
            Self::Multiply(lhs, rhs) => write!(formatter, "mul {}, {}", lhs, rhs),
            Self::Divide(lhs, rhs) => write!(formatter, "div {}, {}", lhs, rhs),
            Self::Modulus(lhs, rhs) => write!(formatter, "rem {}, {}", lhs, rhs),
            Self::Compare(ty, comparison, lhs, rhs) => {
                assert!(
                    !matches!(ty, Type::Struct(_)),
                    "Cannot compare struct types"
                );

                write!(
                    formatter,
                    // All comparisons start with c
                    "c{}{} {}, {}",
                    if ty.is_float() || ty.is_unsigned() {
                        match comparison {
                            Comparison::LessThan => "lt",
                            Comparison::LessThanEqual => "le",
                            Comparison::GreaterThan => "gt",
                            Comparison::GreaterThanEqual => "ge",
                            Comparison::Equal => "eq",
                            Comparison::NotEqual => "ne",
                        }
                    } else {
                        match comparison {
                            Comparison::LessThan => "slt",
                            Comparison::LessThanEqual => "sle",
                            Comparison::GreaterThan => "sgt",
                            Comparison::GreaterThanEqual => "sge",
                            Comparison::Equal => "eq",
                            Comparison::NotEqual => "ne",
                        }
                    },
                    ty.clone().into_abi(),
                    lhs,
                    rhs,
                )
            }
            Self::BitwiseAnd(lhs, rhs) => write!(formatter, "and {}, {}", lhs, rhs),
            Self::BitwiseOr(lhs, rhs) => write!(formatter, "or {}, {}", lhs, rhs),
            Self::BitwiseXor(lhs, rhs) => write!(formatter, "xor {}, {}", lhs, rhs),
            Self::Copy(val) => write!(formatter, "copy {}", val),
            Self::Cast(val) => write!(formatter, "cast {}", val),
            Self::VAArg(val) => write!(formatter, "vaarg {}", val),
            Self::VAStart(val) => write!(formatter, "vastart {}", val),
            Self::Return(val) => match val {
                Some((_, val, _)) => write!(formatter, "ret {}", val),
                None => write!(formatter, "ret"),
            },
            Self::JumpNonZero(val, if_nonzero, if_zero) => {
                write!(formatter, "jnz {}, @{}, @{}", val, if_nonzero, if_zero)
            }
            Self::Jump(label) => write!(formatter, "jmp @{}", label),
            Self::Call(name, args) => {
                write!(
                    formatter,
                    "call {}({})",
                    name,
                    args.iter()
                        .map(|(ty, temp)| match ty {
                            Type::Null => format!("{}", temp),
                            _ => format!("{} {}", ty.clone().into_abi(), temp),
                        })
                        .collect::<Vec<String>>()
                        .join(", "),
                )
            }
            // Self::Alloc4(ty, val) => {
            //     write!(formatter, "alloc4 {}", val)
            // }
            Self::Alloc8(val) => {
                write!(formatter, "alloc8 {}", val)
            }
            // Self::Alloc16(size) => write!(formatter, "alloc16 {}", size),
            Self::Store(r#type, dest, value) => {
                write!(
                    formatter,
                    "store{} {}, {}",
                    if r#type.clone() != Type::Char {
                        r#type.clone().into_base()
                    } else {
                        r#type.clone()
                    },
                    value,
                    dest
                )
            }
            Self::Load(r#type, src) => {
                write!(
                    formatter,
                    "load{} {}",
                    if !r#type.is_unsigned() && r#type.is_map_to_int() {
                        format!("s{}", r#type.clone())
                    } else {
                        if r#type.is_struct() {
                            r#type.clone().into_base()
                        } else {
                            r#type.clone()
                        }
                        .to_string()
                    },
                    src
                )
            }
            Self::Literal(val) => {
                write!(formatter, "{}", val)
            }
            Self::Conversion(first, second, value) => {
                write!(
                    formatter,
                    "{}to{} {}",
                    if first.is_float() {
                        first.to_string()
                    } else {
                        format!("s{}", first.clone().into_abi())
                    },
                    if second.is_float() { "f" } else { "si" },
                    value
                )
            }
            Self::Extension(ty, value) => {
                write!(
                    formatter,
                    "ext{} {}",
                    if ty.is_float() {
                        ty.to_string()
                    } else {
                        format!("s{}", ty)
                    },
                    value
                )
            }
            Self::Truncate(value) => {
                write!(formatter, "truncd {}", value)
            }
            Self::ShiftLeft(val, amount) => {
                write!(formatter, "shl {}, {}", val, amount)
            }
            Self::LogicalShiftRight(val, amount) => {
                write!(formatter, "shr {}, {}", val, amount)
            }
            Self::ArithmeticShiftRight(val, amount) => {
                write!(formatter, "sar {}, {}", val, amount)
            }
            Self::Comment(val) => {
                write!(formatter, "# {}", val)
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Type {
    UnsignedByte,
    UnsignedHalfword,
    UnsignedWord,
    UnsignedLong,
    Byte,
    Halfword,
    Boolean,
    Word,
    Long,
    Single,
    Double,
    Char,
    Void,
    Null,
    Pointer(Box<Type>),
    Struct(String),
}

impl Type {
    pub fn display(&self) -> String {
        match self {
            Self::Byte => "byte".into(),
            Self::UnsignedByte => "unsigned byte".into(),
            Self::Char => "char".into(),
            Self::Halfword => "short".into(),
            Self::UnsignedHalfword => "unsigned short".into(),
            Self::Boolean => "boolean".into(),
            Self::Word => "integer".into(),
            Self::UnsignedWord => "unsigned integer".into(),
            Self::Long => "long".into(),
            Self::UnsignedLong => "unsigned long".into(),
            Self::Pointer(inner) => format!("{} *", inner.display()),
            Self::Single => "float".into(),
            Self::Double => "double".into(),
            Self::Void => "void".into(),
            Self::Null => "null".into(),
            Self::Struct(td) => td.into(),
        }
    }

    pub fn id(&self) -> String {
        match self {
            Self::Char => "char".into(),
            Self::Boolean => "bool".into(),
            Self::Word => "i32".into(),
            Self::Long => "i64".into(),
            Self::Pointer(inner) => format!("{}*", (*inner).clone().id()),
            Self::Single => "f32".into(),
            Self::Double => "f64".into(),
            Self::Struct(td) => td.into(),
            _ => "".into(),
        }
    }

    pub fn get_pointer_inner(self) -> Option<Type> {
        match self {
            Self::Pointer(ty) => Some(*ty),
            _ => None,
        }
    }

    pub fn get_struct_inner(&self) -> Option<String> {
        match self.clone() {
            Self::Struct(val) => Some(val),
            _ => None,
        }
    }

    pub fn into_abi(self) -> Self {
        match self {
            Self::Byte
            | Self::Char
            | Self::UnsignedByte
            | Self::Halfword
            | Self::UnsignedHalfword
            | Self::UnsignedWord => Self::Word,
            Self::UnsignedLong => Self::Long,
            other => other,
        }
    }

    pub fn into_base(self) -> Self {
        match self {
            Self::Byte
            | Self::Char
            | Self::UnsignedByte
            | Self::Halfword
            | Self::UnsignedHalfword
            | Self::UnsignedWord => Self::Word,
            Self::UnsignedLong => Self::Long,
            Self::Struct(_) => Self::Long,
            other => other,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Self::Single | Self::Double => true,
            _ => false,
        }
    }

    pub fn is_void(&self) -> bool {
        match self {
            Self::Void => true,
            _ => false,
        }
    }

    pub fn is_int(&self) -> bool {
        !self.is_float()
    }

    pub fn is_struct(&self) -> bool {
        match self {
            Self::Struct(_) => true,
            _ => false,
        }
    }

    pub fn is_pointer(&self) -> bool {
        match self {
            Self::Pointer(_) => true,
            _ => false,
        }
    }

    pub fn is_pointer_like(&self) -> bool {
        match self {
            Self::Pointer(_) | Self::Long => true,
            _ => false,
        }
    }

    pub fn is_map_to_int(&self) -> bool {
        match self {
            Self::Byte
            | Self::UnsignedByte
            | Self::Halfword
            | Self::UnsignedHalfword
            | Self::UnsignedWord
            | Self::Boolean
            | Self::Char => true,
            _ => false,
        }
    }

    pub fn is_unsigned(&self) -> bool {
        match self {
            Self::UnsignedByte
            | Self::UnsignedHalfword
            | Self::UnsignedWord
            | Self::UnsignedLong => true,
            _ => false,
        }
    }

    pub fn weight(&self) -> u8 {
        match self {
            Self::Double => 4,
            Self::Single => 3,
            Self::Long | Self::UnsignedLong | Self::Pointer(..) => 2,
            Self::Word => 1,
            other if other.is_map_to_int() => 1,
            _ => 0,
        }
    }

    /// Returns number of bytes
    pub fn size(&self, module: &RefCell<Module>) -> u64 {
        match self {
            Self::UnsignedByte | Self::Byte | Self::Char => 1,
            Self::UnsignedHalfword | Self::Halfword => 2,
            Self::UnsignedWord | Self::Word | Self::Single => 4,
            Self::Double => 8,
            // Returns 4 on 32-bit and 8 on 64-bit
            Self::UnsignedLong | Self::Long | Self::Pointer(..) => mem::size_of::<usize>() as u64,
            Self::Struct(val) => {
                let size = module
                    .borrow()
                    .types
                    .iter()
                    .find(|td| td.name == val.clone())
                    .expect(&format!("Unable to find aggregate type named '{}'", val))
                    .size(module) as u64;

                if size < mem::size_of::<usize>() as u64 {
                    mem::size_of::<usize>() as u64
                } else {
                    size
                }
            }
            _ => 0,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Byte => write!(formatter, "b"),
            Self::UnsignedByte => write!(formatter, "ub"),
            Self::Char => write!(formatter, "b"),
            Self::Halfword => write!(formatter, "h"),
            Self::UnsignedHalfword => write!(formatter, "uh"),
            Self::Boolean => write!(formatter, "w"),
            Self::Word => write!(formatter, "w"),
            Self::UnsignedWord => write!(formatter, "uw"),
            Self::Long => write!(formatter, "l"),
            Self::UnsignedLong => write!(formatter, "ul"),
            Self::Pointer(..) => write!(formatter, "l"),
            Self::Single => write!(formatter, "s"),
            Self::Double => write!(formatter, "d"),
            Self::Void => write!(formatter, "w"),
            Self::Null => write!(formatter, ""),
            Self::Struct(td) => write!(formatter, ":{}", td),
        }
    }
}

/// QBE aggregate type definition
#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Default)]
pub struct TypeDef {
    pub name: String,
    pub align: Option<u64>,
    pub items: Vec<(Type, usize)>,
    pub public: bool,
    pub usable: bool,
    pub imported: bool,
}

impl TypeDef {
    pub fn size(&self, module: &RefCell<Module>) -> usize {
        let mut size = 0;

        for (ty, _) in self.items.iter().cloned() {
            if ty.is_struct() {
                let tmp_size = module
                    .borrow()
                    .types
                    .iter()
                    .find(|td| td.name == ty.get_struct_inner().unwrap())
                    .expect(&format!(
                        "Unable to find struct named '{}'",
                        ty.get_struct_inner().unwrap(),
                    ))
                    .size(module);

                size += if tmp_size < mem::size_of::<usize>() {
                    mem::size_of::<usize>()
                } else {
                    tmp_size
                }
            } else {
                size += ty.size(module) as usize;
            }
        }

        size
    }
}

impl fmt::Display for TypeDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "type :{} = ", self.name)?;
        if let Some(align) = self.align {
            write!(f, "align {} ", align)?;
        }

        write!(
            f,
            "{{ {} }}",
            self.items
                .iter()
                .map(|(ty, count)| if *count > 1 {
                    format!(
                        "{} {}",
                        if !ty.is_struct() {
                            ty.clone().into_base()
                        } else {
                            ty.clone()
                        },
                        count
                    )
                } else {
                    format!(
                        "{}",
                        if !ty.is_struct() {
                            ty.clone().into_base()
                        } else {
                            ty.clone()
                        }
                    )
                })
                .collect::<Vec<String>>()
                .join(", "),
        )
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Value {
    Temporary(String),
    Global(String),

    /// Const(prefix, literal)
    Const(Type, i128),
    Literal(String),
}

impl Value {
    pub fn get_string_inner(&self) -> String {
        match self.clone() {
            Self::Temporary(val) => val,
            Self::Global(val) => val,
            Self::Literal(val) => val,
            _ => panic!("Invalid value type {}", self),
        }
    }

    pub fn get_number_inner(&self) -> i128 {
        match self.clone() {
            Self::Const(_, val) => val,
            _ => panic!("Invalid value type {}", self),
        }
    }
}

impl fmt::Display for Value {
    /// Value prefixes based on sigils
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Temporary(name) => write!(formatter, "%{}", name),
            Self::Global(name) => write!(formatter, "${}", name),
            Self::Const(ty, value) => {
                let prefix = if ty.clone() == Type::Double {
                    "d_"
                } else if ty.clone() == Type::Single {
                    "s_"
                } else {
                    ""
                };

                write!(formatter, "{}", format!("{}{}", prefix, value))
            }
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
    String(String),
    Const(i128),
}

impl fmt::Display for DataItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(string) => write!(f, "\"{}\"", string),
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
            .push(Statement::Assign(temp, r#type.into_abi(), instruction));
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
                .map(
                    |instr| if let Statement::Assign(val, ty, ins) = instr.clone() {
                        if matches!(ins, Instruction::Copy(_) | Instruction::Load(_, _)) {
                            Statement::Assign(val, ty.into_base(), ins)
                        } else {
                            instr.clone()
                        }
                    } else {
                        instr.clone()
                    }
                )
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
    pub variadic: bool,
    pub variadic_index: usize,
    pub manual: bool,
    pub external: bool,
    pub unaliased: Option<String>,
    pub usable: bool,
    pub imported: bool,
    pub arguments: Vec<(Type, Value)>,
    pub return_type: Option<Type>,
    pub blocks: Vec<Block>,
}

impl Function {
    pub fn new(
        linkage: Linkage,
        name: impl Into<String>,
        variadic: bool,
        variadic_index: usize,
        manual: bool,
        external: bool,
        unaliased: Option<String>,
        usable: bool,
        imported: bool,
        arguments: Vec<(Type, Value)>,
        return_type: Option<Type>,
    ) -> Self {
        Function {
            linkage,
            name: name.into(),
            variadic,
            variadic_index,
            manual,
            external,
            unaliased,
            usable,
            imported,
            arguments,
            return_type,
            blocks: vec![],
        }
    }

    pub fn add_block(&mut self, label: impl Into<String>) -> &mut Block {
        self.blocks.push(Block {
            label: label.into(),
            statements: vec![],
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

        let mut arguments_clone = self
            .arguments
            .iter()
            .map(|(r#type, temp)| format!("{} {}", r#type, temp))
            .collect::<Vec<String>>()
            .clone();

        if self.variadic {
            arguments_clone.push("...".to_string());
        }

        writeln!(
            formatter,
            " ${name}({args}) {{",
            name = self.name,
            args = arguments_clone.join(", "),
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
    pub types: Vec<TypeDef>,
    pub data: Vec<Data>,
}

impl Module {
    pub fn new() -> Module {
        Module {
            functions: vec![],
            types: vec![],
            data: vec![],
        }
    }

    pub fn add_function(&mut self, function: Function) -> &mut Function {
        self.functions.push(function);
        return self.functions.last_mut().unwrap();
    }

    pub fn add_type(&mut self, def: TypeDef) -> &mut TypeDef {
        self.types.push(def);
        self.types.last_mut().unwrap()
    }

    pub fn add_data(&mut self, data: Data) -> &mut Data {
        self.data.push(data);
        self.data.last_mut().unwrap()
    }

    pub fn remove_unused_functions(&mut self) {
        let mut passes = 5; // should be enough to remove most if not all unused functions

        while passes > 0 {
            passes -= 1;

            let mut used_functions: HashSet<String> = HashSet::new();

            for func in self.functions.iter() {
                for block in func.blocks.iter() {
                    for statement in block.statements.iter() {
                        match statement {
                            Statement::Assign(_, _, instr) | Statement::Volatile(instr) => {
                                for other in self.functions.iter() {
                                    if instr.is_global_used(&other.name) {
                                        used_functions.insert(other.name.clone());
                                    }
                                }
                            }
                        }
                    }
                }
            }

            used_functions.insert("main".to_string());

            self.functions.retain(|func| {
                if !used_functions.contains(&func.name) {
                    #[cfg(debug_assertions)]
                    println!(
                        "Eliminating function '{}' due to it not being called or referenced",
                        func.name.clone()
                    );
                    false
                } else {
                    true
                }
            });
        }
    }

    // doesn't need multiple passes because will run after functions
    pub fn remove_unused_data(&mut self) {
        let mut used_data_sections: HashSet<String> = HashSet::new();

        for func in self.functions.iter() {
            for block in func.blocks.iter() {
                for statement in block.statements.iter() {
                    match statement {
                        Statement::Assign(_, _, instr) | Statement::Volatile(instr) => {
                            for data in self.data.iter() {
                                if instr.is_global_used(&data.name) {
                                    used_data_sections.insert(data.name.clone());
                                }
                            }
                        }
                    }
                }
            }
        }

        self.data.retain(|data| {
            if !used_data_sections.contains(&data.name) {
                #[cfg(debug_assertions)]
                println!(
                    "Eliminating data section '{}' due to it not being referenced",
                    data.name.clone()
                );
                false
            } else {
                true
            }
        });
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for r#type in self.types.iter() {
            writeln!(f, "{}", r#type)?;
        }

        for data in self.data.iter() {
            writeln!(f, "{}", data)?;
        }

        for func in self.functions.iter() {
            // ensure we retain external functions until this point
            // because some data sections may rely on these functions
            // if we remove them the data sections will also be removed
            if !func.external {
                writeln!(f, "{}", func)?;
            }
        }

        Ok(())
    }
}
