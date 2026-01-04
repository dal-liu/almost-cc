use std::collections::HashMap;
use std::fmt;

use utils::{DisplayResolved, Interner};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, PartialOrd, Ord)]
pub enum Register {
    RAX,
    RDI,
    RSI,
    RDX,
    R8,
    R9,
    RCX,
    RSP,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    RBP,
    RBX,
}

impl Register {
    pub const CALLER_SAVED: &[Self] = &[
        Self::RAX,
        Self::RDI,
        Self::RSI,
        Self::RDX,
        Self::RCX,
        Self::R8,
        Self::R9,
        Self::R10,
        Self::R11,
    ];

    pub const CALLEE_SAVED: &[Self] = &[
        Self::RBX,
        Self::RBP,
        Self::R12,
        Self::R13,
        Self::R14,
        Self::R15,
    ];

    pub const NUM_GP_REGISTERS: usize = 15;

    pub fn gp_registers() -> Vec<Self> {
        [Self::CALLER_SAVED, Self::CALLEE_SAVED].concat()
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Register::*;
        let reg = match self {
            RAX => "rax",
            RDI => "rdi",
            RSI => "rsi",
            RDX => "rdx",
            R8 => "r8",
            R9 => "r9",
            RCX => "rcx",
            RSP => "rsp",
            R10 => "r10",
            R11 => "r11",
            R12 => "r12",
            R13 => "r13",
            R14 => "r14",
            R15 => "r15",
            RBP => "rbp",
            RBX => "rbx",
        };
        write!(f, "{}", reg)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, PartialOrd, Ord)]
pub enum Value {
    Register(Register),
    Number(i64),
    Label(SymbolId),
    Function(SymbolId),
    Variable(SymbolId),
}

impl Value {
    pub fn is_gp_variable(&self) -> bool {
        match self {
            Self::Variable(_) => true,
            Self::Register(reg) if !matches!(reg, Register::RSP) => true,
            _ => false,
        }
    }
}

impl DisplayResolved for Value {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        match self {
            Self::Register(reg) => write!(f, "{}", reg),
            Self::Number(num) => write!(f, "{}", num),
            Self::Label(label) => write!(f, ":{}", label.resolved(interner)),
            Self::Function(callee) => write!(f, "@{}", callee.resolved(interner)),
            Self::Variable(var) => write!(f, "%{}", var.resolved(interner)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, PartialOrd, Ord)]
pub struct SymbolId(pub usize);

impl DisplayResolved for SymbolId {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        write!(f, "{}", interner.resolve(self.0))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum ArithmeticOp {
    AddAssign,
    SubAssign,
    MulAssign,
    BitAndAssign,
}

impl fmt::Display for ArithmeticOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let aop = match self {
            Self::AddAssign => "+=",
            Self::SubAssign => "-=",
            Self::MulAssign => "*=",
            Self::BitAndAssign => "&=",
        };
        write!(f, "{}", aop)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum ShiftOp {
    ShlAssign,
    ShrAssign,
}

impl fmt::Display for ShiftOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let sop = match self {
            Self::ShlAssign => "<<=",
            Self::ShrAssign => ">>=",
        };
        write!(f, "{}", sop)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum CompareOp {
    Lt,
    Le,
    Eq,
}

impl fmt::Display for CompareOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let cmp = match self {
            Self::Lt => "<",
            Self::Le => "<=",
            Self::Eq => "=",
        };
        write!(f, "{}", cmp)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum Instruction {
    Assign {
        dst: Value,
        src: Value,
    },
    Load {
        dst: Value,
        src: Value,
        offset: i64,
    },
    Store {
        dst: Value,
        offset: i64,
        src: Value,
    },
    StackArg {
        dst: Value,
        offset: i64,
    },
    Arithmetic {
        dst: Value,
        aop: ArithmeticOp,
        src: Value,
    },
    Shift {
        dst: Value,
        sop: ShiftOp,
        src: Value,
    },
    StoreArithmetic {
        dst: Value,
        offset: i64,
        aop: ArithmeticOp,
        src: Value,
    },
    LoadArithmetic {
        dst: Value,
        aop: ArithmeticOp,
        src: Value,
        offset: i64,
    },
    Compare {
        dst: Value,
        lhs: Value,
        cmp: CompareOp,
        rhs: Value,
    },
    CJump {
        lhs: Value,
        cmp: CompareOp,
        rhs: Value,
        label: SymbolId,
    },
    Label(SymbolId),
    Goto(SymbolId),
    Return,
    Call {
        callee: Value,
        args: i64,
    },
    Print,
    Input,
    Allocate,
    TupleError,
    TensorError(u8),
    Increment(Value),
    Decrement(Value),
    LEA {
        dst: Value,
        src: Value,
        offset: Value,
        scale: u8,
    },
}

impl Instruction {
    pub fn defs(&self) -> Vec<Value> {
        use Instruction::*;
        use Register::*;

        match self {
            Assign { dst, .. }
            | Load { dst, .. }
            | StackArg { dst, .. }
            | Arithmetic { dst, .. }
            | Shift { dst, .. }
            | LoadArithmetic { dst, .. }
            | Compare { dst, .. }
            | LEA { dst, .. } => vec![*dst],

            Store { .. } | StoreArithmetic { .. } | CJump { .. } | Label(_) | Goto(_) | Return => {
                Vec::new()
            }

            Call { .. } | Print | Input | Allocate | TupleError | TensorError(_) => {
                let caller_save = [R10, R11, R8, R9, RAX, RCX, RDI, RDX, RSI];
                caller_save.into_iter().map(Value::Register).collect()
            }

            Increment(val) | Decrement(val) => vec![*val],
        }
    }

    pub fn uses(&self) -> Vec<Value> {
        use Instruction::*;
        use Register::*;

        match self {
            Assign { src, .. } | Load { src, .. } => src
                .is_gp_variable()
                .then_some(vec![*src])
                .unwrap_or_default(),

            Store { dst, src, .. }
            | Arithmetic { dst, src, .. }
            | Shift { dst, src, .. }
            | StoreArithmetic { dst, src, .. }
            | LoadArithmetic { dst, src, .. } => [dst, src]
                .into_iter()
                .filter_map(|&val| val.is_gp_variable().then_some(val))
                .collect(),

            StackArg { .. } | Label(_) | Goto(_) | Input => Vec::new(),

            Compare { lhs, rhs, .. } | CJump { lhs, rhs, .. } => [lhs, rhs]
                .into_iter()
                .filter_map(|&val| val.is_gp_variable().then_some(val))
                .collect(),

            Return => {
                let result_and_callee_save = [RAX, R12, R13, R14, R15, RBP, RBX];
                result_and_callee_save
                    .into_iter()
                    .map(Value::Register)
                    .collect()
            }

            Call { callee, args } => {
                let args = *args;
                let mut uses = Vec::new();
                if callee.is_gp_variable() {
                    uses.push(*callee);
                }
                if args >= 1 {
                    uses.push(Value::Register(RDI));
                }
                if args >= 2 {
                    uses.push(Value::Register(RSI));
                }
                if args >= 3 {
                    uses.push(Value::Register(RDX));
                }
                if args >= 4 {
                    uses.push(Value::Register(RCX));
                }
                if args >= 5 {
                    uses.push(Value::Register(R8));
                }
                if args >= 6 {
                    uses.push(Value::Register(R9));
                }
                uses
            }

            Print => vec![Value::Register(RDI)],

            Allocate => vec![Value::Register(RDI), Value::Register(RSI)],

            TupleError => vec![
                Value::Register(RDI),
                Value::Register(RSI),
                Value::Register(RDX),
            ],

            TensorError(args) => {
                let args = *args;
                let mut uses = Vec::new();
                if args >= 1 {
                    uses.push(Value::Register(RDI));
                }
                if args >= 3 {
                    uses.extend_from_slice(&[Value::Register(RSI), Value::Register(RDX)]);
                }
                if args == 4 {
                    uses.push(Value::Register(RCX));
                }
                uses
            }

            Increment(val) | Decrement(val) => vec![*val],

            LEA { src, offset, .. } => vec![*src, *offset],
        }
    }

    pub fn replace_value(&mut self, old: &Value, new: &Value) {
        use Instruction::*;

        let replace_helper = |val: &mut Value| {
            if val == old {
                *val = *new;
            }
        };

        match self {
            Assign { dst, src }
            | Load { dst, src, .. }
            | Store { dst, src, .. }
            | Arithmetic { dst, src, .. }
            | Shift { dst, src, .. }
            | StoreArithmetic { dst, src, .. }
            | LoadArithmetic { dst, src, .. } => {
                replace_helper(dst);
                replace_helper(src);
            }

            StackArg { dst, .. } => {
                replace_helper(dst);
            }

            Compare { dst, lhs, rhs, .. } => {
                replace_helper(dst);
                replace_helper(lhs);
                replace_helper(rhs);
            }

            CJump { lhs, rhs, .. } => {
                replace_helper(lhs);
                replace_helper(rhs);
            }

            Label(_) | Goto(_) | Return | Print | Input | Allocate | TupleError
            | TensorError(_) => (),

            Call { callee, .. } => {
                replace_helper(callee);
            }

            Increment(val) | Decrement(val) => {
                replace_helper(val);
            }

            LEA {
                dst, src, offset, ..
            } => {
                replace_helper(dst);
                replace_helper(src);
                replace_helper(offset);
            }
        }
    }
}

impl DisplayResolved for Instruction {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        use Instruction::*;
        match self {
            Assign { dst, src } => {
                write!(
                    f,
                    "{} <- {}",
                    dst.resolved(interner),
                    src.resolved(interner)
                )
            }
            Load { dst, src, offset } => {
                write!(
                    f,
                    "{} <- mem {} {}",
                    dst.resolved(interner),
                    src.resolved(interner),
                    offset
                )
            }
            Store { dst, offset, src } => {
                write!(
                    f,
                    "mem {} {} <- {}",
                    dst.resolved(interner),
                    offset,
                    src.resolved(interner)
                )
            }
            StackArg { dst, offset } => {
                write!(f, "{} <- stack-arg {}", dst.resolved(interner), offset)
            }
            Arithmetic { dst, aop, src } => write!(
                f,
                "{} {} {}",
                dst.resolved(interner),
                aop,
                src.resolved(interner)
            ),
            Shift { dst, sop, src } => write!(
                f,
                "{} {} {}",
                dst.resolved(interner),
                sop,
                src.resolved(interner)
            ),
            StoreArithmetic {
                dst,
                offset,
                aop,
                src,
            } => write!(
                f,
                "mem {} {} {} {}",
                dst.resolved(interner),
                offset,
                aop,
                src.resolved(interner)
            ),
            LoadArithmetic {
                dst,
                aop,
                src,
                offset,
            } => write!(
                f,
                "{} {} mem {} {}",
                dst.resolved(interner),
                aop,
                src.resolved(interner),
                offset
            ),
            Compare { dst, lhs, cmp, rhs } => {
                write!(
                    f,
                    "{} <- {} {} {}",
                    dst.resolved(interner),
                    lhs.resolved(interner),
                    cmp,
                    rhs.resolved(interner)
                )
            }
            CJump {
                lhs,
                cmp,
                rhs,
                label,
            } => write!(
                f,
                "cjump {} {} {} :{}",
                lhs.resolved(interner),
                cmp,
                rhs.resolved(interner),
                label.resolved(interner),
            ),
            Label(label) => write!(f, ":{}", label.resolved(interner)),
            Goto(label) => write!(f, "goto :{}", label.resolved(interner)),
            Return => write!(f, "return"),
            Call { callee, args } => write!(f, "call {} {}", callee.resolved(interner), args),
            Print => write!(f, "call print 1"),
            Input => write!(f, "call input 0"),
            Allocate => write!(f, "call allocate 2"),
            TupleError => write!(f, "call tuple-error 3"),
            TensorError(args) => write!(f, "call tensor-error {}", args),
            Increment(reg) => write!(f, "{}++", reg.resolved(interner)),
            Decrement(reg) => write!(f, "{}--", reg.resolved(interner)),
            LEA {
                dst,
                src,
                offset,
                scale,
            } => write!(
                f,
                "{} @ {} {} {}",
                dst.resolved(interner),
                src.resolved(interner),
                offset.resolved(interner),
                scale
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: BlockId,
    pub instructions: Vec<Instruction>,
}

impl DisplayResolved for BasicBlock {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        for inst in &self.instructions {
            writeln!(f, "    {}", inst.resolved(interner))?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct BlockId(pub usize);

#[derive(Debug, Clone)]
pub struct Function {
    pub name: SymbolId,
    pub args: i64,
    pub locals: i64,
    pub basic_blocks: Vec<BasicBlock>,
    pub cfg: ControlFlowGraph,
}

impl Function {
    pub fn new(name: SymbolId, args: i64, instructions: Vec<Instruction>) -> Self {
        let mut basic_blocks = vec![BasicBlock {
            id: BlockId(0),
            instructions: Vec::new(),
        }];

        for inst in instructions {
            let block = basic_blocks.last_mut().unwrap();

            match inst {
                Instruction::CJump { .. }
                | Instruction::Goto(_)
                | Instruction::Return
                | Instruction::TupleError
                | Instruction::TensorError(_) => {
                    block.instructions.push(inst);
                    basic_blocks.push(BasicBlock {
                        id: BlockId(basic_blocks.len()),
                        instructions: Vec::new(),
                    });
                }

                Instruction::Label(_) => {
                    if block.instructions.is_empty() {
                        block.instructions.push(inst);
                    } else {
                        basic_blocks.push(BasicBlock {
                            id: BlockId(basic_blocks.len()),
                            instructions: vec![inst],
                        });
                    }
                }

                _ => block.instructions.push(inst),
            }
        }

        basic_blocks.retain(|block| !block.instructions.is_empty());

        let cfg = ControlFlowGraph::new(&basic_blocks);

        Self {
            name,
            args,
            locals: 0,
            basic_blocks,
            cfg,
        }
    }
}

impl DisplayResolved for Function {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        writeln!(f, "  (@{} {}", self.name.resolved(interner), self.args)?;

        for block in &self.basic_blocks {
            write!(f, "{}", block.resolved(interner))?;
        }

        writeln!(f, "  )")
    }
}

#[derive(Debug, Clone)]
pub struct ControlFlowGraph {
    pub successors: Vec<Vec<BlockId>>,
    pub predecessors: Vec<Vec<BlockId>>,
}

impl ControlFlowGraph {
    pub fn new(basic_blocks: &[BasicBlock]) -> Self {
        let label_to_block: HashMap<SymbolId, BlockId> = basic_blocks
            .iter()
            .filter_map(|block| {
                block.instructions.first().and_then(|inst| match inst {
                    Instruction::Label(label) => Some((*label, block.id)),
                    _ => None,
                })
            })
            .collect();

        let num_blocks = basic_blocks.len();
        let mut cfg = Self {
            successors: vec![Vec::new(); num_blocks],
            predecessors: vec![Vec::new(); num_blocks],
        };
        let last_index = num_blocks.saturating_sub(1);

        for (i, block) in basic_blocks.iter().enumerate() {
            match block.instructions.last() {
                Some(Instruction::CJump { label, .. }) => {
                    let succ = label_to_block[label];
                    cfg.successors[i].push(succ);
                    cfg.predecessors[succ.0].push(block.id);

                    if i < last_index && i + 1 != succ.0 {
                        cfg.successors[i].push(BlockId(i + 1));
                        cfg.predecessors[i + 1].push(block.id);
                    }
                }

                Some(Instruction::Goto(label)) => {
                    let succ = label_to_block[label];
                    cfg.successors[i].push(succ);
                    cfg.predecessors[succ.0].push(block.id);
                }

                Some(Instruction::Return)
                | Some(Instruction::TupleError)
                | Some(Instruction::TensorError(_)) => (),

                Some(_) => {
                    if i < last_index {
                        cfg.successors[i].push(BlockId(i + 1));
                        cfg.predecessors[i + 1].push(block.id);
                    }
                }

                None => unreachable!("empty block"),
            };
        }

        cfg
    }
}

#[derive(Debug)]
pub struct Program {
    pub entry_point: String,
    pub functions: Vec<Function>,
    pub interner: Interner<String>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "(@{}", self.entry_point)?;

        for func in &self.functions {
            write!(f, "{}", func.resolved(&self.interner))?;
        }

        writeln!(f, ")")
    }
}
