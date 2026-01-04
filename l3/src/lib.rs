use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;
use std::iter;

use utils::{DisplayResolved, Interner};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Callee {
    Value(Value),
    Print,
    Allocate,
    Input,
    TupleError,
    TensorError,
}

impl Callee {
    pub fn is_libcall(&self) -> bool {
        !matches!(self, Callee::Value(_))
    }
}

impl DisplayResolved for Callee {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        match self {
            Self::Value(val) => write!(f, "{}", val.resolved(interner)),
            Self::Print => write!(f, "print"),
            Self::Allocate => write!(f, "allocate"),
            Self::Input => write!(f, "input"),
            Self::TupleError => write!(f, "tuple-error"),
            Self::TensorError => write!(f, "tensor-error"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Value {
    Number(i64),
    Label(SymbolId),
    Function(SymbolId),
    Variable(SymbolId),
}

impl DisplayResolved for Value {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        match self {
            Self::Number(num) => write!(f, "{}", num),
            Self::Label(label) => write!(f, ":{}", label.resolved(interner)),
            Self::Function(callee) => write!(f, "@{}", callee.resolved(interner)),
            Self::Variable(var) => write!(f, "%{}", var.resolved(interner)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct SymbolId(pub usize);

impl DisplayResolved for SymbolId {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        write!(f, "{}", interner.resolve(self.0))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    BitAnd,
    Shl,
    Shr,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op = match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::BitAnd => "&",
            Self::Shl => "<<",
            Self::Shr => ">>",
        };
        write!(f, "{}", op)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CompareOp {
    Lt,
    Le,
    Eq,
    Ge,
    Gt,
}

impl fmt::Display for CompareOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let cmp = match self {
            Self::Lt => "<",
            Self::Le => "<=",
            Self::Eq => "=",
            Self::Ge => ">=",
            Self::Gt => ">",
        };
        write!(f, "{}", cmp)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Instruction {
    Assign {
        dst: SymbolId,
        src: Value,
    },
    Binary {
        dst: SymbolId,
        lhs: Value,
        op: BinaryOp,
        rhs: Value,
    },
    Compare {
        dst: SymbolId,
        lhs: Value,
        cmp: CompareOp,
        rhs: Value,
    },
    Load {
        dst: SymbolId,
        src: SymbolId,
    },
    Store {
        dst: SymbolId,
        src: Value,
    },
    Return,
    ReturnValue(Value),
    Label(SymbolId),
    Branch(SymbolId),
    BranchCondition {
        cond: Value,
        label: SymbolId,
    },
    Call {
        callee: Callee,
        args: Vec<Value>,
    },
    CallResult {
        dst: SymbolId,
        callee: Callee,
        args: Vec<Value>,
    },
}

impl Instruction {
    pub fn defs(&self) -> Option<SymbolId> {
        use Instruction::*;

        match self {
            Assign { dst, .. }
            | Binary { dst, .. }
            | Compare { dst, .. }
            | Load { dst, .. }
            | CallResult { dst, .. } => Some(*dst),

            Store { .. }
            | Return
            | ReturnValue(_)
            | Label(_)
            | Branch(_)
            | BranchCondition { .. }
            | Call { .. } => None,
        }
    }

    pub fn uses(&self) -> Vec<SymbolId> {
        use Instruction::*;

        match self {
            Assign { src, .. } => {
                if let Value::Variable(id) = src {
                    vec![*id]
                } else {
                    vec![]
                }
            }

            Binary { lhs, rhs, .. } | Compare { lhs, rhs, .. } => [lhs, rhs]
                .into_iter()
                .filter_map(|&val| {
                    if let Value::Variable(id) = val {
                        Some(id)
                    } else {
                        None
                    }
                })
                .collect(),

            Load { src, .. } => vec![*src],

            Store { dst, src } => iter::once(*dst)
                .chain(if let Value::Variable(id) = src {
                    Some(*id)
                } else {
                    None
                })
                .collect(),

            Return | Label(_) | Branch(_) => Vec::new(),

            ReturnValue(val) => {
                if let Value::Variable(id) = val {
                    vec![*id]
                } else {
                    vec![]
                }
            }

            BranchCondition { cond, .. } => {
                if let Value::Variable(id) = cond {
                    vec![*id]
                } else {
                    vec![]
                }
            }

            Call { callee, args } | CallResult { callee, args, .. } => args
                .iter()
                .filter_map(|arg| {
                    if let Value::Variable(id) = arg {
                        Some(*id)
                    } else {
                        None
                    }
                })
                .chain(if let Callee::Value(Value::Variable(id)) = callee {
                    Some(*id)
                } else {
                    None
                })
                .collect(),
        }
    }
}

impl DisplayResolved for Instruction {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        use Instruction::*;
        match self {
            Assign { dst, src } => write!(
                f,
                "%{} <- {}",
                dst.resolved(interner),
                src.resolved(interner)
            ),
            Binary { dst, lhs, op, rhs } => write!(
                f,
                "%{} <- {} {} {}",
                dst.resolved(interner),
                lhs.resolved(interner),
                op,
                rhs.resolved(interner)
            ),
            Compare { dst, lhs, cmp, rhs } => write!(
                f,
                "%{} <- {} {} {}",
                dst.resolved(interner),
                lhs.resolved(interner),
                cmp,
                rhs.resolved(interner),
            ),
            Load { dst, src } => write!(
                f,
                "%{} <- load %{}",
                dst.resolved(interner),
                src.resolved(interner)
            ),
            Store { dst, src } => write!(
                f,
                "store %{} <- {}",
                dst.resolved(interner),
                src.resolved(interner),
            ),
            Return => write!(f, "return"),
            ReturnValue(val) => write!(f, "return {}", val.resolved(interner)),
            Label(label) => write!(f, ":{}", label.resolved(interner)),
            Branch(label) => write!(f, "br :{}", label.resolved(interner)),
            BranchCondition { cond, label } => write!(
                f,
                "br {} :{}",
                cond.resolved(interner),
                label.resolved(interner)
            ),
            Call { callee, args } => write!(
                f,
                "call {}({})",
                callee.resolved(interner),
                args.iter()
                    .map(|arg| format!("{}", arg.resolved(interner)))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            CallResult { dst, callee, args } => write!(
                f,
                "%{} <- call {}({})",
                dst.resolved(interner),
                callee.resolved(interner),
                args.iter()
                    .map(|arg| format!("{}", arg.resolved(interner)))
                    .collect::<Vec<String>>()
                    .join(", ")
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
            writeln!(f, "\t{}", inst.resolved(interner))?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct BlockId(pub usize);

#[derive(Debug, Clone)]
pub struct Function {
    pub name: SymbolId,
    pub params: Vec<SymbolId>,
    pub basic_blocks: Vec<BasicBlock>,
    pub cfg: ControlFlowGraph,
}

impl Function {
    pub fn new(name: SymbolId, params: Vec<SymbolId>, instructions: Vec<Instruction>) -> Self {
        let mut basic_blocks = vec![BasicBlock {
            id: BlockId(0),
            instructions: Vec::new(),
        }];

        for inst in instructions {
            let block = basic_blocks.last_mut().unwrap();

            match inst {
                Instruction::Return
                | Instruction::ReturnValue(_)
                | Instruction::Branch(_)
                | Instruction::BranchCondition { .. } => {
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

        if basic_blocks
            .last()
            .map_or(false, |block| block.instructions.is_empty())
        {
            basic_blocks.pop();
        }

        let cfg = ControlFlowGraph::new(&basic_blocks);

        Self {
            name,
            params,
            basic_blocks,
            cfg,
        }
    }
}

impl DisplayResolved for Function {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        writeln!(
            f,
            "define @{}({}) {{",
            self.name.resolved(interner),
            self.params
                .iter()
                .map(|param| format!("%{}", param.resolved(interner)))
                .collect::<Vec<String>>()
                .join(", ")
        )?;

        for block in &self.basic_blocks {
            write!(f, "{}", block.resolved(interner))?;
        }

        writeln!(f, "}}")
    }
}

#[derive(Debug, Clone)]
pub struct ControlFlowGraph {
    pub predecessors: Vec<Vec<BlockId>>,
    pub successors: Vec<Vec<BlockId>>,
}

impl ControlFlowGraph {
    pub fn new(basic_blocks: &[BasicBlock]) -> Self {
        let id_map: HashMap<SymbolId, BlockId> = basic_blocks
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
            predecessors: vec![Vec::new(); num_blocks],
            successors: vec![Vec::new(); num_blocks],
        };
        let last_index = num_blocks.saturating_sub(1);

        for (i, block) in basic_blocks.iter().enumerate() {
            match block.instructions.last() {
                Some(Instruction::BranchCondition { label, .. }) => {
                    let succ = id_map[label];
                    cfg.successors[i].push(succ);
                    cfg.predecessors[succ.0].push(block.id);

                    if i < last_index && i + 1 != succ.0 {
                        cfg.successors[i].push(BlockId(i + 1));
                        cfg.predecessors[i + 1].push(block.id);
                    }
                }

                Some(Instruction::Branch(label)) => {
                    let succ = id_map[label];
                    cfg.successors[i].push(succ);
                    cfg.predecessors[succ.0].push(block.id);
                }

                Some(Instruction::Return) | Some(Instruction::ReturnValue(_)) => (),

                Some(_) => {
                    if i < last_index {
                        cfg.successors[i].push(BlockId(i + 1));
                        cfg.predecessors[i + 1].push(block.id);
                    }
                }

                None => panic!("empty block"),
            };
        }

        cfg
    }
}

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Function>,
    pub interner: Interner<String>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for func in &self.functions {
            writeln!(f, "{}", func.resolved(&self.interner))?;
        }
        Ok(())
    }
}
