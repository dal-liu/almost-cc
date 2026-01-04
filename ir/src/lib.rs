use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;

use utils::{DisplayResolved, Interner};

#[derive(Debug, Clone)]
pub enum Type {
    Int64,
    Array(usize),
    Tuple,
    Code,
    Void,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int64 => write!(f, "int64"),
            Self::Array(ndims) => write!(f, "int64{}", "[]".repeat(*ndims)),
            Self::Tuple => write!(f, "tuple"),
            Self::Code => write!(f, "code"),
            Self::Void => write!(f, "void"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Callee {
    Value(Value),
    Print,
    Input,
    TupleError,
    TensorError,
}

impl DisplayResolved for Callee {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        match self {
            Self::Value(val) => write!(f, "{}", val.resolved(interner)),
            Self::Print => write!(f, "print"),
            Self::Input => write!(f, "input"),
            Self::TupleError => write!(f, "tuple-error"),
            Self::TensorError => write!(f, "tensor-error"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Variable(SymbolId),
    Function(SymbolId),
    Number(i64),
}

impl DisplayResolved for Value {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        match self {
            Self::Variable(var) => write!(f, "%{}", var.resolved(interner)),
            Self::Function(callee) => write!(f, "@{}", callee.resolved(interner)),
            Self::Number(num) => write!(f, "{}", num),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(pub usize);

impl DisplayResolved for SymbolId {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        write!(f, "{}", interner.resolve(self.0))
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    BitAnd,
    Shl,
    Shr,
    Lt,
    Le,
    Eq,
    Ge,
    Gt,
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
            Self::Lt => "<",
            Self::Le => "<=",
            Self::Eq => "=",
            Self::Ge => ">=",
            Self::Gt => ">",
        };
        write!(f, "{}", op)
    }
}

#[derive(Debug)]
pub enum Instruction {
    Define {
        ty: Type,
        var: SymbolId,
    },
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
    Extract {
        dst: SymbolId,
        src: SymbolId,
        idxs: Vec<Value>,
    },
    Insert {
        dst: SymbolId,
        idxs: Vec<Value>,
        src: Value,
    },
    ArrayLength {
        dst: SymbolId,
        src: SymbolId,
        dim: Value,
    },
    TupleLength {
        dst: SymbolId,
        src: SymbolId,
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
    NewArray {
        dst: SymbolId,
        dims: Vec<Value>,
    },
    NewTuple {
        dst: SymbolId,
        len: Value,
    },
    PhiNode {
        dst: SymbolId,
        vals: Vec<(Value, SymbolId)>,
    },
}

impl DisplayResolved for Instruction {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        use Instruction::*;
        match self {
            Define { ty, var } => write!(f, "{} %{}", ty, var.resolved(interner)),
            Assign { dst, src } => {
                write!(
                    f,
                    "%{} <- {}",
                    dst.resolved(interner),
                    src.resolved(interner)
                )
            }
            Binary { dst, lhs, op, rhs } => write!(
                f,
                "%{} <- {} {} {}",
                dst.resolved(interner),
                lhs.resolved(interner),
                op,
                rhs.resolved(interner)
            ),
            Extract { dst, src, idxs } => write!(
                f,
                "%{} <- %{}{}",
                dst.resolved(interner),
                src.resolved(interner),
                idxs.iter()
                    .map(|idx| format!("[{}]", idx.resolved(interner)))
                    .collect::<Vec<String>>()
                    .join("")
            ),
            Insert { dst, idxs, src } => write!(
                f,
                "%{}{} <- {}",
                dst.resolved(interner),
                idxs.iter()
                    .map(|idx| format!("[{}]", idx.resolved(interner)))
                    .collect::<Vec<String>>()
                    .join(""),
                src.resolved(interner),
            ),
            ArrayLength { dst, src, dim } => {
                write!(
                    f,
                    "%{} <- length %{} {}",
                    dst.resolved(interner),
                    src.resolved(interner),
                    dim.resolved(interner)
                )
            }
            TupleLength { dst, src } => write!(
                f,
                "%{} <- length %{}",
                dst.resolved(interner),
                src.resolved(interner)
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
            NewArray { dst, dims } => write!(
                f,
                "%{} <- new Array({})",
                dst.resolved(interner),
                dims.iter()
                    .map(|dim| format!("{}", dim.resolved(interner)))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            NewTuple { dst, len } => write!(
                f,
                "%{} <- new Tuple({})",
                dst.resolved(interner),
                len.resolved(interner)
            ),
            PhiNode { dst, vals } => write!(
                f,
                "%{} <- phi {}",
                dst.resolved(interner),
                vals.iter()
                    .map(|(val, label)| format!(
                        "[{} :{}]",
                        val.resolved(interner),
                        label.resolved(interner)
                    ))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Terminator {
    Branch(SymbolId),
    BranchCondition {
        cond: Value,
        true_label: SymbolId,
        false_label: SymbolId,
    },
    Return,
    ReturnValue(Value),
}

impl DisplayResolved for Terminator {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        match self {
            Self::Branch(label) => write!(f, "br :{}", label.resolved(interner)),
            Self::BranchCondition {
                cond,
                true_label,
                false_label,
            } => write!(
                f,
                "br {} :{} :{}",
                cond.resolved(interner),
                true_label.resolved(interner),
                false_label.resolved(interner)
            ),
            Self::Return => write!(f, "return"),
            Self::ReturnValue(val) => write!(f, "return {}", val.resolved(interner)),
        }
    }
}

#[derive(Debug)]
pub struct BasicBlock {
    pub label: SymbolId,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
}

impl DisplayResolved for BasicBlock {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        writeln!(f, "\t:{}", self.label.resolved(interner))?;
        for inst in &self.instructions {
            writeln!(f, "\t{}", inst.resolved(interner))?;
        }
        writeln!(f, "\t{}", self.terminator.resolved(interner))
    }
}

#[derive(Debug)]
pub struct Function {
    pub ty: Type,
    pub name: SymbolId,
    pub params: Vec<Parameter>,
    pub basic_blocks: Vec<BasicBlock>,
    pub cfg: ControlFlowGraph,
}

impl DisplayResolved for Function {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        writeln!(
            f,
            "define {} @{}({}) {{",
            &self.ty,
            self.name.resolved(interner),
            self.params
                .iter()
                .map(|param| format!("{}", param.resolved(interner)))
                .collect::<Vec<String>>()
                .join(", ")
        )?;

        for block in &self.basic_blocks {
            write!(f, "{}", block.resolved(interner))?;
        }

        writeln!(f, "}}")
    }
}

#[derive(Debug)]
pub struct Parameter {
    pub ty: Type,
    pub var: SymbolId,
}

impl DisplayResolved for Parameter {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        write!(f, "{} %{}", &self.ty, self.var.resolved(interner))
    }
}

#[derive(Debug)]
pub struct ControlFlowGraph {
    pub predecessors: HashMap<SymbolId, Vec<SymbolId>>,
    pub successors: HashMap<SymbolId, Vec<SymbolId>>,
}

impl ControlFlowGraph {
    pub fn new(basic_blocks: &[BasicBlock]) -> Self {
        let mut predecessors: HashMap<SymbolId, Vec<SymbolId>> = HashMap::new();
        let mut successors: HashMap<SymbolId, Vec<SymbolId>> = HashMap::new();

        for block in basic_blocks {
            match &block.terminator {
                Terminator::Branch(label) => {
                    successors
                        .entry(block.label)
                        .or_insert(Vec::new())
                        .push(*label);
                    predecessors
                        .entry(*label)
                        .or_insert(Vec::new())
                        .push(block.label);
                }
                Terminator::BranchCondition {
                    true_label,
                    false_label,
                    ..
                } => {
                    successors
                        .entry(block.label)
                        .or_insert(Vec::new())
                        .extend([*true_label, *false_label]);
                    predecessors
                        .entry(*true_label)
                        .or_insert(Vec::new())
                        .push(block.label);
                    predecessors
                        .entry(*false_label)
                        .or_insert(Vec::new())
                        .push(block.label);
                }
                _ => (),
            };
        }

        Self {
            predecessors,
            successors,
        }
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
