use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;
use std::iter;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
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
        use BinaryOp::*;
        let op = match self {
            Add => "+",
            Sub => "-",
            Mul => "*",
            BitAnd => "&",
            Shl => "<<",
            Shr => ">>",
            Lt => "<",
            Le => "<=",
            Eq => "=",
            Ge => ">=",
            Gt => ">",
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
        vals: Vec<PhiValue>,
    },
}

impl Instruction {
    pub fn defs(&self) -> Option<SymbolId> {
        use Instruction::*;

        match self {
            Define { var, .. } => Some(*var),

            Assign { dst, .. }
            | Binary { dst, .. }
            | Extract { dst, .. }
            | ArrayLength { dst, .. }
            | TupleLength { dst, .. }
            | CallResult { dst, .. }
            | NewArray { dst, .. }
            | NewTuple { dst, .. }
            | PhiNode { dst, .. } => Some(*dst),

            Insert { .. } | Call { .. } => None,
        }
    }

    pub fn uses(&self) -> Box<dyn Iterator<Item = SymbolId> + '_> {
        use Instruction::*;

        let var = |val: &Value| match val {
            Value::Variable(id) => Some(*id),
            _ => None,
        };

        match self {
            Define { .. } => Box::new(iter::empty()),

            Assign { src, .. } => Box::new(var(src).into_iter()),

            Binary { lhs, rhs, .. } => Box::new([lhs, rhs].into_iter().filter_map(var)),

            Extract { src, idxs, .. } => {
                Box::new(iter::once(*src).chain(idxs.iter().filter_map(var)))
            }

            Insert { idxs, src, .. } => Box::new(idxs.iter().filter_map(var).chain(var(src))),

            ArrayLength { src, dim, .. } => Box::new(iter::once(*src).chain(var(dim))),

            TupleLength { src, .. } => Box::new(iter::once(*src)),

            Call { callee, args } | CallResult { callee, args, .. } => {
                Box::new(args.iter().filter_map(var).chain(match callee {
                    Callee::Value(val) => var(val),
                    _ => None,
                }))
            }

            NewArray { dims, .. } => Box::new(dims.iter().filter_map(var)),

            NewTuple { len, .. } => Box::new(var(len).into_iter()),

            PhiNode { vals, .. } => Box::new(vals.iter().filter_map(move |val| var(&val.val))),
        }
    }

    pub fn replace_def(&mut self, new: SymbolId) {
        use Instruction::*;

        match self {
            Define { var, .. } => *var = new,

            Assign { dst, .. }
            | Binary { dst, .. }
            | Extract { dst, .. }
            | ArrayLength { dst, .. }
            | TupleLength { dst, .. }
            | CallResult { dst, .. }
            | NewArray { dst, .. }
            | NewTuple { dst, .. }
            | PhiNode { dst, .. } => {
                *dst = new;
            }

            Insert { .. } | Call { .. } => (),
        }
    }

    pub fn replace_use(&mut self, old: SymbolId, new: SymbolId) {
        use Instruction::*;

        let replace_value = |val: &mut Value| {
            if matches!(val, Value::Variable(var) if *var == old) {
                *val = Value::Variable(new);
            }
        };

        let replace_variable = |var: &mut SymbolId| {
            if *var == old {
                *var = new;
            }
        };

        match self {
            Define { .. } => (),

            Assign { src, .. } => replace_value(src),

            Binary { lhs, rhs, .. } => {
                replace_value(lhs);
                replace_value(rhs);
            }

            Extract { src, idxs, .. } => {
                replace_variable(src);
                for idx in idxs {
                    replace_value(idx);
                }
            }

            Insert { dst, idxs, src } => {
                replace_variable(dst);
                for idx in idxs {
                    replace_value(idx);
                }
                replace_value(src);
            }

            ArrayLength { src, dim, .. } => {
                replace_variable(src);
                replace_value(dim);
            }

            TupleLength { src, .. } => replace_variable(src),

            Call { callee, args } | CallResult { callee, args, .. } => {
                if let Callee::Value(val) = callee {
                    replace_value(val);
                }
                for arg in args {
                    replace_value(arg);
                }
            }

            NewArray { dims, .. } => {
                for dim in dims {
                    replace_value(dim);
                }
            }

            NewTuple { len, .. } => replace_value(len),

            PhiNode { vals, .. } => {
                for val in vals {
                    replace_value(&mut val.val);
                }
            }
        }
    }
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
                    .map(|phi_val| format!("{}", phi_val.resolved(interner)))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

#[derive(Debug)]
pub struct PhiValue {
    pub val: Value,
    pub label: SymbolId,
}

impl DisplayResolved for PhiValue {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        write!(
            f,
            "[{}, :{}]",
            self.val.resolved(interner),
            self.label.resolved(interner)
        )
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(pub usize);

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
    pub predecessors: Vec<Vec<BlockId>>,
    pub successors: Vec<Vec<BlockId>>,
}

impl ControlFlowGraph {
    pub fn new(basic_blocks: &[BasicBlock]) -> Self {
        let id_map: HashMap<SymbolId, BlockId> = basic_blocks
            .iter()
            .enumerate()
            .map(|(i, block)| (block.label, BlockId(i)))
            .collect();

        let num_blocks = basic_blocks.len();
        let mut predecessors = vec![vec![]; num_blocks];
        let mut successors = vec![vec![]; num_blocks];

        for (i, block) in basic_blocks.iter().enumerate() {
            match &block.terminator {
                Terminator::Branch(label) => {
                    let succ = id_map[label];
                    successors[i].push(succ);
                    predecessors[succ.0].push(BlockId(i));
                }
                Terminator::BranchCondition {
                    true_label,
                    false_label,
                    ..
                } => {
                    let true_succ = id_map[true_label];
                    let false_succ = id_map[false_label];
                    successors[i].extend([true_succ, false_succ]);
                    predecessors[true_succ.0].push(BlockId(i));
                    predecessors[false_succ.0].push(BlockId(i));
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
