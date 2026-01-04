use std::fmt;

use l3::*;
use utils::{DisplayResolved, Interner};

use crate::analysis::{DefUseChain, LivenessResult};
use crate::isel::contexts::Context;

#[derive(Debug)]
pub enum OpKind {
    Assign,
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
    Load,
    Store,
    Return,
    ReturnValue,
    Branch,
    BranchCond,
}

impl fmt::Display for OpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use OpKind::*;
        let op = match self {
            Assign => "<-",
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
            Load => "load",
            Store => "store",
            Return | ReturnValue => "ret",
            Branch | BranchCond => "br",
        };
        write!(f, "{}", op)
    }
}

#[derive(Debug)]
pub enum NodeKind {
    Op(OpKind),
    Value,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct NodeId(pub usize);

#[derive(Debug)]
pub struct SFNode {
    pub kind: NodeKind,
    pub result: Option<Value>,
    pub parent: Option<NodeId>,
    pub children: Vec<NodeId>,
}

impl DisplayResolved for SFNode {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        if let Some(res) = &self.result {
            write!(f, "{} ", res.resolved(interner))?;
        }
        if let NodeKind::Op(op) = &self.kind {
            write!(f, "{}", op)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct SelectionForest {
    arena: Vec<SFNode>,
    roots: Vec<NodeId>,
}

impl SelectionForest {
    pub fn result(&self, id: NodeId) -> Option<&Value> {
        self.arena[id.0].result.as_ref()
    }

    pub fn kind(&self, id: NodeId) -> &NodeKind {
        &self.arena[id.0].kind
    }

    pub fn child(&self, id: NodeId, idx: usize) -> NodeId {
        self.arena[id.0].children[idx]
    }

    pub fn children(&self, id: NodeId) -> impl DoubleEndedIterator<Item = NodeId> {
        self.arena[id.0].children.iter().copied()
    }

    pub fn num_children(&self, id: NodeId) -> usize {
        self.arena[id.0].children.len()
    }

    pub fn roots(&self) -> impl Iterator<Item = NodeId> {
        self.roots.iter().copied()
    }

    fn new(func: &Function, ctx: &Context) -> Self {
        use Instruction::*;

        let mut forest = Self {
            arena: Vec::new(),
            roots: Vec::new(),
        };

        for &id in &ctx.inst_ids {
            match &func.basic_blocks[ctx.block_id.0].instructions[id] {
                Assign { dst, src } => {
                    forest.alloc_tree(OpKind::Assign, [src], Some(Value::Variable(*dst)))
                }
                Binary { dst, lhs, op, rhs } => {
                    let op = match op {
                        BinaryOp::Add => OpKind::Add,
                        BinaryOp::Sub => OpKind::Sub,
                        BinaryOp::Mul => OpKind::Mul,
                        BinaryOp::BitAnd => OpKind::BitAnd,
                        BinaryOp::Shl => OpKind::Shl,
                        BinaryOp::Shr => OpKind::Shr,
                    };
                    forest.alloc_tree(op, [lhs, rhs], Some(Value::Variable(*dst)))
                }
                Compare { dst, lhs, cmp, rhs } => {
                    let cmp = match cmp {
                        CompareOp::Lt => OpKind::Lt,
                        CompareOp::Le => OpKind::Le,
                        CompareOp::Eq => OpKind::Eq,
                        CompareOp::Ge => OpKind::Ge,
                        CompareOp::Gt => OpKind::Gt,
                    };
                    forest.alloc_tree(cmp, [lhs, rhs], Some(Value::Variable(*dst)))
                }
                Load { dst, src } => forest.alloc_tree(
                    OpKind::Load,
                    [&Value::Variable(*src)],
                    Some(Value::Variable(*dst)),
                ),
                Store { dst, src } => {
                    forest.alloc_tree(OpKind::Store, [&Value::Variable(*dst), src], None)
                }
                Return => forest.alloc_tree(OpKind::Return, [], None),
                ReturnValue(val) => forest.alloc_tree(OpKind::ReturnValue, [val], None),
                Branch(label) => forest.alloc_tree(OpKind::Branch, [&Value::Label(*label)], None),
                BranchCondition { cond, label } => {
                    forest.alloc_tree(OpKind::BranchCond, [cond, &Value::Label(*label)], None)
                }
                Label(_) | Call { .. } | CallResult { .. } => {
                    unreachable!("illegal context instruction")
                }
            }
        }

        forest
    }

    fn alloc_tree<'a>(
        &mut self,
        kind: OpKind,
        children: impl IntoIterator<Item = &'a Value>,
        result: Option<Value>,
    ) {
        let children: Vec<NodeId> = children
            .into_iter()
            .copied()
            .map(|val| {
                self.alloc(SFNode {
                    kind: NodeKind::Value,
                    result: Some(val),
                    parent: None,
                    children: Vec::new(),
                })
            })
            .collect();

        let root = self.alloc(SFNode {
            kind: NodeKind::Op(kind),
            result,
            parent: None,
            children: children.clone(),
        });

        for child in children {
            self.arena[child.0].parent = Some(root);
        }

        self.roots.push(root);
    }

    fn alloc(&mut self, node: SFNode) -> NodeId {
        let id = self.arena.len();
        self.arena.push(node);
        NodeId(id)
    }

    fn merge_trees(
        &mut self,
        func: &Function,
        ctx: &Context,
        liveness: &LivenessResult,
        def_use: &DefUseChain,
    ) {
        let mut ctx_clone = ctx.clone();
        'outer: loop {
            for i in 0..self.roots.len().saturating_sub(1) {
                for j in i + 1..self.roots.len() {
                    if self.try_merge(func, &mut ctx_clone, liveness, def_use, i, j) {
                        continue 'outer;
                    }
                }
            }
            break;
        }
    }

    fn try_merge(
        &mut self,
        func: &Function,
        ctx: &mut Context,
        liveness: &LivenessResult,
        def_use: &DefUseChain,
        idx1: usize,
        idx2: usize,
    ) -> bool {
        let node1 = self.roots[idx1];
        let node2 = self.roots[idx2];

        if matches!(self.kind(node1), NodeKind::Op(OpKind::Store)) {
            return false;
        }

        let Some(Value::Variable(result)) = self.result(node1) else {
            return false;
        };

        let Some(leaf) = self.matching_leaf(node2, *result) else {
            return false;
        };

        let block = &func.basic_blocks[ctx.block_id.0];
        let inst1 = &block.instructions[ctx.inst_ids[idx1]];
        let inst2 = &block.instructions[ctx.inst_ids[idx2]];

        if !liveness.is_dead_at(ctx.block_id, ctx.inst_ids[idx2], *result)
            || !def_use.is_only_user(inst1, inst2)
        {
            return false;
        }

        for k in idx1 + 1..idx2 {
            let mid = &block.instructions[ctx.inst_ids[k]];

            match inst1 {
                Instruction::Load { .. } => {
                    if matches!(mid, Instruction::Load { .. } | Instruction::Store { .. }) {
                        return false;
                    }
                }
                _ => {
                    if mid.uses().any(|use_| inst1.defs() == Some(use_))
                        || inst1.uses().any(|use_| mid.defs() == Some(use_))
                    {
                        return false;
                    }
                }
            }
        }

        let parent = self.arena[leaf.0]
            .parent
            .expect("parent of leaf should exist");

        if let Some(id) = self.arena[parent.0]
            .children
            .iter_mut()
            .find(|&&mut child| child == leaf)
        {
            *id = node1;
        } else {
            unreachable!("leaf should exist in parent children")
        }

        self.arena[node1.0].parent = Some(parent);
        self.roots.remove(idx1);
        ctx.inst_ids.remove(idx1);

        true
    }

    fn matching_leaf(&self, root: NodeId, target: SymbolId) -> Option<NodeId> {
        let mut leaf = None;
        let mut stack = vec![root];

        while let Some(id) = stack.pop() {
            if self.num_children(id) > 0 {
                stack.extend(self.children(id).rev());
                continue;
            }

            if matches!(self.result(id), Some(&Value::Variable(var)) if var == target) {
                if leaf.is_some() {
                    return None;
                }
                leaf = Some(id);
            }
        }

        leaf
    }
}

impl DisplayResolved for SelectionForest {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        for &root in &self.roots {
            let mut stack = vec![(root, 0)];

            while let Some((id, indent)) = stack.pop() {
                let node = &self.arena[id.0];

                writeln!(f, "{}{}", "  ".repeat(indent), node.resolved(interner))?;

                if !node.children.is_empty() {
                    stack.extend(node.children.iter().rev().map(|&child| (child, indent + 1)));
                }
            }
        }

        Ok(())
    }
}

pub fn generate_forest(
    func: &Function,
    liveness: &LivenessResult,
    def_use: &DefUseChain,
    ctx: &Context,
) -> SelectionForest {
    let mut forest = SelectionForest::new(func, ctx);
    forest.merge_trees(func, ctx, liveness, def_use);
    forest
}
