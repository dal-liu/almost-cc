use std::collections::{BTreeSet, HashMap, HashSet};
use std::iter;

use l2::*;
use utils::{BitVector, Interner};

use crate::analysis::{LivenessResult, LoopForest};
use crate::regalloc::interference::InterferenceGraph;

type ValueId = usize;

#[derive(Debug)]
pub struct ColoringResult {
    pub color: HashMap<Value, Value>,
    pub spill_nodes: BTreeSet<Value>,
}

#[derive(Debug)]
struct ColoringAllocator<'a, 'b> {
    prev_spilled: &'b HashSet<Value>,
    num_defs_uses: Vec<Vec<u32>>,
    loop_depths: Vec<u32>,
    live_across_calls: BitVector,
    interner: Interner<Instruction>,

    precolored: Vec<ValueId>,
    simplify_worklist: BitVector,
    freeze_worklist: BitVector,
    spill_worklist: BitVector,
    spill_nodes: BTreeSet<ValueId>,
    coalesced_nodes: BitVector,
    colored_nodes: BitVector,
    select_stack: Vec<ValueId>,

    coalesced_moves: BitVector,
    constrained_moves: BitVector,
    frozen_moves: BitVector,
    worklist_moves: BitVector,
    active_moves: BitVector,

    interference: &'a mut InterferenceGraph<'a>,
    move_list: Vec<BitVector>,
    alias: Vec<ValueId>,
    color: HashMap<ValueId, ValueId>,
}

impl<'a, 'b> ColoringAllocator<'a, 'b> {
    fn new(
        func: &Function,
        liveness: &LivenessResult,
        interference: &'a mut InterferenceGraph<'a>,
        loops: &LoopForest,
        prev_spilled: &'b HashSet<Value>,
    ) -> Self {
        let interner = func
            .basic_blocks
            .iter()
            .flat_map(|block| &block.instructions)
            .fold(Interner::new(), |mut interner, &inst| {
                match inst {
                    Instruction::Assign { dst, src }
                        if dst.is_gp_variable() && src.is_gp_variable() =>
                    {
                        interner.intern(inst);
                    }
                    _ => (),
                }
                interner
            });

        let num_blocks = func.basic_blocks.len();
        let num_nodes = interference.interner.len();
        let num_moves = interner.len();

        let mut num_defs_uses = vec![vec![0; num_blocks]; num_nodes];
        let mut loop_depths = vec![0; num_blocks];
        let mut live_across_calls = BitVector::new(num_nodes);
        let mut worklist_moves = BitVector::new(num_moves);
        let mut move_list = vec![BitVector::new(num_moves); num_nodes];

        for (i, block) in func.basic_blocks.iter().enumerate() {
            loop_depths[i] = 10u32.pow(loops.loop_depth(block.id));

            for (j, inst) in block.instructions.iter().enumerate() {
                for var in inst.defs().chain(inst.uses()) {
                    num_defs_uses[interference.interner[&var]][i] += 1;
                }

                match inst {
                    Instruction::Assign { dst, src }
                        if dst.is_gp_variable() && src.is_gp_variable() =>
                    {
                        let move_ = interner[inst];
                        worklist_moves.set(move_);

                        for var in [dst, src] {
                            let node = interference.interner[var];
                            move_list[node].set(move_);
                        }
                    }

                    Instruction::Call { .. } => {
                        live_across_calls.union(&liveness.inst_out[i][j]);
                    }

                    _ => (),
                }
            }
        }

        let precolored: Vec<ValueId> = Register::gp_registers()
            .map(|reg| interference.interner[&Value::Register(reg)])
            .collect();

        let alias = (0..num_nodes).collect();

        let color = (0..num_nodes)
            .filter_map(|n| precolored.contains(&n).then_some(n))
            .map(|n| (n, n))
            .collect();

        Self {
            prev_spilled,
            num_defs_uses,
            loop_depths,
            live_across_calls,
            interner,

            precolored,
            simplify_worklist: BitVector::new(num_nodes),
            freeze_worklist: BitVector::new(num_nodes),
            spill_worklist: BitVector::new(num_nodes),
            spill_nodes: BTreeSet::new(),
            coalesced_nodes: BitVector::new(num_nodes),
            colored_nodes: BitVector::new(num_nodes),
            select_stack: Vec::new(),

            coalesced_moves: BitVector::new(num_moves),
            constrained_moves: BitVector::new(num_moves),
            frozen_moves: BitVector::new(num_moves),
            worklist_moves,
            active_moves: BitVector::new(num_moves),

            interference,
            move_list,
            alias,
            color,
        }
    }

    fn mk_worklist(&mut self) {
        let num_nodes = self.interference.interner.len();
        for node in (0..num_nodes).filter(|n| !self.precolored.contains(n)) {
            if self.interference.degree(node) >= Register::NUM_GP_REGISTERS {
                self.spill_worklist.set(node);
            } else if self.is_move_related(node) {
                self.freeze_worklist.set(node);
            } else {
                self.simplify_worklist.set(node);
            }
        }
    }

    fn allocate(&mut self) {
        while self.simplify_worklist.any()
            || self.worklist_moves.any()
            || self.freeze_worklist.any()
            || self.spill_worklist.any()
        {
            if self.simplify_worklist.any() {
                self.simplify();
            } else if self.worklist_moves.any() {
                self.coalesce();
            } else if self.freeze_worklist.any() {
                self.freeze();
            } else if self.spill_worklist.any() {
                self.select_spill();
            }
        }
    }

    fn assign_colors(mut self) -> ColoringResult {
        let mut colored_nodes = self.colored_nodes.clone();
        colored_nodes.set_from(self.precolored.iter().copied());

        while let Some(u) = self.select_stack.pop() {
            let mut ok_colors: Vec<ValueId> = if self.live_across_calls.test(u) {
                Register::CALLEE_SAVED
                    .iter()
                    .chain(Register::CALLER_SAVED.iter())
            } else {
                Register::CALLER_SAVED
                    .iter()
                    .chain(Register::CALLEE_SAVED.iter())
            }
            .map(|&reg| self.interference.interner[&Value::Register(reg)])
            .collect();

            for v in &self.interference.graph[u] {
                if colored_nodes.test(self.get_alias(v)) {
                    ok_colors.retain(|&color| color != self.color[&self.get_alias(v)]);
                }
            }

            if ok_colors.is_empty() {
                self.spill_nodes.insert(u);
            } else {
                colored_nodes.set(u);
                self.color.insert(u, ok_colors[0]);
            }
        }

        for node in &self.coalesced_nodes {
            self.color.insert(node, self.color[&self.get_alias(node)]);
        }

        let interner = self.interference.interner;

        let color = self
            .color
            .iter()
            .map(|(&u, &v)| (*interner.resolve(u), *interner.resolve(v)))
            .collect();

        let spill_nodes = self
            .spill_nodes
            .iter()
            .map(|&n| *interner.resolve(n))
            .collect();

        ColoringResult { color, spill_nodes }
    }

    fn add_edge(&mut self, u: ValueId, v: ValueId) {
        if u != v {
            self.interference.add_edge(u, v);
        }
    }

    fn adjacent(&self, node: ValueId) -> BitVector {
        let mut select = BitVector::new(self.interference.graph.len());
        select.set_from(self.select_stack.iter().copied());
        select.union(&self.coalesced_nodes);

        let mut adjacent = self.interference.graph[node].clone();
        adjacent.difference(&select);
        adjacent
    }

    fn node_moves(&self, node: ValueId) -> BitVector {
        let mut node_moves = self.active_moves.clone();
        node_moves.union(&self.worklist_moves);
        node_moves.intersection(&self.move_list[node]);
        node_moves
    }

    fn is_move_related(&self, node: ValueId) -> bool {
        self.move_list[node].any()
    }

    fn simplify(&mut self) {
        if let Some(node) = self.simplify_worklist.iter().next() {
            self.simplify_worklist.reset(node);
            self.select_stack.push(node);

            for neighbor in self.adjacent(node).iter() {
                self.decrement_degree(neighbor);
            }
        }
    }

    fn decrement_degree(&mut self, node: ValueId) {
        if self.interference.degree(node) == Register::NUM_GP_REGISTERS {
            let nodes: Vec<ValueId> = iter::once(node)
                .chain(&self.interference.graph[node])
                .collect();
            self.enable_moves(&nodes);
            self.spill_worklist.reset(node);

            if self.is_move_related(node) {
                self.freeze_worklist.set(node);
            } else {
                self.simplify_worklist.set(node);
            }
        }
    }

    fn enable_moves(&mut self, nodes: &[ValueId]) {
        for &node in nodes {
            for move_ in self.node_moves(node).iter() {
                if self.active_moves.test(move_) {
                    self.active_moves.reset(move_);
                    self.worklist_moves.set(move_);
                }
            }
        }
    }

    fn coalesce(&mut self) {
        if let Some(move_) = self.worklist_moves.iter().next() {
            let interner = self.interference.interner;

            if let Instruction::Assign { dst, src } = self.interner.resolve(move_) {
                let x = self.get_alias(interner[dst]);
                let y = self.get_alias(interner[src]);

                let (u, v) = if self.precolored.contains(&y) {
                    (y, x)
                } else {
                    (x, y)
                };

                self.worklist_moves.reset(move_);

                if u == v {
                    self.coalesced_moves.set(move_);
                    self.add_worklist(u);
                } else if self.precolored.contains(&v) || self.interference.has_edge(u, v) {
                    self.constrained_moves.set(move_);
                    self.add_worklist(u);
                    self.add_worklist(v);
                } else if (self.precolored.contains(&u) && self.can_coalesce_george(u, v))
                    || (!self.precolored.contains(&u) && self.can_coalesce_briggs(u, v))
                {
                    self.coalesced_moves.set(move_);
                    self.combine(u, v);
                    self.add_worklist(u);
                } else {
                    self.active_moves.set(move_);
                }
            }
        }
    }

    fn add_worklist(&mut self, node: ValueId) {
        if !self.precolored.contains(&node)
            && !self.is_move_related(node)
            && self.interference.degree(node) < Register::NUM_GP_REGISTERS
        {
            self.freeze_worklist.reset(node);
            self.simplify_worklist.set(node);
        }
    }

    fn can_coalesce_george(&self, u: ValueId, v: ValueId) -> bool {
        self.adjacent(v).iter().all(|n| {
            self.interference.degree(n) < Register::NUM_GP_REGISTERS
                || self.precolored.contains(&n)
                || self.interference.has_edge(u, n)
        })
    }

    fn can_coalesce_briggs(&self, u: ValueId, v: ValueId) -> bool {
        let mut nodes = BitVector::new(self.interference.interner.len());
        nodes.set_from(self.adjacent(u).into_iter());
        nodes.set_from(self.adjacent(v).into_iter());

        let mut k = 0;
        for node in &nodes {
            if self.interference.degree(node) >= Register::NUM_GP_REGISTERS {
                k += 1;
            }
        }
        return k < Register::NUM_GP_REGISTERS;
    }

    fn get_alias(&self, node: ValueId) -> ValueId {
        if self.coalesced_nodes.test(node) {
            self.get_alias(self.alias[node])
        } else {
            node
        }
    }

    fn combine(&mut self, u: ValueId, v: ValueId) {
        if self.freeze_worklist.test(v) {
            self.freeze_worklist.reset(v);
        } else {
            self.spill_worklist.reset(v);
        }

        self.coalesced_nodes.set(v);
        self.alias[v] = u;

        let move_list = self.move_list[v].clone();
        self.move_list[u].union(&move_list);

        for neighbor in self.adjacent(v).iter() {
            self.add_edge(neighbor, u);
            self.decrement_degree(neighbor);
        }

        if self.interference.degree(u) >= Register::NUM_GP_REGISTERS && self.freeze_worklist.test(u)
        {
            self.freeze_worklist.reset(u);
            self.spill_worklist.set(u);
        }
    }

    fn freeze(&mut self) {
        if let Some(node) = self.freeze_worklist.iter().next() {
            self.freeze_worklist.reset(node);
            self.simplify_worklist.set(node);
            self.freeze_moves(node);
        }
    }

    fn freeze_moves(&mut self, u: ValueId) {
        let value_interner = self.interference.interner;

        for move_ in self.node_moves(u).iter() {
            if self.active_moves.test(move_) {
                self.active_moves.reset(move_);
            } else {
                self.worklist_moves.reset(move_);
            }

            self.frozen_moves.set(move_);

            let v = match self.interner.resolve(move_) {
                Instruction::Assign { dst, src } => {
                    value_interner[if value_interner[dst] == u { src } else { dst }]
                }
                _ => unreachable!("not a move"),
            };

            if self.node_moves(v).none() && self.interference.degree(v) < Register::NUM_GP_REGISTERS
            {
                self.freeze_worklist.reset(v);
                self.simplify_worklist.set(v);
            }
        }
    }

    fn select_spill(&mut self) {
        let candidate = self
            .spill_worklist
            .iter()
            .filter(|&n| {
                !self
                    .prev_spilled
                    .contains(self.interference.interner.resolve(n))
            })
            .min_by(|&a, &b| self.spill_cost(a).total_cmp(&self.spill_cost(b)))
            .or_else(|| {
                self.spill_worklist
                    .iter()
                    .min_by(|&a, &b| self.spill_cost(a).total_cmp(&self.spill_cost(b)))
            });

        if let Some(node) = candidate {
            self.spill_worklist.reset(node);
            self.simplify_worklist.set(node);
            self.freeze_moves(node);
        }
    }

    fn spill_cost(&self, node: ValueId) -> f64 {
        self.num_defs_uses[node]
            .iter()
            .enumerate()
            .map(|(i, num)| {
                (num * self.loop_depths[i]) as f64 / self.interference.degree(node) as f64
            })
            .sum()
    }
}

pub fn color_graph<'a, 'b>(
    func: &Function,
    liveness: &LivenessResult,
    interference: &'a mut InterferenceGraph<'a>,
    loops: &'a LoopForest,
    prev_spilled: &'b HashSet<Value>,
) -> ColoringResult {
    let mut allocator = ColoringAllocator::new(func, liveness, interference, loops, prev_spilled);
    allocator.mk_worklist();
    allocator.allocate();
    allocator.assign_colors()
}
