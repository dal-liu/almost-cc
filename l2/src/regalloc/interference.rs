use std::fmt;

use l2::*;
use utils::bitvector::BitVector;
use utils::interner::{DisplayResolved, Interner};

use crate::analysis::liveness::LivenessResult;

type NodeId = usize;

#[derive(Debug)]
pub struct InterferenceGraph {
    pub graph: Vec<BitVector>,
}

impl InterferenceGraph {
    pub fn new(func: &Function, liveness: &LivenessResult) -> Self {
        let num_gp_vars = liveness.interner.len();
        let mut graph = Self {
            graph: vec![BitVector::new(num_gp_vars); num_gp_vars],
        };

        let gp_registers: Vec<NodeId> = Register::gp_registers()
            .map(|reg| liveness.interner.get(&Value::Register(reg)))
            .collect();
        for &u in &gp_registers {
            for &v in &gp_registers {
                if u < v {
                    graph.add_edge(u, v);
                }
            }
        }

        for (i, block) in func.basic_blocks.iter().enumerate() {
            let mut live = liveness.block_out[i].clone();

            for inst in block.instructions.iter().rev() {
                match inst {
                    Instruction::Assign { src, .. } if src.is_gp_variable() => {
                        live.reset(liveness.interner.get(src));
                    }
                    Instruction::Shift { src, .. } if matches!(src, Value::Variable(_)) => {
                        let rcx = liveness.interner.get(&Value::Register(Register::RCX));
                        let u = liveness.interner.get(src);
                        for &v in &gp_registers {
                            if v != rcx {
                                graph.add_edge(u, v);
                            }
                        }
                    }
                    _ => (),
                }

                let defs: Vec<NodeId> =
                    inst.defs().map(|def| liveness.interner.get(&def)).collect();

                live.set_from(defs.iter().copied());
                for &u in &defs {
                    for v in &live {
                        if u != v {
                            graph.add_edge(u, v);
                        }
                    }
                }

                live.reset_from(defs.iter().copied());
                live.set_from(inst.uses().map(|use_| liveness.interner.get(&use_)));
            }
        }

        graph
    }

    pub fn add_edge(&mut self, u: NodeId, v: NodeId) {
        self.graph[u].set(v);
        self.graph[v].set(u);
    }

    pub fn has_edge(&self, u: NodeId, v: NodeId) -> bool {
        self.graph[u].test(v)
    }

    pub fn degree(&self, id: NodeId) -> NodeId {
        self.graph[id].count()
    }

    pub fn num_nodes(&self) -> usize {
        self.graph.len()
    }

    #[allow(dead_code)]
    pub fn display<'a>(&'a self, interner: &'a Interner<Value>) -> InterferenceGraphDisplay<'a> {
        InterferenceGraphDisplay {
            interference: self,
            interner,
        }
    }
}

#[derive(Debug)]
pub struct InterferenceGraphDisplay<'a> {
    interference: &'a InterferenceGraph,
    interner: &'a Interner<Value>,
}

impl DisplayResolved for InterferenceGraphDisplay<'_> {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        let mut lines: Vec<String> = (0..self.interference.num_nodes())
            .map(|i| {
                let mut line: Vec<String> = self.interference.graph[i]
                    .iter()
                    .map(|j| self.interner.resolve(j).resolved(interner).to_string())
                    .collect();
                line.sort();
                format!(
                    "{} {}",
                    self.interner.resolve(i).resolved(interner),
                    line.join(" ")
                )
            })
            .collect();
        lines.sort();
        writeln!(f, "{}", lines.join("\n"))
    }
}
