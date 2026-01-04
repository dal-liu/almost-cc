use std::fmt;

use l2::*;
use utils::{BitVector, DisplayResolved, Interner};

use crate::analysis::LivenessResult;

type NodeId = usize;

#[derive(Debug)]
pub struct InterferenceGraph<'a> {
    pub graph: Vec<BitVector>,
    pub interner: &'a Interner<Value>,
}

impl<'a> InterferenceGraph<'a> {
    pub fn new(func: &Function, liveness: &'a LivenessResult) -> Self {
        let num_gp_variables = liveness.interner.len();
        let mut graph = Self {
            graph: vec![BitVector::new(num_gp_variables); num_gp_variables],
            interner: &liveness.interner,
        };

        let gp_registers: Vec<NodeId> = Register::gp_registers()
            .map(|reg| liveness.interner[&Value::Register(reg)])
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
                        live.reset(liveness.interner[src]);
                    }
                    Instruction::Shift { src, .. } if matches!(src, Value::Variable(_)) => {
                        let rcx = graph.interner[&Value::Register(Register::RCX)];
                        let u = graph.interner[src];
                        for &v in &gp_registers {
                            if v != rcx {
                                graph.add_edge(u, v);
                            }
                        }
                    }
                    _ => (),
                }

                let defs: Vec<NodeId> = inst.defs().map(|def| liveness.interner[&def]).collect();

                live.set_from(defs.iter().copied());
                for &u in &defs {
                    for v in &live {
                        if u != v {
                            graph.add_edge(u, v);
                        }
                    }
                }

                live.reset_from(defs.iter().copied());
                live.set_from(inst.uses().map(|use_| liveness.interner[&use_]));
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
}

impl DisplayResolved for InterferenceGraph<'_> {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        let mut lines: Vec<String> = (0..self.graph.len())
            .into_iter()
            .map(|i| {
                let mut line: Vec<String> = self.graph[i]
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
