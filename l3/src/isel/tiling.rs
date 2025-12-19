use std::collections::HashMap;

use l2;
use l3::*;

use crate::isel::forest::{NodeId, NodeKind, OpKind, SFNode, SelectionForest};

macro_rules! pat {
    (any) => {
        Pattern {
            children: Vec::new(),
            matches: |node, _| {
                matches!(
                    &node.kind,
                    NodeKind::Op {
                        result: Some(_),
                        ..
                    } | NodeKind::Value(_)
                )
            },
        }
    };

    (exact) => {
        Pattern {
            children: Vec::new(),
            matches: |node, opt| match &node.kind {
                NodeKind::Op {
                    result, ..
                } => result == opt,
                NodeKind::Value(val) => matches!(opt, Some(v) if v == val)
            },
        }
    };

    ($kind:ident) => {
        Pattern {
            children: Vec::new(),
            matches: |node, _| matches!(&node.kind, NodeKind::Op { kind: OpKind::$kind, result: None }),
        }
    };

    ($kind:ident($($child:expr),*)) => {
        Pattern {
            children: vec![$($child),*],
            matches: |node, _| matches!(&node.kind, NodeKind::Op { kind: OpKind::$kind, result: None }),
        }
    };

    ($kind:ident($($child:expr),*) -> any) => {
        Pattern {
            children: vec![$($child),*],
            matches: |node, _| matches!(&node.kind, NodeKind::Op { kind: OpKind::$kind, result: Some(_) }),
        }
    };

    ($kind:ident($($child:expr),*) -> exact) => {
        Pattern {
            children: vec![$($child),*],
            matches: |node, opt| {
                matches!(
                    &node.kind,
                    NodeKind::Op {
                        kind: OpKind::$kind,
                        result
                    } if result == opt
                )
            },
        }
    };
}

#[derive(Debug, Clone)]
pub struct Pattern {
    children: Vec<Pattern>,
    matches: fn(&SFNode, &Option<Value>) -> bool,
}

#[derive(Debug, Clone)]
pub struct Tile {
    pub pattern: Pattern,
    pub cost: u32,
    pub emit: fn(&SelectionForest, NodeId) -> Vec<l2::Instruction>,
}

impl Tile {
    fn new(
        pattern: Pattern,
        cost: u32,
        emit: fn(&SelectionForest, NodeId) -> Vec<l2::Instruction>,
    ) -> Self {
        Self {
            pattern,
            cost,
            emit,
        }
    }

    fn try_cover(&self, forest: &SelectionForest, id: NodeId) -> Option<Vec<NodeId>> {
        let mut uncovered = Vec::new();
        let NodeKind::Op { result: opt, .. } = forest.kind(id) else {
            unreachable!("cover nodes should be ops");
        };

        fn dfs(
            forest: &SelectionForest,
            id: NodeId,
            opt: &Option<Value>,
            pat: &Pattern,
            uncovered: &mut Vec<NodeId>,
        ) -> bool {
            let node = &forest.arena[id];

            if !(pat.matches)(node, opt) {
                return false;
            }

            if pat.children.is_empty() {
                if matches!(
                    node.kind,
                    NodeKind::Op {
                        result: Some(_),
                        ..
                    }
                ) {
                    uncovered.push(id);
                }
                return true;
            }

            if pat.children.len() != node.children.len() {
                return false;
            }

            node.children
                .iter()
                .zip(&pat.children)
                .all(|(&i, p)| dfs(forest, i, opt, p, uncovered))
        }

        dfs(forest, id, opt, &self.pattern, &mut uncovered).then(|| uncovered)
    }
}

#[derive(Debug)]
pub struct Cover<'a> {
    pub map: HashMap<NodeId, &'a Tile>,
    pub cost: u32,
    pub root: NodeId,
}

pub fn cover_forest<'a>(forest: &SelectionForest, tiles: &'a [Tile]) -> Vec<Cover<'a>> {
    fn dfs<'a>(
        forest: &SelectionForest,
        id: NodeId,
        tiles: &'a [Tile],
        dp: &mut HashMap<NodeId, Cover<'a>>,
    ) {
        for child in forest
            .children(id)
            .filter(|&child| matches!(forest.kind(child), &NodeKind::Op { .. }))
        {
            dfs(forest, child, tiles, dp)
        }

        let mut best = None;

        for tile in tiles {
            let Some(uncovered) = tile.try_cover(forest, id) else {
                continue;
            };

            let mut cost = tile.cost;
            let mut map = HashMap::new();
            map.insert(id, tile);

            for child in &uncovered {
                cost += dp[child].cost;
                map.extend(dp[child].map.iter());
            }

            if best.as_ref().map_or(true, |b: &Cover| cost < b.cost) {
                best = Some(Cover {
                    map,
                    cost,
                    root: id,
                });
            }
        }

        dp.insert(id, best.expect("node should be matched"));
    }

    forest
        .roots
        .iter()
        .map(|&root| {
            let mut dp = HashMap::new();
            dfs(forest, root, tiles, &mut dp);
            dp.remove(&root).unwrap()
        })
        .collect()
}

pub fn isel_tiles() -> Vec<Tile> {
    let assign = Tile::new(pat!(Assign(pat!(any)) -> any), 1, |forest, root| {
        vec![l2::Instruction::Assign {
            dst: translate_node(forest, root),
            src: translate_node(forest, forest.child(root, 0)),
        }]
    });

    let assign_add = Tile::new(pat!(Add(pat!(any), pat!(any)) -> any), 2, |forest, root| {
        let dst = translate_node(forest, root);
        vec![
            l2::Instruction::Assign {
                dst,
                src: translate_node(forest, forest.child(root, 0)),
            },
            l2::Instruction::Arithmetic {
                dst,
                aop: l2::ArithmeticOp::AddAssign,
                src: translate_node(forest, forest.child(root, 1)),
            },
        ]
    });

    let assign_sub = Tile::new(pat!(Sub(pat!(any), pat!(any)) -> any), 2, |forest, root| {
        let dst = translate_node(forest, root);
        vec![
            l2::Instruction::Assign {
                dst,
                src: translate_node(forest, forest.child(root, 0)),
            },
            l2::Instruction::Arithmetic {
                dst,
                aop: l2::ArithmeticOp::SubAssign,
                src: translate_node(forest, forest.child(root, 1)),
            },
        ]
    });

    let assign_mul = Tile::new(pat!(Mul(pat!(any), pat!(any)) -> any), 2, |forest, root| {
        let dst = translate_node(forest, root);
        vec![
            l2::Instruction::Assign {
                dst,
                src: translate_node(forest, forest.child(root, 0)),
            },
            l2::Instruction::Arithmetic {
                dst,
                aop: l2::ArithmeticOp::MulAssign,
                src: translate_node(forest, forest.child(root, 1)),
            },
        ]
    });

    let assign_bit_and = Tile::new(
        pat!(BitAnd(pat!(any), pat!(any)) -> any),
        2,
        |forest, root| {
            let dst = translate_node(forest, root);
            vec![
                l2::Instruction::Assign {
                    dst,
                    src: translate_node(forest, forest.child(root, 0)),
                },
                l2::Instruction::Arithmetic {
                    dst,
                    aop: l2::ArithmeticOp::BitAndAssign,
                    src: translate_node(forest, forest.child(root, 1)),
                },
            ]
        },
    );

    let assign_shl = Tile::new(pat!(Shl(pat!(any), pat!(any)) -> any), 2, |forest, root| {
        let dst = translate_node(forest, root);
        vec![
            l2::Instruction::Assign {
                dst,
                src: translate_node(forest, forest.child(root, 0)),
            },
            l2::Instruction::Shift {
                dst,
                sop: l2::ShiftOp::ShlAssign,
                src: translate_node(forest, forest.child(root, 1)),
            },
        ]
    });

    let assign_shr = Tile::new(pat!(Shr(pat!(any), pat!(any)) -> any), 2, |forest, root| {
        let dst = translate_node(forest, root);
        vec![
            l2::Instruction::Assign {
                dst,
                src: translate_node(forest, forest.child(root, 0)),
            },
            l2::Instruction::Shift {
                dst,
                sop: l2::ShiftOp::ShrAssign,
                src: translate_node(forest, forest.child(root, 1)),
            },
        ]
    });

    let lt = Tile::new(pat!(Lt(pat!(any), pat!(any)) -> any), 1, |forest, root| {
        vec![l2::Instruction::Compare {
            dst: translate_node(forest, root),
            lhs: translate_node(forest, forest.child(root, 0)),
            cmp: l2::CompareOp::Lt,
            rhs: translate_node(forest, forest.child(root, 1)),
        }]
    });

    let le = Tile::new(pat!(Le(pat!(any), pat!(any)) -> any), 1, |forest, root| {
        vec![l2::Instruction::Compare {
            dst: translate_node(forest, root),
            lhs: translate_node(forest, forest.child(root, 0)),
            cmp: l2::CompareOp::Le,
            rhs: translate_node(forest, forest.child(root, 1)),
        }]
    });

    let eq = Tile::new(pat!(Eq(pat!(any), pat!(any)) -> any), 1, |forest, root| {
        vec![l2::Instruction::Compare {
            dst: translate_node(forest, root),
            lhs: translate_node(forest, forest.child(root, 0)),
            cmp: l2::CompareOp::Eq,
            rhs: translate_node(forest, forest.child(root, 1)),
        }]
    });

    let ge = Tile::new(pat!(Ge(pat!(any), pat!(any)) -> any), 1, |forest, root| {
        vec![l2::Instruction::Compare {
            dst: translate_node(forest, root),
            lhs: translate_node(forest, forest.child(root, 1)),
            cmp: l2::CompareOp::Le,
            rhs: translate_node(forest, forest.child(root, 0)),
        }]
    });

    let gt = Tile::new(pat!(Gt(pat!(any), pat!(any)) -> any), 1, |forest, root| {
        vec![l2::Instruction::Compare {
            dst: translate_node(forest, root),
            lhs: translate_node(forest, forest.child(root, 1)),
            cmp: l2::CompareOp::Lt,
            rhs: translate_node(forest, forest.child(root, 0)),
        }]
    });

    let load = Tile::new(pat!(Load(pat!(any)) -> any), 1, |forest, root| {
        vec![l2::Instruction::Load {
            dst: translate_node(forest, root),
            src: translate_node(forest, forest.child(root, 0)),
            offset: 0,
        }]
    });

    let store = Tile::new(pat!(Store(pat!(any), pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Store {
            dst: translate_node(forest, forest.child(root, 0)),
            offset: 0,
            src: translate_node(forest, forest.child(root, 1)),
        }]
    });

    let return_ = Tile::new(pat!(Return), 1, |_, _| vec![l2::Instruction::Return]);

    let return_value = Tile::new(pat!(ReturnValue(pat!(any))), 2, |forest, root| {
        vec![
            l2::Instruction::Assign {
                dst: l2::Value::Register(l2::Register::RAX),
                src: translate_node(forest, forest.child(root, 0)),
            },
            l2::Instruction::Return,
        ]
    });

    let branch = Tile::new(pat!(Branch(pat!(any))), 1, |forest, root| {
        let NodeKind::Value(Value::Label(label)) = forest.kind(forest.child(root, 0)) else {
            unreachable!("branch node should have label");
        };
        vec![l2::Instruction::Goto(l2::SymbolId(label.0))]
    });

    let branch_cond = Tile::new(pat!(BranchCond(pat!(any), pat!(any))), 1, |forest, root| {
        let NodeKind::Value(Value::Label(label)) = forest.kind(forest.child(root, 1)) else {
            unreachable!("branch cond node should have label");
        };
        vec![l2::Instruction::CJump {
            lhs: translate_node(forest, forest.child(root, 1)),
            cmp: l2::CompareOp::Eq,
            rhs: l2::Value::Number(1),
            label: l2::SymbolId(label.0),
        }]
    });

    let add_left = Tile::new(
        pat!(Add(pat!(exact), pat!(any)) -> exact),
        1,
        |forest, root| {
            vec![l2::Instruction::Arithmetic {
                dst: translate_node(forest, root),
                aop: l2::ArithmeticOp::AddAssign,
                src: translate_node(forest, forest.child(root, 1)),
            }]
        },
    );

    let add_right = Tile::new(
        pat!(Add(pat!(any), pat!(exact)) -> exact),
        1,
        |forest, root| {
            vec![l2::Instruction::Arithmetic {
                dst: translate_node(forest, root),
                aop: l2::ArithmeticOp::AddAssign,
                src: translate_node(forest, forest.child(root, 0)),
            }]
        },
    );

    let sub_left = Tile::new(
        pat!(Sub(pat!(exact), pat!(any)) -> exact),
        1,
        |forest, root| {
            vec![l2::Instruction::Arithmetic {
                dst: translate_node(forest, root),
                aop: l2::ArithmeticOp::SubAssign,
                src: translate_node(forest, forest.child(root, 1)),
            }]
        },
    );

    let sub_right = Tile::new(
        pat!(Sub(pat!(any), pat!(exact)) -> exact),
        1,
        |forest, root| {
            vec![l2::Instruction::Arithmetic {
                dst: translate_node(forest, root),
                aop: l2::ArithmeticOp::SubAssign,
                src: translate_node(forest, forest.child(root, 0)),
            }]
        },
    );

    let mul_left = Tile::new(
        pat!(Mul(pat!(exact), pat!(any)) -> exact),
        1,
        |forest, root| {
            vec![l2::Instruction::Arithmetic {
                dst: translate_node(forest, root),
                aop: l2::ArithmeticOp::MulAssign,
                src: translate_node(forest, forest.child(root, 1)),
            }]
        },
    );

    let mul_right = Tile::new(
        pat!(Mul(pat!(any), pat!(exact)) -> exact),
        1,
        |forest, root| {
            vec![l2::Instruction::Arithmetic {
                dst: translate_node(forest, root),
                aop: l2::ArithmeticOp::MulAssign,
                src: translate_node(forest, forest.child(root, 0)),
            }]
        },
    );

    let bit_and_left = Tile::new(
        pat!(BitAnd(pat!(exact), pat!(any)) -> exact),
        1,
        |forest, root| {
            vec![l2::Instruction::Arithmetic {
                dst: translate_node(forest, root),
                aop: l2::ArithmeticOp::BitAndAssign,
                src: translate_node(forest, forest.child(root, 1)),
            }]
        },
    );

    let bit_and_right = Tile::new(
        pat!(BitAnd(pat!(any), pat!(exact)) -> exact),
        1,
        |forest, root| {
            vec![l2::Instruction::Arithmetic {
                dst: translate_node(forest, root),
                aop: l2::ArithmeticOp::BitAndAssign,
                src: translate_node(forest, forest.child(root, 0)),
            }]
        },
    );

    let shl_left = Tile::new(
        pat!(Shl(pat!(exact), pat!(any)) -> exact),
        1,
        |forest, root| {
            vec![l2::Instruction::Shift {
                dst: translate_node(forest, root),
                sop: l2::ShiftOp::ShlAssign,
                src: translate_node(forest, forest.child(root, 1)),
            }]
        },
    );

    let shl_right = Tile::new(
        pat!(Shl(pat!(any), pat!(exact)) -> exact),
        1,
        |forest, root| {
            vec![l2::Instruction::Shift {
                dst: translate_node(forest, root),
                sop: l2::ShiftOp::ShlAssign,
                src: translate_node(forest, forest.child(root, 0)),
            }]
        },
    );

    let shr_left = Tile::new(
        pat!(Shr(pat!(exact), pat!(any)) -> exact),
        1,
        |forest, root| {
            vec![l2::Instruction::Shift {
                dst: translate_node(forest, root),
                sop: l2::ShiftOp::ShrAssign,
                src: translate_node(forest, forest.child(root, 1)),
            }]
        },
    );

    let shr_right = Tile::new(
        pat!(Shr(pat!(any), pat!(exact)) -> exact),
        1,
        |forest, root| {
            vec![l2::Instruction::Shift {
                dst: translate_node(forest, root),
                sop: l2::ShiftOp::ShrAssign,
                src: translate_node(forest, forest.child(root, 0)),
            }]
        },
    );

    vec![
        assign,
        assign_add,
        assign_sub,
        assign_mul,
        assign_bit_and,
        assign_shl,
        assign_shr,
        le,
        lt,
        eq,
        ge,
        gt,
        load,
        store,
        return_,
        return_value,
        branch,
        branch_cond,
        add_left,
        add_right,
        sub_left,
        sub_right,
        mul_left,
        mul_right,
        bit_and_left,
        bit_and_right,
        shl_left,
        shl_right,
        shr_left,
        shr_right,
    ]
}

fn translate_node(forest: &SelectionForest, id: NodeId) -> l2::Value {
    match &forest.kind(id) {
        NodeKind::Op { result, .. } => {
            let Some(Value::Variable(res)) = result else {
                unreachable!("op should have a result");
            };
            l2::Value::Variable(l2::SymbolId(res.0))
        }
        NodeKind::Value(val) => match val {
            Value::Number(num) => l2::Value::Number(*num),
            Value::Label(label) => l2::Value::Label(l2::SymbolId(label.0)),
            Value::Function(callee) => l2::Value::Function(l2::SymbolId(callee.0)),
            Value::Variable(var) => l2::Value::Variable(l2::SymbolId(var.0)),
        },
    }
}
