use std::cmp::Reverse;

use l2;
use l3::*;

use crate::isel::forest::{NodeKind, OpKind, SFNode, SelectionForest};

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
                } => *result == opt,
                NodeKind::Value(val) => Some(*val) == opt,
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
                    } if *result == opt
                )
            },
        }
    };
}

type NodeId = usize;

#[derive(Debug)]
pub struct Pattern {
    children: Vec<Pattern>,
    matches: fn(&SFNode, Option<Value>) -> bool,
}

#[derive(Debug)]
pub struct Tile {
    size: u32,
    pattern: Pattern,
    cost: u32,
    emit: fn(&SelectionForest, NodeId) -> Vec<l2::Instruction>,
}

impl Tile {
    pub fn new(
        size: u32,
        pattern: Pattern,
        cost: u32,
        emit: fn(&SelectionForest, NodeId) -> Vec<l2::Instruction>,
    ) -> Self {
        Self {
            size,
            pattern,
            cost,
            emit,
        }
    }

    fn try_cover(&self, forest: &SelectionForest, root: NodeId) -> Option<Vec<NodeId>> {
        fn dfs(
            forest: &SelectionForest,
            id: NodeId,
            opt: Option<Value>,
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

        let NodeKind::Op { result: opt, .. } = forest.arena[root].kind else {
            unreachable!("roots should be ops");
        };

        let mut uncovered = Vec::new();
        dfs(forest, root, opt, &self.pattern, &mut uncovered).then(|| uncovered)
    }
}

pub fn greedy_match(forest: &SelectionForest) {
    fn dfs(forest: &SelectionForest, id: NodeId, tiles: &[Tile]) {
        let tile = tiles
            .iter()
            .find(|tile| {
                tile.try_cover(forest, id)
                    .and_then(|children| {
                        Some(children.iter().for_each(|&child| dfs(forest, child, tiles)))
                    })
                    .is_some()
            })
            .unwrap();
        dbg!((tile.emit)(forest, id));
    }
    let tiles = tiles();
    for &root in &forest.roots {
        dfs(forest, root, &tiles);
    }
}

fn translate_node(forest: &SelectionForest, id: NodeId) -> l2::Value {
    match &forest.arena[id].kind {
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

fn tiles() -> Vec<Tile> {
    use l2::Instruction as L2;

    let assign = Tile::new(2, pat!(Assign(pat!(any)) -> any), 1, |forest, root| {
        vec![L2::Assign {
            dst: translate_node(forest, root),
            src: translate_node(forest, forest.child_of(root, 0)),
        }]
    });

    let assign_add = Tile::new(
        3,
        pat!(Add(pat!(any), pat!(any)) -> any),
        2,
        |forest, root| {
            let dst = translate_node(forest, root);
            vec![
                L2::Assign {
                    dst,
                    src: translate_node(forest, forest.child_of(root, 0)),
                },
                L2::Arithmetic {
                    dst,
                    aop: l2::ArithmeticOp::AddAssign,
                    src: translate_node(forest, forest.child_of(root, 1)),
                },
            ]
        },
    );

    let assign_sub = Tile::new(
        3,
        pat!(Sub(pat!(any), pat!(any)) -> any),
        2,
        |forest, root| {
            let dst = translate_node(forest, root);
            vec![
                L2::Assign {
                    dst,
                    src: translate_node(forest, forest.child_of(root, 0)),
                },
                L2::Arithmetic {
                    dst,
                    aop: l2::ArithmeticOp::SubAssign,
                    src: translate_node(forest, forest.child_of(root, 1)),
                },
            ]
        },
    );

    let assign_mul = Tile::new(
        3,
        pat!(Mul(pat!(any), pat!(any)) -> any),
        2,
        |forest, root| {
            let dst = translate_node(forest, root);
            vec![
                L2::Assign {
                    dst,
                    src: translate_node(forest, forest.child_of(root, 0)),
                },
                L2::Arithmetic {
                    dst,
                    aop: l2::ArithmeticOp::MulAssign,
                    src: translate_node(forest, forest.child_of(root, 1)),
                },
            ]
        },
    );

    let assign_bit_and = Tile::new(
        3,
        pat!(BitAnd(pat!(any), pat!(any)) -> any),
        2,
        |forest, root| {
            let dst = translate_node(forest, root);
            vec![
                L2::Assign {
                    dst,
                    src: translate_node(forest, forest.child_of(root, 0)),
                },
                L2::Arithmetic {
                    dst,
                    aop: l2::ArithmeticOp::BitAndAssign,
                    src: translate_node(forest, forest.child_of(root, 1)),
                },
            ]
        },
    );

    let assign_shl = Tile::new(
        3,
        pat!(Shl(pat!(any), pat!(any)) -> any),
        2,
        |forest, root| {
            let dst = translate_node(forest, root);
            vec![
                L2::Assign {
                    dst,
                    src: translate_node(forest, forest.child_of(root, 0)),
                },
                L2::Shift {
                    dst,
                    sop: l2::ShiftOp::ShlAssign,
                    src: translate_node(forest, forest.child_of(root, 1)),
                },
            ]
        },
    );

    let assign_shr = Tile::new(
        3,
        pat!(Shr(pat!(any), pat!(any)) -> any),
        2,
        |forest, root| {
            let dst = translate_node(forest, root);
            vec![
                L2::Assign {
                    dst,
                    src: translate_node(forest, forest.child_of(root, 0)),
                },
                L2::Shift {
                    dst,
                    sop: l2::ShiftOp::ShrAssign,
                    src: translate_node(forest, forest.child_of(root, 1)),
                },
            ]
        },
    );

    let lt = Tile::new(
        3,
        pat!(Lt(pat!(any), pat!(any)) -> any),
        1,
        |forest, root| {
            vec![L2::Compare {
                dst: translate_node(forest, root),
                lhs: translate_node(forest, forest.child_of(root, 0)),
                cmp: l2::CompareOp::Lt,
                rhs: translate_node(forest, forest.child_of(root, 1)),
            }]
        },
    );

    let le = Tile::new(
        3,
        pat!(Le(pat!(any), pat!(any)) -> any),
        1,
        |forest, root| {
            vec![L2::Compare {
                dst: translate_node(forest, root),
                lhs: translate_node(forest, forest.child_of(root, 0)),
                cmp: l2::CompareOp::Le,
                rhs: translate_node(forest, forest.child_of(root, 1)),
            }]
        },
    );

    let eq = Tile::new(
        3,
        pat!(Eq(pat!(any), pat!(any)) -> any),
        1,
        |forest, root| {
            vec![L2::Compare {
                dst: translate_node(forest, root),
                lhs: translate_node(forest, forest.child_of(root, 0)),
                cmp: l2::CompareOp::Eq,
                rhs: translate_node(forest, forest.child_of(root, 1)),
            }]
        },
    );

    let ge = Tile::new(
        3,
        pat!(Ge(pat!(any), pat!(any)) -> any),
        1,
        |forest, root| {
            vec![L2::Compare {
                dst: translate_node(forest, root),
                lhs: translate_node(forest, forest.child_of(root, 1)),
                cmp: l2::CompareOp::Le,
                rhs: translate_node(forest, forest.child_of(root, 0)),
            }]
        },
    );

    let gt = Tile::new(
        3,
        pat!(Gt(pat!(any), pat!(any)) -> any),
        1,
        |forest, root| {
            vec![L2::Compare {
                dst: translate_node(forest, root),
                lhs: translate_node(forest, forest.child_of(root, 1)),
                cmp: l2::CompareOp::Lt,
                rhs: translate_node(forest, forest.child_of(root, 0)),
            }]
        },
    );

    let load = Tile::new(2, pat!(Load(pat!(any)) -> any), 1, |forest, root| {
        vec![L2::Load {
            dst: translate_node(forest, root),
            src: translate_node(forest, forest.child_of(root, 0)),
            offset: 0,
        }]
    });

    let store = Tile::new(3, pat!(Store(pat!(any), pat!(any))), 1, |forest, root| {
        vec![L2::Store {
            dst: translate_node(forest, forest.child_of(root, 0)),
            offset: 0,
            src: translate_node(forest, forest.child_of(root, 1)),
        }]
    });

    let return_ = Tile::new(1, pat!(Return), 1, |_, _| vec![L2::Return]);

    let return_value = Tile::new(2, pat!(ReturnValue(pat!(any))), 2, |forest, root| {
        vec![
            L2::Assign {
                dst: l2::Value::Register(l2::Register::RAX),
                src: translate_node(forest, forest.child_of(root, 0)),
            },
            L2::Return,
        ]
    });

    let branch = Tile::new(2, pat!(Branch(pat!(any))), 1, |forest, root| {
        let NodeKind::Value(Value::Label(label)) = forest.arena[forest.child_of(root, 0)].kind
        else {
            unreachable!("branch node should have label");
        };
        vec![L2::Goto(l2::SymbolId(label.0))]
    });

    let branch_cond = Tile::new(
        3,
        pat!(BranchCond(pat!(any), pat!(any))),
        1,
        |forest, root| {
            let NodeKind::Value(Value::Label(label)) = forest.arena[forest.child_of(root, 1)].kind
            else {
                unreachable!("branch cond node should have label");
            };
            vec![L2::CJump {
                lhs: translate_node(forest, forest.child_of(root, 1)),
                cmp: l2::CompareOp::Eq,
                rhs: l2::Value::Number(1),
                label: l2::SymbolId(label.0),
            }]
        },
    );

    let add_left = Tile::new(
        3,
        pat!(Add(pat!(exact), pat!(any)) -> exact),
        1,
        |forest, root| {
            vec![L2::Arithmetic {
                dst: translate_node(forest, root),
                aop: l2::ArithmeticOp::AddAssign,
                src: translate_node(forest, forest.child_of(root, 1)),
            }]
        },
    );

    let add_right = Tile::new(
        3,
        pat!(Add(pat!(any), pat!(exact)) -> exact),
        1,
        |forest, root| {
            vec![L2::Arithmetic {
                dst: translate_node(forest, root),
                aop: l2::ArithmeticOp::AddAssign,
                src: translate_node(forest, forest.child_of(root, 0)),
            }]
        },
    );

    let sub_left = Tile::new(
        3,
        pat!(Sub(pat!(exact), pat!(any)) -> exact),
        1,
        |forest, root| {
            vec![L2::Arithmetic {
                dst: translate_node(forest, root),
                aop: l2::ArithmeticOp::SubAssign,
                src: translate_node(forest, forest.child_of(root, 1)),
            }]
        },
    );

    let sub_right = Tile::new(
        3,
        pat!(Sub(pat!(any), pat!(exact)) -> exact),
        1,
        |forest, root| {
            vec![L2::Arithmetic {
                dst: translate_node(forest, root),
                aop: l2::ArithmeticOp::SubAssign,
                src: translate_node(forest, forest.child_of(root, 0)),
            }]
        },
    );

    let mul_left = Tile::new(
        3,
        pat!(Mul(pat!(exact), pat!(any)) -> exact),
        1,
        |forest, root| {
            vec![L2::Arithmetic {
                dst: translate_node(forest, root),
                aop: l2::ArithmeticOp::MulAssign,
                src: translate_node(forest, forest.child_of(root, 1)),
            }]
        },
    );

    let mul_right = Tile::new(
        3,
        pat!(Mul(pat!(any), pat!(exact)) -> exact),
        1,
        |forest, root| {
            vec![L2::Arithmetic {
                dst: translate_node(forest, root),
                aop: l2::ArithmeticOp::MulAssign,
                src: translate_node(forest, forest.child_of(root, 0)),
            }]
        },
    );

    let bit_and_left = Tile::new(
        3,
        pat!(BitAnd(pat!(exact), pat!(any)) -> exact),
        1,
        |forest, root| {
            vec![L2::Arithmetic {
                dst: translate_node(forest, root),
                aop: l2::ArithmeticOp::BitAndAssign,
                src: translate_node(forest, forest.child_of(root, 1)),
            }]
        },
    );

    let bit_and_right = Tile::new(
        3,
        pat!(BitAnd(pat!(any), pat!(exact)) -> exact),
        1,
        |forest, root| {
            vec![L2::Arithmetic {
                dst: translate_node(forest, root),
                aop: l2::ArithmeticOp::BitAndAssign,
                src: translate_node(forest, forest.child_of(root, 0)),
            }]
        },
    );

    let shl_left = Tile::new(
        3,
        pat!(Shl(pat!(exact), pat!(any)) -> exact),
        1,
        |forest, root| {
            vec![L2::Shift {
                dst: translate_node(forest, root),
                sop: l2::ShiftOp::ShlAssign,
                src: translate_node(forest, forest.child_of(root, 1)),
            }]
        },
    );

    let shl_right = Tile::new(
        3,
        pat!(Shl(pat!(any), pat!(exact)) -> exact),
        1,
        |forest, root| {
            vec![L2::Shift {
                dst: translate_node(forest, root),
                sop: l2::ShiftOp::ShlAssign,
                src: translate_node(forest, forest.child_of(root, 0)),
            }]
        },
    );

    let shr_left = Tile::new(
        3,
        pat!(Shr(pat!(exact), pat!(any)) -> exact),
        1,
        |forest, root| {
            vec![L2::Shift {
                dst: translate_node(forest, root),
                sop: l2::ShiftOp::ShrAssign,
                src: translate_node(forest, forest.child_of(root, 1)),
            }]
        },
    );

    let shr_right = Tile::new(
        3,
        pat!(Shr(pat!(any), pat!(exact)) -> exact),
        1,
        |forest, root| {
            vec![L2::Shift {
                dst: translate_node(forest, root),
                sop: l2::ShiftOp::ShrAssign,
                src: translate_node(forest, forest.child_of(root, 0)),
            }]
        },
    );

    let mut tiles = vec![
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
    ];
    tiles.sort_by_key(|tile| (Reverse(tile.size), tile.cost));
    tiles
}
