use std::collections::HashMap;

use l2;
use l3::*;

use crate::isel::forest::{NodeId, NodeKind, SFNode, SelectionForest};
use crate::translation::translate_value;

macro_rules! pat {
    (any) => {
        Pattern {
            children: Vec::new(),
            matches: |node, _|  node.result.is_some(),
        }
    };

    (exact) => {
        Pattern {
            children: Vec::new(),
            matches: |node, res| node.result == res,
        }
    };

    (not) => {
        Pattern {
            children: Vec::new(),
            matches: |node, res| node.result != res,
        }
    };

    (number: $num:literal) => {
        Pattern {
            children: Vec::new(),
            matches: |node, _| {
                matches!(&node.kind, NodeKind::Number)
                    && matches!(&node.result, Some(Value::Number(num)) if *num == $num)
            }
        }
    };

    (multiple: $mul:literal) => {
        Pattern {
            children: Vec::new(),
            matches: |node, _| {
                matches!(&node.kind, NodeKind::Number)
                    && matches!(&node.result, Some(Value::Number(num)) if *num % $mul == 0)
            }
        }
    };

    (power: 2) => {
        Pattern {
            children: Vec::new(),
            matches: |node, _| {
                matches!(&node.kind, NodeKind::Number)
                    && matches!(&node.result, Some(Value::Number(num)) if *num > 0 && *num & (num - 1) == 0)
            }
        }
    };

    ($kind:ident) => {
        Pattern {
            children: Vec::new(),
            matches: |node, _| matches!(&node.kind, NodeKind::$kind),
        }
    };

    ($kind:ident($($child:expr),*)) => {
        Pattern {
            children: vec![$($child),*],
            matches: |node, _| matches!(&node.kind, NodeKind::$kind),
        }
    };
}

#[derive(Debug, Clone)]
pub struct Pattern {
    children: Vec<Pattern>,
    matches: fn(&SFNode, Option<Value>) -> bool,
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

        fn dfs(
            forest: &SelectionForest,
            id: NodeId,
            res: Option<Value>,
            pat: &Pattern,
            uncovered: &mut Vec<NodeId>,
        ) -> bool {
            let node = forest.node(id);

            if !(pat.matches)(node, res) {
                return false;
            }

            if pat.children.is_empty() {
                if node.is_op() && node.result.is_some() {
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
                .all(|(&i, p)| dfs(forest, i, res, p, uncovered))
        }

        dfs(
            forest,
            id,
            forest.node(id).result,
            &self.pattern,
            &mut uncovered,
        )
        .then(|| uncovered)
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
        for &child in forest
            .node(id)
            .children
            .iter()
            .filter(|&&child| forest.node(child).is_op())
        {
            dfs(forest, child, tiles, dp)
        }

        let mut best = None;

        for tile in tiles {
            let Some(uncovered) = tile.try_cover(forest, id) else {
                continue;
            };

            let mut cost = tile.cost;
            let mut map = HashMap::from([(id, tile)]);

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
    let assign = Tile::new(pat!(Assign(pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Assign {
            dst: translate_node(forest, root),
            src: translate_node(forest, forest.child(root, 0)),
        }]
    });

    let assign_add = Tile::new(pat!(Add(pat!(any), pat!(any))), 2, |forest, root| {
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

    let assign_sub = Tile::new(pat!(Sub(pat!(any), pat!(not))), 2, |forest, root| {
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

    let assign_mul = Tile::new(pat!(Mul(pat!(any), pat!(any))), 2, |forest, root| {
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

    let assign_bit_and = Tile::new(pat!(BitAnd(pat!(any), pat!(any))), 2, |forest, root| {
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
    });

    let assign_shl = Tile::new(pat!(Shl(pat!(any), pat!(not))), 2, |forest, root| {
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

    let assign_shr = Tile::new(pat!(Shr(pat!(any), pat!(not))), 2, |forest, root| {
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

    let lt = Tile::new(pat!(Lt(pat!(any), pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Compare {
            dst: translate_node(forest, root),
            lhs: translate_node(forest, forest.child(root, 0)),
            cmp: l2::CompareOp::Lt,
            rhs: translate_node(forest, forest.child(root, 1)),
        }]
    });

    let le = Tile::new(pat!(Le(pat!(any), pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Compare {
            dst: translate_node(forest, root),
            lhs: translate_node(forest, forest.child(root, 0)),
            cmp: l2::CompareOp::Le,
            rhs: translate_node(forest, forest.child(root, 1)),
        }]
    });

    let eq = Tile::new(pat!(Eq(pat!(any), pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Compare {
            dst: translate_node(forest, root),
            lhs: translate_node(forest, forest.child(root, 0)),
            cmp: l2::CompareOp::Eq,
            rhs: translate_node(forest, forest.child(root, 1)),
        }]
    });

    let ge = Tile::new(pat!(Ge(pat!(any), pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Compare {
            dst: translate_node(forest, root),
            lhs: translate_node(forest, forest.child(root, 1)),
            cmp: l2::CompareOp::Le,
            rhs: translate_node(forest, forest.child(root, 0)),
        }]
    });

    let gt = Tile::new(pat!(Gt(pat!(any), pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Compare {
            dst: translate_node(forest, root),
            lhs: translate_node(forest, forest.child(root, 1)),
            cmp: l2::CompareOp::Lt,
            rhs: translate_node(forest, forest.child(root, 0)),
        }]
    });

    let load = Tile::new(pat!(Load(pat!(any))), 1, |forest, root| {
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
        let Some(Value::Label(label)) = &forest.node(forest.child(root, 0)).result else {
            unreachable!("branch node should have label");
        };
        vec![l2::Instruction::Goto(l2::SymbolId(label.0))]
    });

    let branch_cond = Tile::new(pat!(BranchCond(pat!(any), pat!(any))), 1, |forest, root| {
        let Some(Value::Label(label)) = &forest.node(forest.child(root, 1)).result else {
            unreachable!("branch cond node should have label");
        };
        vec![l2::Instruction::CJump {
            lhs: translate_node(forest, forest.child(root, 0)),
            cmp: l2::CompareOp::Eq,
            rhs: l2::Value::Number(1),
            label: l2::SymbolId(label.0),
        }]
    });

    let load_add_offset_left = Tile::new(
        pat!(Load(pat!(Add(pat!(any), pat!(multiple: 8))))),
        1,
        |forest, root| {
            if let Some(Value::Number(num)) =
                &forest.node(forest.child(forest.child(root, 0), 1)).result
                && *num % 8 == 0
            {
                vec![l2::Instruction::Load {
                    dst: translate_node(forest, root),
                    src: translate_node(forest, forest.child(forest.child(root, 0), 0)),
                    offset: *num,
                }]
            } else {
                unreachable!("add node should have number that is multiple of 8")
            }
        },
    );

    let load_add_offset_right = Tile::new(
        pat!(Load(pat!(Add(pat!(multiple: 8), pat!(any))))),
        1,
        |forest, root| {
            if let Some(Value::Number(num)) =
                &forest.node(forest.child(forest.child(root, 0), 0)).result
                && *num % 8 == 0
            {
                vec![l2::Instruction::Load {
                    dst: translate_node(forest, root),
                    src: translate_node(forest, forest.child(forest.child(root, 0), 1)),
                    offset: *num,
                }]
            } else {
                unreachable!("add node should have number that is multiple of 8")
            }
        },
    );

    let load_sub_offset_left = Tile::new(
        pat!(Load(pat!(Sub(pat!(any), pat!(multiple: 8))))),
        1,
        |forest, root| {
            if let Some(Value::Number(num)) =
                &forest.node(forest.child(forest.child(root, 0), 1)).result
                && *num % 8 == 0
            {
                vec![l2::Instruction::Load {
                    dst: translate_node(forest, root),
                    src: translate_node(forest, forest.child(forest.child(root, 0), 0)),
                    offset: -(*num),
                }]
            } else {
                unreachable!("sub node should have number that is multiple of 8")
            }
        },
    );

    let add_left = Tile::new(pat!(Add(pat!(exact), pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Arithmetic {
            dst: translate_node(forest, root),
            aop: l2::ArithmeticOp::AddAssign,
            src: translate_node(forest, forest.child(root, 1)),
        }]
    });

    let add_right = Tile::new(pat!(Add(pat!(any), pat!(exact))), 1, |forest, root| {
        vec![l2::Instruction::Arithmetic {
            dst: translate_node(forest, root),
            aop: l2::ArithmeticOp::AddAssign,
            src: translate_node(forest, forest.child(root, 0)),
        }]
    });

    let sub_left = Tile::new(pat!(Sub(pat!(exact), pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Arithmetic {
            dst: translate_node(forest, root),
            aop: l2::ArithmeticOp::SubAssign,
            src: translate_node(forest, forest.child(root, 1)),
        }]
    });

    let sub_right = Tile::new(pat!(Sub(pat!(any), pat!(exact))), 2, |forest, root| {
        let dst = translate_node(forest, root);
        vec![
            l2::Instruction::Arithmetic {
                dst,
                aop: l2::ArithmeticOp::MulAssign,
                src: l2::Value::Number(-1),
            },
            l2::Instruction::Arithmetic {
                dst,
                aop: l2::ArithmeticOp::AddAssign,
                src: translate_node(forest, forest.child(root, 0)),
            },
        ]
    });

    let mul_left = Tile::new(pat!(Mul(pat!(exact), pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Arithmetic {
            dst: translate_node(forest, root),
            aop: l2::ArithmeticOp::MulAssign,
            src: translate_node(forest, forest.child(root, 1)),
        }]
    });

    let mul_right = Tile::new(pat!(Mul(pat!(any), pat!(exact))), 1, |forest, root| {
        vec![l2::Instruction::Arithmetic {
            dst: translate_node(forest, root),
            aop: l2::ArithmeticOp::MulAssign,
            src: translate_node(forest, forest.child(root, 0)),
        }]
    });

    let bit_and_left = Tile::new(pat!(BitAnd(pat!(exact), pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Arithmetic {
            dst: translate_node(forest, root),
            aop: l2::ArithmeticOp::BitAndAssign,
            src: translate_node(forest, forest.child(root, 1)),
        }]
    });

    let bit_and_right = Tile::new(pat!(BitAnd(pat!(any), pat!(exact))), 1, |forest, root| {
        vec![l2::Instruction::Arithmetic {
            dst: translate_node(forest, root),
            aop: l2::ArithmeticOp::BitAndAssign,
            src: translate_node(forest, forest.child(root, 0)),
        }]
    });

    let shl_left = Tile::new(pat!(Shl(pat!(exact), pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Shift {
            dst: translate_node(forest, root),
            sop: l2::ShiftOp::ShlAssign,
            src: translate_node(forest, forest.child(root, 1)),
        }]
    });

    let shr_left = Tile::new(pat!(Shr(pat!(exact), pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Shift {
            dst: translate_node(forest, root),
            sop: l2::ShiftOp::ShrAssign,
            src: translate_node(forest, forest.child(root, 1)),
        }]
    });

    let shl_mul_left = Tile::new(pat!(Mul(pat!(exact), pat!(power: 2))), 1, |forest, root| {
        if let Some(Value::Number(num)) = &forest.node(forest.child(root, 1)).result
            && *num > 0
            && *num & (*num - 1) == 0
        {
            vec![l2::Instruction::Shift {
                dst: translate_node(forest, root),
                sop: l2::ShiftOp::ShlAssign,
                src: l2::Value::Number(num.trailing_zeros().into()),
            }]
        } else {
            unreachable!("mul node should have number that is power of 2")
        }
    });

    let shl_mul_right = Tile::new(pat!(Mul(pat!(power: 2), pat!(exact))), 1, |forest, root| {
        if let Some(Value::Number(num)) = &forest.node(forest.child(root, 0)).result
            && *num > 0
            && *num & (*num - 1) == 0
        {
            vec![l2::Instruction::Shift {
                dst: translate_node(forest, root),
                sop: l2::ShiftOp::ShlAssign,
                src: l2::Value::Number(num.trailing_zeros().into()),
            }]
        } else {
            unreachable!("mul node should have number that is power of 2")
        }
    });

    let increment_left = Tile::new(
        pat!(Add(pat!(exact), pat!(number: 1))),
        1,
        |forest, root| vec![l2::Instruction::Increment(translate_node(forest, root))],
    );

    let increment_right = Tile::new(
        pat!(Add(pat!(number: 1), pat!(exact))),
        1,
        |forest, root| vec![l2::Instruction::Increment(translate_node(forest, root))],
    );

    let decrement_left = Tile::new(
        pat!(Sub(pat!(exact), pat!(number: 1))),
        1,
        |forest, root| vec![l2::Instruction::Decrement(translate_node(forest, root))],
    );

    vec![
        shl_mul_left,
        shl_mul_right,
        increment_left,
        increment_right,
        decrement_left,
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
        load_add_offset_left,
        load_add_offset_right,
        load_sub_offset_left,
        add_left,
        add_right,
        sub_left,
        sub_right,
        mul_left,
        mul_right,
        bit_and_left,
        bit_and_right,
        shl_left,
        shr_left,
    ]
}

fn translate_node(forest: &SelectionForest, id: NodeId) -> l2::Value {
    match &forest.node(id).result {
        Some(val) => translate_value(val),
        None => unreachable!("nodes should have values"),
    }
}
