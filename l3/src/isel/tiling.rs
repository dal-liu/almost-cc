use std::collections::HashMap;

use l2;
use l3::*;

use crate::isel::forest::{NodeId, NodeKind, SFNode, SelectionForest};
use crate::translation::{translate_symbol_id, translate_value};

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
            matches: |node, res| node.result.as_ref() == res,
        }
    };

    (not) => {
        Pattern {
            children: Vec::new(),
            matches: |node, res| node.result.as_ref() != res,
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

    (scale) => {
        Pattern {
            children: Vec::new(),
            matches: |node, _| {
                matches!(&node.kind, NodeKind::Number)
                    && matches!(&node.result, Some(Value::Number(num)) if matches!(*num, 1 | 2 | 4 | 8))
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
    matches: fn(&SFNode, Option<&Value>) -> bool,
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
            res: Option<&Value>,
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

        dfs(forest, id, forest.result(id), &self.pattern, &mut uncovered).then(|| uncovered)
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
    // VAR <- VAL
    let assign = Tile::new(pat!(Assign(pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Assign {
            dst: translate_node(forest, root),
            src: translate_node(forest, forest.child(root, 0)),
        }]
    });

    // VAR <- VAL1 + VAL2
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

    // VAR <- VAL - NOT VAR
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

    // VAR <- VAL1 * VAL2
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

    // VAR <- VAL1 & VAL2
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

    // VAR <- VAL1 << VAL2
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

    // VAR <- VAL1 >> VAL2
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

    // VAR <- VAL1 < VAL2
    let lt = Tile::new(pat!(Lt(pat!(any), pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Compare {
            dst: translate_node(forest, root),
            lhs: translate_node(forest, forest.child(root, 0)),
            cmp: l2::CompareOp::Lt,
            rhs: translate_node(forest, forest.child(root, 1)),
        }]
    });

    // VAR <- VAL1 <= VAL2
    let le = Tile::new(pat!(Le(pat!(any), pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Compare {
            dst: translate_node(forest, root),
            lhs: translate_node(forest, forest.child(root, 0)),
            cmp: l2::CompareOp::Le,
            rhs: translate_node(forest, forest.child(root, 1)),
        }]
    });

    // VAR <- VAL1 = VAL2
    let eq = Tile::new(pat!(Eq(pat!(any), pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Compare {
            dst: translate_node(forest, root),
            lhs: translate_node(forest, forest.child(root, 0)),
            cmp: l2::CompareOp::Eq,
            rhs: translate_node(forest, forest.child(root, 1)),
        }]
    });

    // VAR <- VAL1 >= VAL2
    let ge = Tile::new(pat!(Ge(pat!(any), pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Compare {
            dst: translate_node(forest, root),
            lhs: translate_node(forest, forest.child(root, 1)),
            cmp: l2::CompareOp::Le,
            rhs: translate_node(forest, forest.child(root, 0)),
        }]
    });

    // VAR <- VAL1 > VAL2
    let gt = Tile::new(pat!(Gt(pat!(any), pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Compare {
            dst: translate_node(forest, root),
            lhs: translate_node(forest, forest.child(root, 1)),
            cmp: l2::CompareOp::Lt,
            rhs: translate_node(forest, forest.child(root, 0)),
        }]
    });

    // VAR1 <- load VAR2
    let load = Tile::new(pat!(Load(pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Load {
            dst: translate_node(forest, root),
            src: translate_node(forest, forest.child(root, 0)),
            offset: 0,
        }]
    });

    // store VAR <- VAL
    let store = Tile::new(pat!(Store(pat!(any), pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Store {
            dst: translate_node(forest, forest.child(root, 0)),
            offset: 0,
            src: translate_node(forest, forest.child(root, 1)),
        }]
    });

    // return
    let return_ = Tile::new(pat!(Return), 1, |_, _| vec![l2::Instruction::Return]);

    // return VAL
    let return_value = Tile::new(pat!(ReturnValue(pat!(any))), 2, |forest, root| {
        vec![
            l2::Instruction::Assign {
                dst: l2::Value::Register(l2::Register::RAX),
                src: translate_node(forest, forest.child(root, 0)),
            },
            l2::Instruction::Return,
        ]
    });

    // br LABEL
    let branch = Tile::new(pat!(Branch(pat!(any))), 1, |forest, root| {
        let Some(Value::Label(label)) = forest.result(forest.child(root, 0)) else {
            unreachable!("branch tile should have label");
        };
        vec![l2::Instruction::Goto(translate_symbol_id(*label))]
    });

    // br VAL LABEL
    let branch_cond = Tile::new(pat!(BranchCond(pat!(any), pat!(any))), 1, |forest, root| {
        let Some(Value::Label(label)) = forest.result(forest.child(root, 1)) else {
            unreachable!("branch cond tile should have label");
        };
        vec![l2::Instruction::CJump {
            lhs: translate_node(forest, forest.child(root, 0)),
            cmp: l2::CompareOp::Eq,
            rhs: l2::Value::Number(1),
            label: translate_symbol_id(*label),
        }]
    });

    // VAR1 <- VAR2 + MUL 8
    // VAR3 <- load VAR1
    let load_add_offset_left = Tile::new(
        pat!(Load(pat!(Add(pat!(any), pat!(multiple: 8))))),
        1,
        |forest, root| {
            let Some(Value::Number(num)) = forest.result(forest.child(forest.child(root, 0), 1))
            else {
                unreachable!("load add offset left tile should have a multiple of 8")
            };
            vec![l2::Instruction::Load {
                dst: translate_node(forest, root),
                src: translate_node(forest, forest.child(forest.child(root, 0), 0)),
                offset: *num,
            }]
        },
    );

    // VAR1 <- MUL 8 + VAR2
    // VAR3 <- load VAR1
    let load_add_offset_right = Tile::new(
        pat!(Load(pat!(Add(pat!(multiple: 8), pat!(any))))),
        1,
        |forest, root| {
            let Some(Value::Number(num)) = forest.result(forest.child(forest.child(root, 0), 0))
            else {
                unreachable!("load add offset right tile should have a multiple of 8")
            };
            vec![l2::Instruction::Load {
                dst: translate_node(forest, root),
                src: translate_node(forest, forest.child(forest.child(root, 0), 1)),
                offset: *num,
            }]
        },
    );

    // VAR1 <- VAR2 - MUL 8
    // VAR3 <- load VAR1
    let load_sub_offset = Tile::new(
        pat!(Load(pat!(Sub(pat!(any), pat!(multiple: 8))))),
        1,
        |forest, root| {
            let Some(Value::Number(num)) = forest.result(forest.child(forest.child(root, 0), 1))
            else {
                unreachable!("load sub offset left tile should have a multiple of 8")
            };
            vec![l2::Instruction::Load {
                dst: translate_node(forest, root),
                src: translate_node(forest, forest.child(forest.child(root, 0), 0)),
                offset: -(*num),
            }]
        },
    );

    // VAR <- VAR + VAL
    let add_left = Tile::new(pat!(Add(pat!(exact), pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Arithmetic {
            dst: translate_node(forest, root),
            aop: l2::ArithmeticOp::AddAssign,
            src: translate_node(forest, forest.child(root, 1)),
        }]
    });

    // VAR <- VAL + VAR
    let add_right = Tile::new(pat!(Add(pat!(any), pat!(exact))), 1, |forest, root| {
        vec![l2::Instruction::Arithmetic {
            dst: translate_node(forest, root),
            aop: l2::ArithmeticOp::AddAssign,
            src: translate_node(forest, forest.child(root, 0)),
        }]
    });

    // VAR <- VAR - VAL
    let sub_left = Tile::new(pat!(Sub(pat!(exact), pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Arithmetic {
            dst: translate_node(forest, root),
            aop: l2::ArithmeticOp::SubAssign,
            src: translate_node(forest, forest.child(root, 1)),
        }]
    });

    // VAR <- VAL - VAR
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

    // VAR <- VAR * VAL
    let mul_left = Tile::new(pat!(Mul(pat!(exact), pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Arithmetic {
            dst: translate_node(forest, root),
            aop: l2::ArithmeticOp::MulAssign,
            src: translate_node(forest, forest.child(root, 1)),
        }]
    });

    // VAR <- VAL * VAR
    let mul_right = Tile::new(pat!(Mul(pat!(any), pat!(exact))), 1, |forest, root| {
        vec![l2::Instruction::Arithmetic {
            dst: translate_node(forest, root),
            aop: l2::ArithmeticOp::MulAssign,
            src: translate_node(forest, forest.child(root, 0)),
        }]
    });

    // VAR <- VAR & VAL
    let bit_and_left = Tile::new(pat!(BitAnd(pat!(exact), pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Arithmetic {
            dst: translate_node(forest, root),
            aop: l2::ArithmeticOp::BitAndAssign,
            src: translate_node(forest, forest.child(root, 1)),
        }]
    });

    // VAR <- VAL & VAR
    let bit_and_right = Tile::new(pat!(BitAnd(pat!(any), pat!(exact))), 1, |forest, root| {
        vec![l2::Instruction::Arithmetic {
            dst: translate_node(forest, root),
            aop: l2::ArithmeticOp::BitAndAssign,
            src: translate_node(forest, forest.child(root, 0)),
        }]
    });

    // VAR <- VAR << VAL
    let shl_left = Tile::new(pat!(Shl(pat!(exact), pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Shift {
            dst: translate_node(forest, root),
            sop: l2::ShiftOp::ShlAssign,
            src: translate_node(forest, forest.child(root, 1)),
        }]
    });

    // VAR <- VAR >> VAL
    let shr_left = Tile::new(pat!(Shr(pat!(exact), pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Shift {
            dst: translate_node(forest, root),
            sop: l2::ShiftOp::ShrAssign,
            src: translate_node(forest, forest.child(root, 1)),
        }]
    });

    // VAR <- VAL * POW 2
    let assign_shl_mul_left = Tile::new(pat!(Mul(pat!(any), pat!(power: 2))), 2, |forest, root| {
        let Some(Value::Number(num)) = forest.result(forest.child(root, 1)) else {
            unreachable!("assign shl mul left tile should have a power of 2")
        };
        let dst = translate_node(forest, root);
        vec![
            l2::Instruction::Assign {
                dst,
                src: translate_node(forest, forest.child(root, 0)),
            },
            l2::Instruction::Shift {
                dst,
                sop: l2::ShiftOp::ShlAssign,
                src: l2::Value::Number(num.trailing_zeros().into()),
            },
        ]
    });

    // VAR <- POW 2 * VAL
    let assign_shl_mul_right =
        Tile::new(pat!(Mul(pat!(power: 2), pat!(any))), 2, |forest, root| {
            let Some(Value::Number(num)) = forest.result(forest.child(root, 0)) else {
                unreachable!("assign shl mul right tile should have a power of 2")
            };
            let dst = translate_node(forest, root);
            vec![
                l2::Instruction::Assign {
                    dst,
                    src: translate_node(forest, forest.child(root, 1)),
                },
                l2::Instruction::Shift {
                    dst,
                    sop: l2::ShiftOp::ShlAssign,
                    src: l2::Value::Number(num.trailing_zeros().into()),
                },
            ]
        });

    // VAR <- VAR * POW 2
    let shl_mul_left = Tile::new(pat!(Mul(pat!(exact), pat!(power: 2))), 1, |forest, root| {
        let Some(Value::Number(num)) = forest.result(forest.child(root, 1)) else {
            unreachable!("shl mul left tile should have a power of 2")
        };
        vec![l2::Instruction::Shift {
            dst: translate_node(forest, root),
            sop: l2::ShiftOp::ShlAssign,
            src: l2::Value::Number(num.trailing_zeros().into()),
        }]
    });

    // VAR <- POW 2 * VAR
    let shl_mul_right = Tile::new(pat!(Mul(pat!(power: 2), pat!(exact))), 1, |forest, root| {
        let Some(Value::Number(num)) = forest.result(forest.child(root, 0)) else {
            unreachable!("shl mul right tile should have a power of 2")
        };
        vec![l2::Instruction::Shift {
            dst: translate_node(forest, root),
            sop: l2::ShiftOp::ShlAssign,
            src: l2::Value::Number(num.trailing_zeros().into()),
        }]
    });

    // VAR <- VAL1 < VAL2
    // br VAR LABEL
    let branch_cond_lt = Tile::new(
        pat!(BranchCond(pat!(Lt(pat!(any), pat!(any))), pat!(any))),
        1,
        |forest, root| {
            let Some(Value::Label(label)) = forest.result(forest.child(root, 1)) else {
                unreachable!("branch cond lt tile should have label");
            };
            vec![l2::Instruction::CJump {
                lhs: translate_node(forest, forest.child(forest.child(root, 0), 0)),
                cmp: l2::CompareOp::Lt,
                rhs: translate_node(forest, forest.child(forest.child(root, 0), 1)),
                label: translate_symbol_id(*label),
            }]
        },
    );

    // VAR <- VAL1 <= VAL2
    // br VAR LABEL
    let branch_cond_le = Tile::new(
        pat!(BranchCond(pat!(Le(pat!(any), pat!(any))), pat!(any))),
        1,
        |forest, root| {
            let Some(Value::Label(label)) = forest.result(forest.child(root, 1)) else {
                unreachable!("branch cond le tile should have label");
            };
            vec![l2::Instruction::CJump {
                lhs: translate_node(forest, forest.child(forest.child(root, 0), 0)),
                cmp: l2::CompareOp::Le,
                rhs: translate_node(forest, forest.child(forest.child(root, 0), 1)),
                label: translate_symbol_id(*label),
            }]
        },
    );

    // VAR <- VAL1 = VAL2
    // br VAR LABEL
    let branch_cond_eq = Tile::new(
        pat!(BranchCond(pat!(Eq(pat!(any), pat!(any))), pat!(any))),
        1,
        |forest, root| {
            let Some(Value::Label(label)) = forest.result(forest.child(root, 1)) else {
                unreachable!("branch cond eq tile should have label");
            };
            vec![l2::Instruction::CJump {
                lhs: translate_node(forest, forest.child(forest.child(root, 0), 0)),
                cmp: l2::CompareOp::Eq,
                rhs: translate_node(forest, forest.child(forest.child(root, 0), 1)),
                label: translate_symbol_id(*label),
            }]
        },
    );

    // VAR <- VAL1 >= VAL2
    // br VAR LABEL
    let branch_cond_ge = Tile::new(
        pat!(BranchCond(pat!(Ge(pat!(any), pat!(any))), pat!(any))),
        1,
        |forest, root| {
            let Some(Value::Label(label)) = forest.result(forest.child(root, 1)) else {
                unreachable!("branch cond ge tile should have label");
            };
            vec![l2::Instruction::CJump {
                lhs: translate_node(forest, forest.child(forest.child(root, 0), 1)),
                cmp: l2::CompareOp::Le,
                rhs: translate_node(forest, forest.child(forest.child(root, 0), 0)),
                label: translate_symbol_id(*label),
            }]
        },
    );

    // VAR <- VAL1 > VAL2
    // br VAR LABEL
    let branch_cond_gt = Tile::new(
        pat!(BranchCond(pat!(Gt(pat!(any), pat!(any))), pat!(any))),
        1,
        |forest, root| {
            let Some(Value::Label(label)) = forest.result(forest.child(root, 1)) else {
                unreachable!("branch cond gt tile should have label");
            };
            vec![l2::Instruction::CJump {
                lhs: translate_node(forest, forest.child(forest.child(root, 0), 1)),
                cmp: l2::CompareOp::Lt,
                rhs: translate_node(forest, forest.child(forest.child(root, 0), 0)),
                label: translate_symbol_id(*label),
            }]
        },
    );

    // VAR <- VAL + 1
    let assign_increment_left =
        Tile::new(pat!(Add(pat!(any), pat!(number: 1))), 2, |forest, root| {
            let dst = translate_node(forest, root);
            vec![
                l2::Instruction::Assign {
                    dst,
                    src: translate_node(forest, forest.child(root, 0)),
                },
                l2::Instruction::Increment(dst),
            ]
        });

    // VAR <- 1 + VAL
    let assign_increment_right =
        Tile::new(pat!(Add(pat!(number: 1), pat!(any))), 2, |forest, root| {
            let dst = translate_node(forest, root);
            vec![
                l2::Instruction::Assign {
                    dst,
                    src: translate_node(forest, forest.child(root, 1)),
                },
                l2::Instruction::Increment(dst),
            ]
        });

    // VAR <- VAL - 1
    let assign_decrement = Tile::new(pat!(Sub(pat!(any), pat!(number: 1))), 2, |forest, root| {
        let dst = translate_node(forest, root);
        vec![
            l2::Instruction::Assign {
                dst,
                src: translate_node(forest, forest.child(root, 1)),
            },
            l2::Instruction::Decrement(dst),
        ]
    });

    // VAR <- VAR + 1
    let increment_left = Tile::new(
        pat!(Add(pat!(exact), pat!(number: 1))),
        1,
        |forest, root| vec![l2::Instruction::Increment(translate_node(forest, root))],
    );

    // VAR <- 1 + VAR
    let increment_right = Tile::new(
        pat!(Add(pat!(number: 1), pat!(exact))),
        1,
        |forest, root| vec![l2::Instruction::Increment(translate_node(forest, root))],
    );

    // VAR <- VAR - 1
    let decrement_left = Tile::new(
        pat!(Sub(pat!(exact), pat!(number: 1))),
        1,
        |forest, root| vec![l2::Instruction::Decrement(translate_node(forest, root))],
    );

    // VAR <- 1 - VAR
    let decrement_right = Tile::new(
        pat!(Sub(pat!(number: 1), pat!(exact))),
        2,
        |forest, root| {
            let dst = translate_node(forest, root);
            vec![
                l2::Instruction::Arithmetic {
                    dst,
                    aop: l2::ArithmeticOp::MulAssign,
                    src: l2::Value::Number(-1),
                },
                l2::Instruction::Increment(dst),
            ]
        },
    );

    // VAR1 <- VAR2 * NUM 1 | 2 | 4 | 8
    // VAR4 <- VAL3 + VAR1
    let lea_left_left = Tile::new(
        pat!(Add(pat!(Variable), pat!(Mul(pat!(Variable), pat!(scale))))),
        1,
        |forest, root| {
            let Some(Value::Number(num)) = forest.result(forest.child(forest.child(root, 1), 1))
            else {
                unreachable!("lea left left tile should have 1, 2, 4, or 8")
            };
            {
                vec![l2::Instruction::LEA {
                    dst: translate_node(forest, root),
                    src: translate_node(forest, forest.child(root, 0)),
                    offset: translate_node(forest, forest.child(forest.child(root, 1), 0)),
                    scale: *num as u8,
                }]
            }
        },
    );

    // VAR1 <- NUM 1 | 2 | 4 | 8 * VAR2
    // VAR4 <- VAR3 + VAR1
    let lea_left_right = Tile::new(
        pat!(Add(pat!(Variable), pat!(Mul(pat!(scale), pat!(Variable))))),
        1,
        |forest, root| {
            let Some(Value::Number(num)) = forest.result(forest.child(forest.child(root, 1), 0))
            else {
                unreachable!("lea left right tile should have 1, 2, 4, or 8")
            };
            {
                vec![l2::Instruction::LEA {
                    dst: translate_node(forest, root),
                    src: translate_node(forest, forest.child(root, 0)),
                    offset: translate_node(forest, forest.child(forest.child(root, 1), 0)),
                    scale: *num as u8,
                }]
            }
        },
    );

    // VAR1 <- VAR2 * NUM 1 | 2 | 4 | 8
    // VAR4 <- VAR1 + VAR3
    let lea_right_left = Tile::new(
        pat!(Add(pat!(Mul(pat!(Variable), pat!(scale))), pat!(Variable))),
        1,
        |forest, root| {
            let Some(Value::Number(num)) = forest.result(forest.child(forest.child(root, 0), 1))
            else {
                unreachable!("lea right left tile should have 1, 2, 4, or 8")
            };
            {
                vec![l2::Instruction::LEA {
                    dst: translate_node(forest, root),
                    src: translate_node(forest, forest.child(root, 1)),
                    offset: translate_node(forest, forest.child(forest.child(root, 0), 0)),
                    scale: *num as u8,
                }]
            }
        },
    );

    // VAR1 <- NUM 1 | 2 | 4 | 8 * VAR2
    // VAR4 <- VAR1 + VAR3
    let lea_right_right = Tile::new(
        pat!(Add(pat!(Mul(pat!(scale), pat!(Variable))), pat!(Variable))),
        1,
        |forest, root| {
            let Some(Value::Number(num)) = forest.result(forest.child(forest.child(root, 0), 0))
            else {
                unreachable!("lea right right tile should have 1, 2, 4, or 8")
            };
            {
                vec![l2::Instruction::LEA {
                    dst: translate_node(forest, root),
                    src: translate_node(forest, forest.child(root, 1)),
                    offset: translate_node(forest, forest.child(forest.child(root, 0), 1)),
                    scale: *num as u8,
                }]
            }
        },
    );

    // TODO: store offset, arith mem
    vec![
        assign_shl_mul_left,
        assign_shl_mul_right,
        shl_mul_left,
        shl_mul_right,
        assign_increment_left,
        assign_increment_right,
        assign_decrement,
        increment_left,
        increment_right,
        decrement_left,
        decrement_right,
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
        load_sub_offset,
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
        branch_cond_lt,
        branch_cond_le,
        branch_cond_eq,
        branch_cond_ge,
        branch_cond_gt,
        lea_left_left,
        lea_left_right,
        lea_right_left,
        lea_right_right,
    ]
}

fn translate_node(forest: &SelectionForest, id: NodeId) -> l2::Value {
    match forest.result(id) {
        Some(val) => translate_value(val),
        None => unreachable!("nodes should have values"),
    }
}
