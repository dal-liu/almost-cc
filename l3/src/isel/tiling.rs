use std::collections::HashMap;

use l2;
use l3::*;

use crate::isel::forest::{NodeId, NodeKind, OpKind, SelectionForest};
use crate::translation::{translate_symbol_id, translate_value};

macro_rules! pat {
    (any) => {
        Pattern {
            children: Vec::new(),
            matches: |forest, id, _| forest.result(id).is_some().then_some(None),
        }
    };

    (exact) => {
        Pattern {
            children: Vec::new(),
            matches: |forest, id, res| (forest.result(id) == res).then_some(None),
        }
    };

    (not) => {
        Pattern {
            children: Vec::new(),
            matches: |forest, id, res| (forest.result(id) != res).then_some(None),
        }
    };

    (num: $num:literal) => {
        Pattern {
            children: Vec::new(),
            matches: |forest, id, _| {
                matches!(forest.result(id), Some(Value::Number(num)) if *num == $num).then_some(None)
            },
        }
    };

    (mul: $mul:literal) => {
        Pattern {
            children: Vec::new(),
            matches: |forest, id, _| {
                matches!(forest.result(id), Some(Value::Number(num)) if *num % $mul == 0).then_some(None)
            },
        }
    };

    (pow) => {
        Pattern {
            children: Vec::new(),
            matches: |forest, id, _| {
                matches!(forest.result(id), Some(Value::Number(num)) if *num > 0 && *num & (num - 1) == 0).then_some(None)
            },
        }
    };

    (scale) => {
        Pattern {
            children: Vec::new(),
            matches: |forest, id, _| {
                matches!(forest.result(id), Some(Value::Number(num)) if matches!(*num, 1 | 2 | 4 | 8))
                    .then_some(None)
            },
        }
    };

    (var) => {
        Pattern {
            children: Vec::new(),
            matches: |forest, id, _| matches!(forest.result(id), Some(Value::Variable(_))).then_some(None),
        }
    };

    ($kind:ident) => {
        Pattern {
            children: Vec::new(),
            matches: |forest, id, _| matches!(forest.kind(id), NodeKind::Op(OpKind::$kind)).then_some(None),
        }
    };

    ($kind:ident($($child:expr),*)) => {
        Pattern {
            children: vec![$($child),*],
            matches: |forest, id, _| matches!(forest.kind(id), NodeKind::Op(OpKind::$kind)).then_some(None),
        }
    };

    ($kind:ident($($child:expr),*) -> inherit) => {
        Pattern {
            children: vec![$($child),*],
            matches: |forest, id, res| matches!(forest.kind(id), NodeKind::Op(OpKind::$kind)).then_some(res),
        }
    };

    ($kind:ident($($child:expr),*) -> res) => {
        Pattern {
            children: vec![$($child),*],
            matches: |forest, id, _| {
                matches!(forest.kind(id), NodeKind::Op(OpKind::$kind)).then(|| forest.result(id))
            },
        }
    };
}

impl SelectionForest {
    fn translate_node(&self, id: NodeId) -> l2::Value {
        match self.result(id) {
            Some(val) => translate_value(val),
            None => unreachable!("translatable nodes should have values"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Pattern {
    children: Vec<Pattern>,
    matches:
        for<'a> fn(&'a SelectionForest, NodeId, Option<&'a Value>) -> Option<Option<&'a Value>>,
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
            let Some(res) = (pat.matches)(forest, id, res) else {
                return false;
            };

            if pat.children.is_empty() {
                if matches!(forest.kind(id), NodeKind::Op(_)) && forest.result(id).is_some() {
                    uncovered.push(id);
                }
                return true;
            }

            if pat.children.len() != forest.num_children(id) {
                return false;
            }

            forest
                .children(id)
                .zip(&pat.children)
                .all(|(i, p)| dfs(forest, i, res, p, uncovered))
        }

        dfs(forest, id, None, &self.pattern, &mut uncovered).then(|| uncovered)
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
            .filter(|&child| matches!(forest.kind(child), NodeKind::Op(_)))
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
        .roots()
        .map(|root| {
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
            dst: forest.translate_node(root),
            src: forest.translate_node(forest.child(root, 0)),
        }]
    });

    // VAR1 <- VAL + NOT VAR1
    let add_any = Tile::new(pat!(Add(pat!(any), pat!(not)) -> res), 2, |forest, root| {
        let dst = forest.translate_node(root);
        vec![
            l2::Instruction::Assign {
                dst,
                src: forest.translate_node(forest.child(root, 0)),
            },
            l2::Instruction::Arithmetic {
                dst,
                aop: l2::ArithmeticOp::AddAssign,
                src: forest.translate_node(forest.child(root, 1)),
            },
        ]
    });

    // VAR1 <- VAR1 + VAL
    let add_exact_left = Tile::new(
        pat!(Add(pat!(exact), pat!(any)) -> res),
        1,
        |forest, root| {
            vec![l2::Instruction::Arithmetic {
                dst: forest.translate_node(root),
                aop: l2::ArithmeticOp::AddAssign,
                src: forest.translate_node(forest.child(root, 1)),
            }]
        },
    );

    // VAR1 <- VAL + VAR1
    let add_exact_right = Tile::new(
        pat!(Add(pat!(any), pat!(exact)) -> res),
        1,
        |forest, root| {
            vec![l2::Instruction::Arithmetic {
                dst: forest.translate_node(root),
                aop: l2::ArithmeticOp::AddAssign,
                src: forest.translate_node(forest.child(root, 0)),
            }]
        },
    );

    // VAR1 <- VAL - NOT VAR1
    let sub_any = Tile::new(pat!(Sub(pat!(any), pat!(not)) -> res), 2, |forest, root| {
        let dst = forest.translate_node(root);
        vec![
            l2::Instruction::Assign {
                dst,
                src: forest.translate_node(forest.child(root, 0)),
            },
            l2::Instruction::Arithmetic {
                dst,
                aop: l2::ArithmeticOp::SubAssign,
                src: forest.translate_node(forest.child(root, 1)),
            },
        ]
    });

    // VAR1 <- VAR1 - VAL
    let sub_exact_left = Tile::new(
        pat!(Sub(pat!(exact), pat!(any)) -> res),
        1,
        |forest, root| {
            vec![l2::Instruction::Arithmetic {
                dst: forest.translate_node(root),
                aop: l2::ArithmeticOp::SubAssign,
                src: forest.translate_node(forest.child(root, 1)),
            }]
        },
    );

    // VAR1 <- VAL - VAR1
    let sub_exact_right = Tile::new(
        pat!(Sub(pat!(any), pat!(exact)) -> res),
        2,
        |forest, root| {
            let dst = forest.translate_node(root);
            vec![
                l2::Instruction::Arithmetic {
                    dst,
                    aop: l2::ArithmeticOp::MulAssign,
                    src: l2::Value::Number(-1),
                },
                l2::Instruction::Arithmetic {
                    dst,
                    aop: l2::ArithmeticOp::AddAssign,
                    src: forest.translate_node(forest.child(root, 0)),
                },
            ]
        },
    );

    // VAR1 <- VAL * NOT VAR1
    let mul_any = Tile::new(pat!(Mul(pat!(any), pat!(not)) -> res), 2, |forest, root| {
        let dst = forest.translate_node(root);
        vec![
            l2::Instruction::Assign {
                dst,
                src: forest.translate_node(forest.child(root, 0)),
            },
            l2::Instruction::Arithmetic {
                dst,
                aop: l2::ArithmeticOp::MulAssign,
                src: forest.translate_node(forest.child(root, 1)),
            },
        ]
    });

    // VAR1 <- VAR1 * VAL
    let mul_exact_left = Tile::new(
        pat!(Mul(pat!(exact), pat!(any)) -> res),
        1,
        |forest, root| {
            vec![l2::Instruction::Arithmetic {
                dst: forest.translate_node(root),
                aop: l2::ArithmeticOp::MulAssign,
                src: forest.translate_node(forest.child(root, 1)),
            }]
        },
    );

    // VAR1 <- VAL * VAR1
    let mul_exact_right = Tile::new(
        pat!(Mul(pat!(any), pat!(exact)) -> res),
        1,
        |forest, root| {
            vec![l2::Instruction::Arithmetic {
                dst: forest.translate_node(root),
                aop: l2::ArithmeticOp::MulAssign,
                src: forest.translate_node(forest.child(root, 0)),
            }]
        },
    );

    // VAR1 <- VAL & NOT VAR1
    let bit_and_any = Tile::new(
        pat!(BitAnd(pat!(any), pat!(not)) -> res),
        2,
        |forest, root| {
            let dst = forest.translate_node(root);
            vec![
                l2::Instruction::Assign {
                    dst,
                    src: forest.translate_node(forest.child(root, 0)),
                },
                l2::Instruction::Arithmetic {
                    dst,
                    aop: l2::ArithmeticOp::BitAndAssign,
                    src: forest.translate_node(forest.child(root, 1)),
                },
            ]
        },
    );

    // VAR1 <- VAR1 & VAL
    let bit_and_exact_left = Tile::new(
        pat!(BitAnd(pat!(exact), pat!(any)) -> res),
        1,
        |forest, root| {
            vec![l2::Instruction::Arithmetic {
                dst: forest.translate_node(root),
                aop: l2::ArithmeticOp::BitAndAssign,
                src: forest.translate_node(forest.child(root, 1)),
            }]
        },
    );

    // VAR1 <- VAL & VAR1
    let bit_and_exact_right = Tile::new(
        pat!(BitAnd(pat!(any), pat!(exact)) -> res),
        1,
        |forest, root| {
            vec![l2::Instruction::Arithmetic {
                dst: forest.translate_node(root),
                aop: l2::ArithmeticOp::BitAndAssign,
                src: forest.translate_node(forest.child(root, 0)),
            }]
        },
    );

    // VAR1 <- VAL << NOT VAR1
    let shl_any = Tile::new(pat!(Shl(pat!(any), pat!(not)) -> res), 2, |forest, root| {
        let dst = forest.translate_node(root);
        vec![
            l2::Instruction::Assign {
                dst,
                src: forest.translate_node(forest.child(root, 0)),
            },
            l2::Instruction::Shift {
                dst,
                sop: l2::ShiftOp::ShlAssign,
                src: forest.translate_node(forest.child(root, 1)),
            },
        ]
    });

    // VAR1 <- VAR1 << VAL
    let shl_exact = Tile::new(
        pat!(Shl(pat!(exact), pat!(any)) -> res),
        1,
        |forest, root| {
            vec![l2::Instruction::Shift {
                dst: forest.translate_node(root),
                sop: l2::ShiftOp::ShlAssign,
                src: forest.translate_node(forest.child(root, 1)),
            }]
        },
    );

    // VAR1 <- VAL >> NOT VAR1
    let shr_any = Tile::new(pat!(Shr(pat!(any), pat!(not)) -> res), 2, |forest, root| {
        let dst = forest.translate_node(root);
        vec![
            l2::Instruction::Assign {
                dst,
                src: forest.translate_node(forest.child(root, 0)),
            },
            l2::Instruction::Shift {
                dst,
                sop: l2::ShiftOp::ShrAssign,
                src: forest.translate_node(forest.child(root, 1)),
            },
        ]
    });

    // VAR1 <- VAR1 >> VAL
    let shr_exact = Tile::new(
        pat!(Shr(pat!(exact), pat!(any)) -> res),
        1,
        |forest, root| {
            vec![l2::Instruction::Shift {
                dst: forest.translate_node(root),
                sop: l2::ShiftOp::ShrAssign,
                src: forest.translate_node(forest.child(root, 1)),
            }]
        },
    );

    // VAR <- VAL1 < VAL2
    let lt = Tile::new(pat!(Lt(pat!(any), pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Compare {
            dst: forest.translate_node(root),
            lhs: forest.translate_node(forest.child(root, 0)),
            cmp: l2::CompareOp::Lt,
            rhs: forest.translate_node(forest.child(root, 1)),
        }]
    });

    // VAR <- VAL1 <= VAL2
    let le = Tile::new(pat!(Le(pat!(any), pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Compare {
            dst: forest.translate_node(root),
            lhs: forest.translate_node(forest.child(root, 0)),
            cmp: l2::CompareOp::Le,
            rhs: forest.translate_node(forest.child(root, 1)),
        }]
    });

    // VAR <- VAL1 = VAL2
    let eq = Tile::new(pat!(Eq(pat!(any), pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Compare {
            dst: forest.translate_node(root),
            lhs: forest.translate_node(forest.child(root, 0)),
            cmp: l2::CompareOp::Eq,
            rhs: forest.translate_node(forest.child(root, 1)),
        }]
    });

    // VAR <- VAL1 >= VAL2
    let ge = Tile::new(pat!(Ge(pat!(any), pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Compare {
            dst: forest.translate_node(root),
            lhs: forest.translate_node(forest.child(root, 1)),
            cmp: l2::CompareOp::Le,
            rhs: forest.translate_node(forest.child(root, 0)),
        }]
    });

    // VAR <- VAL1 > VAL2
    let gt = Tile::new(pat!(Gt(pat!(any), pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Compare {
            dst: forest.translate_node(root),
            lhs: forest.translate_node(forest.child(root, 1)),
            cmp: l2::CompareOp::Lt,
            rhs: forest.translate_node(forest.child(root, 0)),
        }]
    });

    // VAR2 <- load VAR1
    let load = Tile::new(pat!(Load(pat!(var))), 1, |forest, root| {
        vec![l2::Instruction::Load {
            dst: forest.translate_node(root),
            src: forest.translate_node(forest.child(root, 0)),
            offset: 0,
        }]
    });

    // store VAR <- VAL
    let store = Tile::new(pat!(Store(pat!(var), pat!(any))), 1, |forest, root| {
        vec![l2::Instruction::Store {
            dst: forest.translate_node(forest.child(root, 0)),
            offset: 0,
            src: forest.translate_node(forest.child(root, 1)),
        }]
    });

    // return
    let return_ = Tile::new(pat!(Return), 1, |_, _| vec![l2::Instruction::Return]);

    // return VAL
    let return_value = Tile::new(pat!(ReturnValue(pat!(any))), 2, |forest, root| {
        vec![
            l2::Instruction::Assign {
                dst: l2::Value::Register(l2::Register::RAX),
                src: forest.translate_node(forest.child(root, 0)),
            },
            l2::Instruction::Return,
        ]
    });

    // br LABEL
    let branch = Tile::new(pat!(Branch(pat!(any))), 1, |forest, root| {
        let Some(Value::Label(label)) = forest.result(forest.child(root, 0)) else {
            unreachable!("pattern should have label");
        };
        vec![l2::Instruction::Goto(translate_symbol_id(*label))]
    });

    // br VAL LABEL
    let branch_cond = Tile::new(pat!(BranchCond(pat!(any), pat!(any))), 1, |forest, root| {
        let Some(Value::Label(label)) = forest.result(forest.child(root, 1)) else {
            unreachable!("pattern should have label");
        };
        vec![l2::Instruction::CJump {
            lhs: forest.translate_node(forest.child(root, 0)),
            cmp: l2::CompareOp::Eq,
            rhs: l2::Value::Number(1),
            label: translate_symbol_id(*label),
        }]
    });

    // VAR2 <- MUL 8 + VAR1
    // VAR3 <- load VAR2
    let load_offset_add_left = Tile::new(
        pat!(Load(pat!(Add(pat!(mul: 8), pat!(var))))),
        1,
        |forest, root| {
            let Some(Value::Number(offset)) = forest.result(forest.child(forest.child(root, 0), 0))
            else {
                unreachable!("pattern should have a multiple of 8")
            };
            vec![l2::Instruction::Load {
                dst: forest.translate_node(root),
                src: forest.translate_node(forest.child(forest.child(root, 0), 1)),
                offset: *offset,
            }]
        },
    );

    // VAR2 <- VAR1 + MUL 8
    // VAR3 <- load VAR2
    let load_offset_add_right = Tile::new(
        pat!(Load(pat!(Add(pat!(var), pat!(mul: 8))))),
        1,
        |forest, root| {
            let Some(Value::Number(offset)) = forest.result(forest.child(forest.child(root, 0), 1))
            else {
                unreachable!("pattern should have a multiple of 8")
            };
            vec![l2::Instruction::Load {
                dst: forest.translate_node(root),
                src: forest.translate_node(forest.child(forest.child(root, 0), 0)),
                offset: *offset,
            }]
        },
    );

    // VAR2 <- VAR1 - MUL 8
    // VAR3 <- load VAR2
    let load_offset_sub = Tile::new(
        pat!(Load(pat!(Sub(pat!(var), pat!(mul: 8))))),
        1,
        |forest, root| {
            let Some(Value::Number(offset)) = forest.result(forest.child(forest.child(root, 0), 1))
            else {
                unreachable!("pattern should have a multiple of 8")
            };
            vec![l2::Instruction::Load {
                dst: forest.translate_node(root),
                src: forest.translate_node(forest.child(forest.child(root, 0), 0)),
                offset: -*offset,
            }]
        },
    );

    // VAR2 <- MUL 8 + VAR1
    // store VAR2 <- VAL
    let store_offset_add_left = Tile::new(
        pat!(Store(pat!(Add(pat!(mul: 8), pat!(any))), pat!(any))),
        1,
        |forest, root| {
            let Some(Value::Number(offset)) = forest.result(forest.child(forest.child(root, 0), 0))
            else {
                unreachable!("pattern should have multiple of 8")
            };
            vec![l2::Instruction::Store {
                dst: forest.translate_node(forest.child(forest.child(root, 0), 1)),
                offset: *offset,
                src: forest.translate_node(forest.child(root, 1)),
            }]
        },
    );

    // VAR2 <- VAR1 + MUL 8
    // store VAR2 <- VAL
    let store_offset_add_right = Tile::new(
        pat!(Store(pat!(Add(pat!(var), pat!(mul: 8))), pat!(any))),
        1,
        |forest, root| {
            let Some(Value::Number(offset)) = forest.result(forest.child(forest.child(root, 0), 1))
            else {
                unreachable!("pattern should have multiple of 8")
            };
            vec![l2::Instruction::Store {
                dst: forest.translate_node(forest.child(forest.child(root, 0), 0)),
                offset: *offset,
                src: forest.translate_node(forest.child(root, 1)),
            }]
        },
    );

    // VAR2 <- VAR1 - MUL 8
    // store VAR2 <- VAL
    let store_offset_sub = Tile::new(
        pat!(Store(pat!(Sub(pat!(any), pat!(mul: 8))), pat!(any))),
        1,
        |forest, root| {
            let Some(Value::Number(offset)) = forest.result(forest.child(forest.child(root, 0), 1))
            else {
                unreachable!("pattern should have multiple of 8")
            };
            vec![l2::Instruction::Store {
                dst: forest.translate_node(forest.child(forest.child(root, 0), 0)),
                offset: -*offset,
                src: forest.translate_node(forest.child(root, 1)),
            }]
        },
    );

    // VAR <- VAL * POW
    let mul_by_pow_any_left = Tile::new(pat!(Mul(pat!(any), pat!(pow))), 2, |forest, root| {
        let Some(Value::Number(num)) = forest.result(forest.child(root, 1)) else {
            unreachable!("pattern should have a power of 2")
        };
        let dst = forest.translate_node(root);
        vec![
            l2::Instruction::Assign {
                dst,
                src: forest.translate_node(forest.child(root, 0)),
            },
            l2::Instruction::Shift {
                dst,
                sop: l2::ShiftOp::ShlAssign,
                src: l2::Value::Number(num.trailing_zeros().into()),
            },
        ]
    });

    // VAR <- POW * VAL
    let mul_by_pow_any_right = Tile::new(pat!(Mul(pat!(pow), pat!(any))), 2, |forest, root| {
        let Some(Value::Number(num)) = forest.result(forest.child(root, 0)) else {
            unreachable!("pattern should have a power of 2")
        };
        let dst = forest.translate_node(root);
        vec![
            l2::Instruction::Assign {
                dst,
                src: forest.translate_node(forest.child(root, 1)),
            },
            l2::Instruction::Shift {
                dst,
                sop: l2::ShiftOp::ShlAssign,
                src: l2::Value::Number(num.trailing_zeros().into()),
            },
        ]
    });

    // VAR <- VAR * POW
    let mul_by_pow_exact_left = Tile::new(
        pat!(Mul(pat!(exact), pat!(pow)) -> res),
        1,
        |forest, root| {
            let Some(Value::Number(num)) = forest.result(forest.child(root, 1)) else {
                unreachable!("pattern should have a power of 2")
            };
            vec![l2::Instruction::Shift {
                dst: forest.translate_node(root),
                sop: l2::ShiftOp::ShlAssign,
                src: l2::Value::Number(num.trailing_zeros().into()),
            }]
        },
    );

    // VAR <- POW * VAR
    let mul_by_pow_exact_right = Tile::new(
        pat!(Mul(pat!(pow), pat!(exact)) -> res),
        1,
        |forest, root| {
            let Some(Value::Number(num)) = forest.result(forest.child(root, 0)) else {
                unreachable!("pattern should have a power of 2")
            };
            vec![l2::Instruction::Shift {
                dst: forest.translate_node(root),
                sop: l2::ShiftOp::ShlAssign,
                src: l2::Value::Number(num.trailing_zeros().into()),
            }]
        },
    );

    // VAR2 <- load VAR1
    // VAR3 <- VAR2 + VAL
    // store VAR1 <- VAR3
    let store_add_load_left = Tile::new(
        pat!(Store(
            pat!(var),
            pat!(Add(pat!(Load(pat!(exact)) -> inherit), pat!(any)) -> inherit)
        ) -> res),
        1,
        |forest, root| {
            vec![l2::Instruction::StoreArithmetic {
                dst: forest.translate_node(forest.child(root, 0)),
                offset: 0,
                aop: l2::ArithmeticOp::AddAssign,
                src: forest.translate_node(forest.child(forest.child(root, 1), 1)),
            }]
        },
    );

    // VAR2 <- load VAR1
    // VAR3 <- VAL + VAR2
    // store VAR1 <- VAR3
    let store_add_load_right = Tile::new(
        pat!(Store(
            pat!(var),
            pat!(Add(pat!(any), pat!(Load(pat!(exact)) -> inherit)) -> inherit)
        ) -> res),
        1,
        |forest, root| {
            vec![l2::Instruction::StoreArithmetic {
                dst: forest.translate_node(forest.child(root, 0)),
                offset: 0,
                aop: l2::ArithmeticOp::AddAssign,
                src: forest.translate_node(forest.child(forest.child(root, 1), 0)),
            }]
        },
    );

    // VAR2 <- load VAR1
    // VAR3 <- VAR2 - VAL
    // store VAR1 <- VAR3
    let store_sub_load = Tile::new(
        pat!(Store(
            pat!(var),
            pat!(Sub(pat!(Load(pat!(exact)) -> inherit), pat!(any)) -> inherit)
        ) -> res),
        1,
        |forest, root| {
            vec![l2::Instruction::StoreArithmetic {
                dst: forest.translate_node(forest.child(root, 0)),
                offset: 0,
                aop: l2::ArithmeticOp::SubAssign,
                src: forest.translate_node(forest.child(forest.child(root, 1), 1)),
            }]
        },
    );

    // VAR2 <- load VAR1
    // VAR3 <- VAL + VAR2
    let load_add_any_left = Tile::new(
        pat!(Add(pat!(any), pat!(Load(pat!(var))))),
        2,
        |forest, root| {
            let dst = forest.translate_node(root);
            vec![
                l2::Instruction::Assign {
                    dst,
                    src: forest.translate_node(forest.child(root, 0)),
                },
                l2::Instruction::LoadArithmetic {
                    dst,
                    aop: l2::ArithmeticOp::AddAssign,
                    src: forest.translate_node(forest.child(forest.child(root, 1), 0)),
                    offset: 0,
                },
            ]
        },
    );

    // VAR2 <- load VAR1
    // VAR3 <- VAR2 + VAL
    let load_add_any_right = Tile::new(
        pat!(Add(pat!(Load(pat!(var))), pat!(any))),
        2,
        |forest, root| {
            let dst = forest.translate_node(root);
            vec![
                l2::Instruction::Assign {
                    dst,
                    src: forest.translate_node(forest.child(root, 1)),
                },
                l2::Instruction::LoadArithmetic {
                    dst,
                    aop: l2::ArithmeticOp::AddAssign,
                    src: forest.translate_node(forest.child(forest.child(root, 0), 0)),
                    offset: 0,
                },
            ]
        },
    );

    // VAR2 <- load VAR1
    // VAR3 <- VAR3 + VAR2
    let load_add_exact_left = Tile::new(
        pat!(Add(pat!(exact), pat!(Load(pat!(var)))) -> res),
        1,
        |forest, root| {
            vec![l2::Instruction::LoadArithmetic {
                dst: forest.translate_node(root),
                aop: l2::ArithmeticOp::AddAssign,
                src: forest.translate_node(forest.child(forest.child(root, 1), 0)),
                offset: 0,
            }]
        },
    );

    // VAR2 <- load VAR1
    // VAR3 <- VAR2 + VAR3
    let load_add_exact_right = Tile::new(
        pat!(Add(pat!(Load(pat!(var))), pat!(exact)) -> res),
        1,
        |forest, root| {
            vec![l2::Instruction::LoadArithmetic {
                dst: forest.translate_node(root),
                aop: l2::ArithmeticOp::AddAssign,
                src: forest.translate_node(forest.child(forest.child(root, 0), 0)),
                offset: 0,
            }]
        },
    );

    // VAR2 <- load VAR1
    // VAR3 <- VAL - VAR2
    let load_sub_any = Tile::new(
        pat!(Sub(pat!(any), pat!(Load(pat!(var))))),
        2,
        |forest, root| {
            let dst = forest.translate_node(root);
            vec![
                l2::Instruction::Assign {
                    dst,
                    src: forest.translate_node(forest.child(root, 0)),
                },
                l2::Instruction::LoadArithmetic {
                    dst,
                    aop: l2::ArithmeticOp::SubAssign,
                    src: forest.translate_node(forest.child(forest.child(root, 1), 0)),
                    offset: 0,
                },
            ]
        },
    );

    // VAR2 <- load VAR1
    // VAR3 <- VAR3 - VAR2
    let load_sub_exact = Tile::new(
        pat!(Sub(pat!(exact), pat!(Load(pat!(var)))) -> res),
        1,
        |forest, root| {
            vec![l2::Instruction::LoadArithmetic {
                dst: forest.translate_node(root),
                aop: l2::ArithmeticOp::SubAssign,
                src: forest.translate_node(forest.child(forest.child(root, 1), 0)),
                offset: 0,
            }]
        },
    );

    // VAR2 <- MUL 8 + VAR1
    // VAR3 <- load VAR2
    // VAR4 <- VAL + VAR3
    let load_add_offset_add_left_any_left = Tile::new(
        pat!(Add(
            pat!(any),
            pat!(Load(pat!(Add(pat!(mul: 8), pat!(var)))))
        )),
        2,
        |forest, root| {
            let dst = forest.translate_node(root);
            let Some(Value::Number(offset)) =
                forest.result(forest.child(forest.child(forest.child(root, 1), 0), 0))
            else {
                unreachable!("pattern should have multiple of 8")
            };
            vec![
                l2::Instruction::Assign {
                    dst,
                    src: forest.translate_node(forest.child(root, 0)),
                },
                l2::Instruction::LoadArithmetic {
                    dst,
                    aop: l2::ArithmeticOp::AddAssign,
                    src: forest
                        .translate_node(forest.child(forest.child(forest.child(root, 1), 0), 1)),
                    offset: *offset,
                },
            ]
        },
    );

    // VAR2 <- MUL 8 + VAR1
    // VAR3 <- load VAR2
    // VAR4 <- VAR3 + VAL
    let load_add_offset_add_left_any_right = Tile::new(
        pat!(Add(
            pat!(Load(pat!(Add(pat!(mul: 8), pat!(var))))),
            pat!(any)
        )),
        2,
        |forest, root| {
            let dst = forest.translate_node(root);
            let Some(Value::Number(offset)) =
                forest.result(forest.child(forest.child(forest.child(root, 0), 0), 0))
            else {
                unreachable!("pattern should have multiple of 8")
            };
            vec![
                l2::Instruction::Assign {
                    dst,
                    src: forest.translate_node(forest.child(root, 0)),
                },
                l2::Instruction::LoadArithmetic {
                    dst,
                    aop: l2::ArithmeticOp::AddAssign,
                    src: forest
                        .translate_node(forest.child(forest.child(forest.child(root, 0), 0), 1)),
                    offset: *offset,
                },
            ]
        },
    );

    // VAR2 <- MUL 8 + VAR1
    // VAR3 <- load VAR2
    // VAR4 <- VAR4 + VAR3
    let load_add_offset_add_left_exact_left = Tile::new(
        pat!(Add(pat!(exact), pat!(Load(pat!(Add(pat!(mul: 8), pat!(var)))))) -> res),
        1,
        |forest, root| {
            let Some(Value::Number(offset)) =
                forest.result(forest.child(forest.child(forest.child(root, 1), 0), 0))
            else {
                unreachable!("pattern should have multiple of 8")
            };
            vec![l2::Instruction::LoadArithmetic {
                dst: forest.translate_node(root),
                aop: l2::ArithmeticOp::AddAssign,
                src: forest.translate_node(forest.child(forest.child(forest.child(root, 1), 0), 1)),
                offset: *offset,
            }]
        },
    );

    // VAR2 <- MUL 8 + VAR1
    // VAR3 <- load VAR2
    // VAR4 <- VAR3 + VAR4
    let load_add_offset_add_left_exact_right = Tile::new(
        pat!(Add(pat!(Load(pat!(Add(pat!(mul: 8), pat!(var))))), pat!(exact)) -> res),
        1,
        |forest, root| {
            let Some(Value::Number(offset)) =
                forest.result(forest.child(forest.child(forest.child(root, 0), 0), 0))
            else {
                unreachable!("pattern should have multiple of 8")
            };
            vec![l2::Instruction::LoadArithmetic {
                dst: forest.translate_node(root),
                aop: l2::ArithmeticOp::AddAssign,
                src: forest.translate_node(forest.child(forest.child(forest.child(root, 0), 0), 1)),
                offset: *offset,
            }]
        },
    );

    // VAR2 <- VAR1 + MUL 8
    // VAR3 <- load VAR2
    // VAR4 <- VAL + VAR3
    let load_add_offset_add_right_any_left = Tile::new(
        pat!(Add(
            pat!(any),
            pat!(Load(pat!(Add(pat!(var), pat!(mul: 8)))))
        )),
        2,
        |forest, root| {
            let dst = forest.translate_node(root);
            let Some(Value::Number(offset)) =
                forest.result(forest.child(forest.child(forest.child(root, 1), 0), 1))
            else {
                unreachable!("pattern should have multiple of 8")
            };
            vec![
                l2::Instruction::Assign {
                    dst,
                    src: forest.translate_node(forest.child(root, 0)),
                },
                l2::Instruction::LoadArithmetic {
                    dst,
                    aop: l2::ArithmeticOp::AddAssign,
                    src: forest
                        .translate_node(forest.child(forest.child(forest.child(root, 1), 0), 0)),
                    offset: *offset,
                },
            ]
        },
    );

    // VAR2 <- VAR1 + MUL 8
    // VAR3 <- load VAR2
    // VAR4 <- VAR3 + VAL
    let load_add_offset_add_right_any_right = Tile::new(
        pat!(Add(
            pat!(Load(pat!(Add(pat!(var), pat!(mul: 8))))),
            pat!(any)
        )),
        2,
        |forest, root| {
            let dst = forest.translate_node(root);
            let Some(Value::Number(offset)) =
                forest.result(forest.child(forest.child(forest.child(root, 0), 0), 1))
            else {
                unreachable!("pattern should have multiple of 8")
            };
            vec![
                l2::Instruction::Assign {
                    dst,
                    src: forest.translate_node(forest.child(root, 1)),
                },
                l2::Instruction::LoadArithmetic {
                    dst,
                    aop: l2::ArithmeticOp::AddAssign,
                    src: forest
                        .translate_node(forest.child(forest.child(forest.child(root, 0), 0), 0)),
                    offset: *offset,
                },
            ]
        },
    );

    // VAR2 <- VAR1 + MUL 8
    // VAR3 <- load VAR2
    // VAR4 <- VAR4 + VAR3
    let load_add_offset_add_right_exact_left = Tile::new(
        pat!(Add(pat!(exact), pat!(Load(pat!(Add(pat!(var), pat!(mul: 8)))))) -> res),
        1,
        |forest, root| {
            let Some(Value::Number(offset)) =
                forest.result(forest.child(forest.child(forest.child(root, 1), 0), 1))
            else {
                unreachable!("pattern should have multiple of 8")
            };
            vec![l2::Instruction::LoadArithmetic {
                dst: forest.translate_node(root),
                aop: l2::ArithmeticOp::AddAssign,
                src: forest.translate_node(forest.child(forest.child(forest.child(root, 1), 0), 0)),
                offset: *offset,
            }]
        },
    );

    // VAR2 <- VAR1 + MUL 8
    // VAR3 <- load VAR2
    // VAR4 <- VAR3 + VAR4
    let load_add_offset_add_right_exact_right = Tile::new(
        pat!(Add(pat!(Load(pat!(Add(pat!(var), pat!(mul: 8))))), pat!(exact)) -> res),
        1,
        |forest, root| {
            let Some(Value::Number(offset)) =
                forest.result(forest.child(forest.child(forest.child(root, 0), 0), 1))
            else {
                unreachable!("pattern should have multiple of 8")
            };
            vec![l2::Instruction::LoadArithmetic {
                dst: forest.translate_node(root),
                aop: l2::ArithmeticOp::AddAssign,
                src: forest.translate_node(forest.child(forest.child(forest.child(root, 0), 0), 0)),
                offset: *offset,
            }]
        },
    );

    // VAR2 <- VAR1 - MUL 8
    // VAR3 <- load VAR2
    // VAR4 <- VAL + VAR3
    let load_add_offset_sub_any_left = Tile::new(
        pat!(Add(
            pat!(any),
            pat!(Load(pat!(Sub(pat!(var), pat!(mul: 8)))))
        )),
        2,
        |forest, root| {
            let dst = forest.translate_node(root);
            let Some(Value::Number(offset)) =
                forest.result(forest.child(forest.child(forest.child(root, 1), 0), 1))
            else {
                unreachable!("pattern should have multiple of 8")
            };
            vec![
                l2::Instruction::Assign {
                    dst,
                    src: forest.translate_node(forest.child(root, 0)),
                },
                l2::Instruction::LoadArithmetic {
                    dst,
                    aop: l2::ArithmeticOp::AddAssign,
                    src: forest
                        .translate_node(forest.child(forest.child(forest.child(root, 1), 0), 0)),
                    offset: -*offset,
                },
            ]
        },
    );

    // VAR2 <- VAR1 - MUL 8
    // VAR3 <- load VAR2
    // VAR4 <- VAR3 + VAL
    let load_add_offset_sub_any_right = Tile::new(
        pat!(Add(
            pat!(Load(pat!(Sub(pat!(var), pat!(mul: 8))))),
            pat!(any)
        )),
        2,
        |forest, root| {
            let dst = forest.translate_node(root);
            let Some(Value::Number(offset)) =
                forest.result(forest.child(forest.child(forest.child(root, 0), 0), 1))
            else {
                unreachable!("pattern should have multiple of 8")
            };
            vec![
                l2::Instruction::Assign {
                    dst,
                    src: forest.translate_node(forest.child(root, 1)),
                },
                l2::Instruction::LoadArithmetic {
                    dst,
                    aop: l2::ArithmeticOp::AddAssign,
                    src: forest
                        .translate_node(forest.child(forest.child(forest.child(root, 0), 0), 0)),
                    offset: -*offset,
                },
            ]
        },
    );

    // VAR2 <- VAR1 - MUL 8
    // VAR3 <- load VAR2
    // VAR4 <- VAR4 + VAR3
    let load_add_offset_sub_exact_left = Tile::new(
        pat!(Add(pat!(exact), pat!(Load(pat!(Sub(pat!(var), pat!(mul: 8)))))) -> res),
        1,
        |forest, root| {
            let Some(Value::Number(offset)) =
                forest.result(forest.child(forest.child(forest.child(root, 1), 0), 1))
            else {
                unreachable!("pattern should have multiple of 8")
            };
            vec![l2::Instruction::LoadArithmetic {
                dst: forest.translate_node(root),
                aop: l2::ArithmeticOp::AddAssign,
                src: forest.translate_node(forest.child(forest.child(forest.child(root, 1), 0), 0)),
                offset: -*offset,
            }]
        },
    );

    // VAR2 <- VAR1 - MUL 8
    // VAR3 <- load VAR2
    // VAR4 <- VAR3 + VAR4
    let load_add_offset_sub_exact_right = Tile::new(
        pat!(Add(pat!(Load(pat!(Sub(pat!(var), pat!(mul: 8))))), pat!(exact)) -> res),
        1,
        |forest, root| {
            let Some(Value::Number(offset)) =
                forest.result(forest.child(forest.child(forest.child(root, 0), 0), 1))
            else {
                unreachable!("pattern should have multiple of 8")
            };
            vec![l2::Instruction::LoadArithmetic {
                dst: forest.translate_node(root),
                aop: l2::ArithmeticOp::AddAssign,
                src: forest.translate_node(forest.child(forest.child(forest.child(root, 0), 0), 0)),
                offset: -*offset,
            }]
        },
    );

    // VAR2 <- MUL 8 + VAR1
    // VAR3 <- load VAR2
    // VAR4 <- VAL - VAR3
    let load_sub_offset_add_left_any = Tile::new(
        pat!(Sub(
            pat!(any),
            pat!(Load(pat!(Add(pat!(mul: 8), pat!(var)))))
        )),
        2,
        |forest, root| {
            let dst = forest.translate_node(root);
            let Some(Value::Number(offset)) =
                forest.result(forest.child(forest.child(forest.child(root, 1), 0), 0))
            else {
                unreachable!("pattern should have multiple of 8")
            };
            vec![
                l2::Instruction::Assign {
                    dst,
                    src: forest.translate_node(forest.child(root, 0)),
                },
                l2::Instruction::LoadArithmetic {
                    dst,
                    aop: l2::ArithmeticOp::SubAssign,
                    src: forest
                        .translate_node(forest.child(forest.child(forest.child(root, 1), 0), 1)),
                    offset: *offset,
                },
            ]
        },
    );

    // VAR2 <- MUL 8 + VAR1
    // VAR3 <- load VAR2
    // VAR4 <- VAR4 - VAR3
    let load_sub_offset_add_left_exact = Tile::new(
        pat!(Sub(pat!(exact), pat!(Load(pat!(Add(pat!(mul: 8), pat!(var)))))) -> res),
        1,
        |forest, root| {
            let Some(Value::Number(offset)) =
                forest.result(forest.child(forest.child(forest.child(root, 1), 0), 0))
            else {
                unreachable!("pattern should have multiple of 8")
            };
            vec![l2::Instruction::LoadArithmetic {
                dst: forest.translate_node(root),
                aop: l2::ArithmeticOp::SubAssign,
                src: forest.translate_node(forest.child(forest.child(forest.child(root, 1), 0), 1)),
                offset: *offset,
            }]
        },
    );

    // VAR2 <- VAR1 + MUL 8
    // VAR3 <- load VAR2
    // VAR4 <- VAL - VAR3
    let load_sub_offset_add_right_any = Tile::new(
        pat!(Sub(
            pat!(any),
            pat!(Load(pat!(Add(pat!(var), pat!(mul: 8)))))
        )),
        2,
        |forest, root| {
            let dst = forest.translate_node(root);
            let Some(Value::Number(offset)) =
                forest.result(forest.child(forest.child(forest.child(root, 1), 0), 1))
            else {
                unreachable!("pattern should have multiple of 8")
            };
            vec![
                l2::Instruction::Assign {
                    dst,
                    src: forest.translate_node(forest.child(root, 0)),
                },
                l2::Instruction::LoadArithmetic {
                    dst,
                    aop: l2::ArithmeticOp::SubAssign,
                    src: forest
                        .translate_node(forest.child(forest.child(forest.child(root, 1), 0), 0)),
                    offset: *offset,
                },
            ]
        },
    );

    // VAR2 <- VAR1 + MUL 8
    // VAR3 <- load VAR2
    // VAR4 <- VAR4 - VAR3
    let load_sub_offset_add_right_exact = Tile::new(
        pat!(Add(pat!(exact), pat!(Load(pat!(Add(pat!(var), pat!(mul: 8)))))) -> res),
        1,
        |forest, root| {
            let Some(Value::Number(offset)) =
                forest.result(forest.child(forest.child(forest.child(root, 1), 0), 1))
            else {
                unreachable!("pattern should have multiple of 8")
            };
            vec![l2::Instruction::LoadArithmetic {
                dst: forest.translate_node(root),
                aop: l2::ArithmeticOp::SubAssign,
                src: forest.translate_node(forest.child(forest.child(forest.child(root, 1), 0), 0)),
                offset: *offset,
            }]
        },
    );

    // VAR2 <- VAR1 - MUL 8
    // VAR3 <- load VAR2
    // VAR4 <- VAL - VAR3
    let load_sub_offset_sub_any = Tile::new(
        pat!(Sub(
            pat!(any),
            pat!(Load(pat!(Sub(pat!(var), pat!(mul: 8)))))
        )),
        2,
        |forest, root| {
            let dst = forest.translate_node(root);
            let Some(Value::Number(offset)) =
                forest.result(forest.child(forest.child(forest.child(root, 1), 0), 1))
            else {
                unreachable!("pattern should have multiple of 8")
            };
            vec![
                l2::Instruction::Assign {
                    dst,
                    src: forest.translate_node(forest.child(root, 0)),
                },
                l2::Instruction::LoadArithmetic {
                    dst,
                    aop: l2::ArithmeticOp::SubAssign,
                    src: forest
                        .translate_node(forest.child(forest.child(forest.child(root, 1), 0), 0)),
                    offset: -*offset,
                },
            ]
        },
    );

    // VAR2 <- VAR1 - MUL 8
    // VAR3 <- load VAR2
    // VAR4 <- VAR4 - VAR3
    let load_sub_offset_sub_exact = Tile::new(
        pat!(Sub(pat!(exact), pat!(Load(pat!(Sub(pat!(var), pat!(mul: 8)))))) -> res),
        1,
        |forest, root| {
            let Some(Value::Number(offset)) =
                forest.result(forest.child(forest.child(forest.child(root, 1), 0), 1))
            else {
                unreachable!("pattern should have multiple of 8")
            };
            vec![l2::Instruction::LoadArithmetic {
                dst: forest.translate_node(root),
                aop: l2::ArithmeticOp::SubAssign,
                src: forest.translate_node(forest.child(forest.child(forest.child(root, 1), 0), 0)),
                offset: -*offset,
            }]
        },
    );

    // VAR <- VAL1 < VAL2
    // br VAR LABEL
    let branch_cond_lt = Tile::new(
        pat!(BranchCond(pat!(Lt(pat!(any), pat!(any))), pat!(any))),
        1,
        |forest, root| {
            let Some(Value::Label(label)) = forest.result(forest.child(root, 1)) else {
                unreachable!("pattern should have label");
            };
            vec![l2::Instruction::CJump {
                lhs: forest.translate_node(forest.child(forest.child(root, 0), 0)),
                cmp: l2::CompareOp::Lt,
                rhs: forest.translate_node(forest.child(forest.child(root, 0), 1)),
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
                unreachable!("pattern should have label");
            };
            vec![l2::Instruction::CJump {
                lhs: forest.translate_node(forest.child(forest.child(root, 0), 0)),
                cmp: l2::CompareOp::Le,
                rhs: forest.translate_node(forest.child(forest.child(root, 0), 1)),
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
                unreachable!("pattern should have label");
            };
            vec![l2::Instruction::CJump {
                lhs: forest.translate_node(forest.child(forest.child(root, 0), 0)),
                cmp: l2::CompareOp::Eq,
                rhs: forest.translate_node(forest.child(forest.child(root, 0), 1)),
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
                unreachable!("pattern should have label");
            };
            vec![l2::Instruction::CJump {
                lhs: forest.translate_node(forest.child(forest.child(root, 0), 1)),
                cmp: l2::CompareOp::Le,
                rhs: forest.translate_node(forest.child(forest.child(root, 0), 0)),
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
                unreachable!("pattern should have label");
            };
            vec![l2::Instruction::CJump {
                lhs: forest.translate_node(forest.child(forest.child(root, 0), 1)),
                cmp: l2::CompareOp::Lt,
                rhs: forest.translate_node(forest.child(forest.child(root, 0), 0)),
                label: translate_symbol_id(*label),
            }]
        },
    );

    // VAR <- VAL + 1
    let increment_any_left = Tile::new(pat!(Add(pat!(any), pat!(num: 1))), 2, |forest, root| {
        let dst = forest.translate_node(root);
        vec![
            l2::Instruction::Assign {
                dst,
                src: forest.translate_node(forest.child(root, 0)),
            },
            l2::Instruction::Increment(dst),
        ]
    });

    // VAR <- 1 + VAL
    let increment_any_right = Tile::new(pat!(Add(pat!(num: 1), pat!(any))), 2, |forest, root| {
        let dst = forest.translate_node(root);
        vec![
            l2::Instruction::Assign {
                dst,
                src: forest.translate_node(forest.child(root, 1)),
            },
            l2::Instruction::Increment(dst),
        ]
    });

    // VAR <- VAR + 1
    let increment_exact_left = Tile::new(
        pat!(Add(pat!(exact), pat!(num: 1)) -> res),
        1,
        |forest, root| vec![l2::Instruction::Increment(forest.translate_node(root))],
    );

    // VAR <- 1 + VAR
    let increment_exact_right = Tile::new(
        pat!(Add(pat!(num: 1), pat!(exact)) -> res),
        1,
        |forest, root| vec![l2::Instruction::Increment(forest.translate_node(root))],
    );

    // VAR <- VAL - 1
    let decrement_any = Tile::new(pat!(Sub(pat!(any), pat!(num: 1))), 2, |forest, root| {
        let dst = forest.translate_node(root);
        vec![
            l2::Instruction::Assign {
                dst,
                src: forest.translate_node(forest.child(root, 0)),
            },
            l2::Instruction::Decrement(dst),
        ]
    });

    // VAR <- VAR - 1
    let decrement_exact_left = Tile::new(
        pat!(Sub(pat!(exact), pat!(num: 1)) -> res),
        1,
        |forest, root| vec![l2::Instruction::Decrement(forest.translate_node(root))],
    );

    // VAR <- 1 - VAR
    let decrement_exact_right = Tile::new(
        pat!(Sub(pat!(num: 1), pat!(exact)) -> res),
        2,
        |forest, root| {
            let dst = forest.translate_node(root);
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

    // VAR2 <- NUM 1 | 2 | 4 | 8 * VAR1
    // VAR4 <- VAR2 + VAR3
    let lea_scale_left_left = Tile::new(
        pat!(Add(pat!(Mul(pat!(scale), pat!(var))), pat!(var))),
        1,
        |forest, root| {
            let Some(Value::Number(num)) = forest.result(forest.child(forest.child(root, 0), 0))
            else {
                unreachable!("pattern should have 1, 2, 4, or 8")
            };
            {
                vec![l2::Instruction::LEA {
                    dst: forest.translate_node(root),
                    src: forest.translate_node(forest.child(root, 1)),
                    offset: forest.translate_node(forest.child(forest.child(root, 0), 1)),
                    scale: *num as u8,
                }]
            }
        },
    );

    // VAR2 <- NUM 1 | 2 | 4 | 8 * VAR1
    // VAR4 <- VAR3 + VAR2
    let lea_scale_left_right = Tile::new(
        pat!(Add(pat!(var), pat!(Mul(pat!(scale), pat!(var))))),
        1,
        |forest, root| {
            let Some(Value::Number(num)) = forest.result(forest.child(forest.child(root, 1), 0))
            else {
                unreachable!("pattern should have 1, 2, 4, or 8")
            };
            {
                vec![l2::Instruction::LEA {
                    dst: forest.translate_node(root),
                    src: forest.translate_node(forest.child(root, 0)),
                    offset: forest.translate_node(forest.child(forest.child(root, 1), 1)),
                    scale: *num as u8,
                }]
            }
        },
    );

    // VAR2 <- VAR1 * NUM 1 | 2 | 4 | 8
    // VAR4 <- VAR2 + VAR3
    let lea_scale_right_left = Tile::new(
        pat!(Add(pat!(Mul(pat!(var), pat!(scale))), pat!(var))),
        1,
        |forest, root| {
            let Some(Value::Number(num)) = forest.result(forest.child(forest.child(root, 0), 1))
            else {
                unreachable!("pattern should have 1, 2, 4, or 8")
            };
            {
                vec![l2::Instruction::LEA {
                    dst: forest.translate_node(root),
                    src: forest.translate_node(forest.child(root, 1)),
                    offset: forest.translate_node(forest.child(forest.child(root, 0), 0)),
                    scale: *num as u8,
                }]
            }
        },
    );

    // VAR2 <- VAR1 * NUM 1 | 2 | 4 | 8
    // VAR4 <- VAL3 + VAR2
    let lea_scale_right_right = Tile::new(
        pat!(Add(pat!(var), pat!(Mul(pat!(var), pat!(scale))))),
        1,
        |forest, root| {
            let Some(Value::Number(num)) = forest.result(forest.child(forest.child(root, 1), 1))
            else {
                unreachable!("pattern should have 1, 2, 4, or 8")
            };
            {
                vec![l2::Instruction::LEA {
                    dst: forest.translate_node(root),
                    src: forest.translate_node(forest.child(root, 0)),
                    offset: forest.translate_node(forest.child(forest.child(root, 1), 0)),
                    scale: *num as u8,
                }]
            }
        },
    );

    vec![
        mul_by_pow_any_left,
        mul_by_pow_any_right,
        mul_by_pow_exact_left,
        mul_by_pow_exact_right,
        increment_any_left,
        increment_any_right,
        increment_exact_left,
        increment_exact_right,
        decrement_any,
        decrement_exact_left,
        decrement_exact_right,
        assign,
        add_any,
        add_exact_left,
        add_exact_right,
        sub_any,
        sub_exact_left,
        sub_exact_right,
        mul_any,
        mul_exact_left,
        mul_exact_right,
        bit_and_any,
        bit_and_exact_left,
        bit_and_exact_right,
        shl_any,
        shl_exact,
        shr_any,
        shr_exact,
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
        load_offset_add_left,
        load_offset_add_right,
        load_offset_sub,
        store_offset_add_left,
        store_offset_add_right,
        store_offset_sub,
        store_add_load_left,
        store_add_load_right,
        store_sub_load,
        load_add_any_left,
        load_add_any_right,
        load_add_exact_left,
        load_add_exact_right,
        load_sub_any,
        load_sub_exact,
        load_add_offset_add_left_any_left,
        load_add_offset_add_left_any_right,
        load_add_offset_add_left_exact_left,
        load_add_offset_add_left_exact_right,
        load_add_offset_add_right_any_left,
        load_add_offset_add_right_any_right,
        load_add_offset_add_right_exact_left,
        load_add_offset_add_right_exact_right,
        load_add_offset_sub_any_left,
        load_add_offset_sub_any_right,
        load_add_offset_sub_exact_left,
        load_add_offset_sub_exact_right,
        load_sub_offset_add_left_any,
        load_sub_offset_add_left_exact,
        load_sub_offset_add_right_any,
        load_sub_offset_add_right_exact,
        load_sub_offset_sub_any,
        load_sub_offset_sub_exact,
        branch_cond_lt,
        branch_cond_le,
        branch_cond_eq,
        branch_cond_ge,
        branch_cond_gt,
        lea_scale_left_left,
        lea_scale_left_right,
        lea_scale_right_left,
        lea_scale_right_right,
    ]
}
