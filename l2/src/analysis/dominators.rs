use l2::*;
use utils::{BitVector, Worklist};

#[derive(Debug)]
pub struct DominatorTree {
    preorder: Vec<u32>,
    postorder: Vec<u32>,
}

impl DominatorTree {
    pub fn new(func: &Function) -> Self {
        let num_blocks = func.basic_blocks.len();
        let entry_id = BlockId(0);

        let mut sdom = vec![BitVector::new(num_blocks); num_blocks];
        for i in 0..num_blocks {
            sdom[i].set_from(0..num_blocks);
        }

        let mut worklist = Worklist::new();
        worklist.push(entry_id);

        while let Some(id) = worklist.pop() {
            let i = id.0;
            let mut temp = BitVector::new(num_blocks);

            if i != entry_id.0 {
                temp.set_from(0..num_blocks);
                for pred in &func.cfg.predecessors[i] {
                    temp.intersection(&sdom[pred.0]);
                }
            }

            temp.set(i);

            if temp != sdom[i] {
                sdom[i] = temp;
                worklist.extend(func.cfg.successors[i].iter().copied());
            }
        }

        for i in 0..num_blocks {
            sdom[i].reset(i);
        }

        let idom: Vec<Option<usize>> = sdom
            .iter()
            .map(|dom| dom.iter().max_by_key(|&n| sdom[n].count()))
            .collect();

        let mut children = vec![BitVector::new(num_blocks); num_blocks];
        for (node, &parent) in idom.iter().enumerate() {
            if let Some(parent) = parent {
                children[parent].set(node);
            }
        }

        let mut counter = 0;
        let mut preorder = vec![0; num_blocks];
        let mut postorder = vec![0; num_blocks];

        fn dfs(
            node: usize,
            tree: &[BitVector],
            counter: &mut u32,
            preorder: &mut [u32],
            postorder: &mut [u32],
        ) {
            preorder[node] = *counter;
            *counter += 1;

            for child in &tree[node] {
                dfs(child, tree, counter, preorder, postorder);
            }

            postorder[node] = *counter;
            *counter += 1;
        }

        dfs(
            entry_id.0,
            &children,
            &mut counter,
            &mut preorder,
            &mut postorder,
        );

        Self {
            preorder,
            postorder,
        }
    }

    pub fn dominates(&self, u: BlockId, v: BlockId) -> bool {
        self.preorder[u.0] <= self.preorder[v.0] && self.postorder[u.0] >= self.postorder[v.0]
    }
}
