use ir::*;
use utils::{BitVector, Worklist};

#[derive(Debug)]
pub struct DominatorTree {
    idom: Vec<Option<BlockId>>,
    children: Vec<BitVector>,
    preorder: Vec<u32>,
    postorder: Vec<u32>,
}

impl DominatorTree {
    pub fn new(func: &Function) -> Self {
        let num_blocks = func.basic_blocks.len();
        let mut sdom = vec![BitVector::new(num_blocks); num_blocks];
        for i in 0..num_blocks {
            sdom[i].set_from(0..num_blocks);
        }

        let entry_id = BlockId(0);
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

        let idom: Vec<Option<BlockId>> = sdom
            .iter()
            .map(|dom| dom.iter().max_by_key(|&n| sdom[n].count()).map(BlockId))
            .collect();

        let mut children = vec![BitVector::new(num_blocks); num_blocks];
        for (node, parent) in idom.iter().enumerate() {
            if let Some(parent) = parent {
                children[parent.0].set(node);
            }
        }

        let mut counter = 0;
        let mut preorder = vec![0; num_blocks];
        let mut postorder = vec![0; num_blocks];

        fn dfs(
            node: BlockId,
            children: &[BitVector],
            counter: &mut u32,
            preorder: &mut [u32],
            postorder: &mut [u32],
        ) {
            let i = node.0;
            preorder[i] = *counter;
            *counter += 1;

            for child in &children[i] {
                dfs(BlockId(child), children, counter, preorder, postorder);
            }

            postorder[i] = *counter;
            *counter += 1;
        }

        dfs(
            entry_id,
            &children,
            &mut counter,
            &mut preorder,
            &mut postorder,
        );

        Self {
            idom,
            children,
            preorder,
            postorder,
        }
    }

    pub fn dominates(&self, u: BlockId, v: BlockId) -> bool {
        self.preorder[u.0] <= self.preorder[v.0] && self.postorder[u.0] >= self.postorder[v.0]
    }

    pub fn children(&self, node: BlockId) -> impl Iterator<Item = BlockId> {
        self.children[node.0].iter().map(BlockId)
    }
}

#[derive(Debug)]
pub struct DominanceFrontier {
    pub frontier: Vec<BitVector>,
}

impl<'a> DominanceFrontier {
    pub fn new(func: &Function, dom_tree: &'a DominatorTree) -> Self {
        let num_blocks = func.basic_blocks.len();

        let mut local_frontier = vec![BitVector::new(num_blocks); num_blocks];
        for i in 0..num_blocks {
            for &succ in &func.cfg.successors[i] {
                if i == succ.0 || !dom_tree.dominates(BlockId(i), succ) {
                    local_frontier[i].set(succ.0);
                }
            }
        }

        let mut frontier = vec![BitVector::new(num_blocks); num_blocks];

        fn dfs(
            dom_tree: &DominatorTree,
            node: BlockId,
            local_frontier: &[BitVector],
            frontier: &mut [BitVector],
        ) {
            let i = node.0;
            frontier[i] = local_frontier[i].clone();

            for child in &dom_tree.children[i] {
                dfs(dom_tree, BlockId(child), local_frontier, frontier);

                if dom_tree.idom[child] != Some(node) {
                    if i < child {
                        let (left, right) = frontier.split_at_mut(child);
                        left[i].set_from(right[0].iter());
                    } else {
                        let (left, right) = frontier.split_at_mut(i);
                        right[0].set_from(left[child].iter());
                    }
                }
            }
        }

        dfs(dom_tree, BlockId(0), &local_frontier, &mut frontier);

        Self { frontier }
    }
}
