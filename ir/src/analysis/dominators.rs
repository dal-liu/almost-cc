use ir::*;
use utils::{BitVector, Interner, Worklist};

type BlockId = usize;

#[derive(Debug)]
pub struct DominatorTree {
    interner: Interner<SymbolId>,
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

        let interner = func
            .basic_blocks
            .iter()
            .fold(Interner::new(), |mut interner, block| {
                interner.intern(block.label);
                interner
            });

        let entry_label = func.basic_blocks[0].label;
        let mut worklist = Worklist::new();
        worklist.push(entry_label);

        while let Some(label) = worklist.pop() {
            let mut temp = BitVector::new(num_blocks);

            if label != entry_label {
                temp.set_from(0..num_blocks);
                for pred in func.cfg.predecessors(label) {
                    temp.intersection(&sdom[interner[&pred]]);
                }
            }

            let i = interner[&label];
            temp.set(i);

            if temp != sdom[i] {
                sdom[i] = temp;
                worklist.extend(func.cfg.successors(label));
            }
        }

        for i in 0..num_blocks {
            sdom[i].reset(i);
        }

        let idom: Vec<Option<BlockId>> = sdom
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
            node: BlockId,
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
            interner[&entry_label],
            &children,
            &mut counter,
            &mut preorder,
            &mut postorder,
        );

        Self {
            interner,
            idom,
            children,
            preorder,
            postorder,
        }
    }

    pub fn dominates(&self, u: SymbolId, v: SymbolId) -> bool {
        let u = self.interner[&u];
        let v = self.interner[&v];
        self.preorder[u] <= self.preorder[v] && self.postorder[u] >= self.postorder[v]
    }
}

#[derive(Debug)]
pub struct DominanceFrontier<'a> {
    pub interner: &'a Interner<SymbolId>,
    pub frontier: Vec<BitVector>,
}

impl<'a> DominanceFrontier<'a> {
    pub fn new(func: &Function, dom_tree: &'a DominatorTree) -> Self {
        let interner = &dom_tree.interner;
        let num_blocks = func.basic_blocks.len();

        let mut local_frontier = vec![BitVector::new(num_blocks); num_blocks];
        for (i, block) in func.basic_blocks.iter().enumerate() {
            for succ in func.cfg.successors(block.label) {
                if block.label == succ || !dom_tree.dominates(block.label, succ) {
                    local_frontier[i].set(interner[&succ]);
                }
            }
        }

        let mut frontier = vec![BitVector::new(num_blocks); num_blocks];

        fn dfs(
            dom_tree: &DominatorTree,
            id: BlockId,
            local_frontier: &[BitVector],
            frontier: &mut [BitVector],
        ) {
            frontier[id] = local_frontier[id].clone();

            for child in &dom_tree.children[id] {
                dfs(dom_tree, child, local_frontier, frontier);

                if dom_tree.idom[child] != Some(id) {
                    if id < child {
                        let (left, right) = frontier.split_at_mut(child);
                        left[id].set_from(right[0].iter());
                    } else {
                        let (left, right) = frontier.split_at_mut(id);
                        right[0].set_from(left[child].iter());
                    }
                }
            }
        }

        dfs(dom_tree, 0, &local_frontier, &mut frontier);

        Self { interner, frontier }
    }
}
