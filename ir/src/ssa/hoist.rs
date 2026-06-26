use ir::*;

pub fn hoist_define_instructions(prog: &mut Program) {
    for func in &mut prog.functions {
        if let Some(instructions) = func.basic_blocks.first_mut().map(|block| {
            block
                .instructions
                .extract_if(.., |inst| matches!(inst, Instruction::Define { .. }))
                .collect()
        }) {
            func.basic_blocks.insert(
                0,
                BasicBlock {
                    label: SymbolId(prog.interner.intern(String::from("_entry_"))),
                    instructions,
                    terminator: Instruction::Branch(func.basic_blocks[0].label),
                },
            );
            func.cfg = ControlFlowGraph::new(&func.basic_blocks);
            func.symtab = SymbolTable::new(&func.params, &func.basic_blocks);
        }
    }
}
