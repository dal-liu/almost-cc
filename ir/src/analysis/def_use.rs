use std::collections::HashMap;

use ir::*;
use utils::{BitVector, Interner};

#[derive(Debug)]
pub enum User {
    Instruction(Instruction),
    Terminator(Terminator),
}

pub struct DefUseChain {
    interner: Interner<User>,
    users: HashMap<SymbolId, BitVector>,
}
