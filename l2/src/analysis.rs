mod dominators;
mod liveness;
mod loops;

pub use crate::analysis::dominators::DominatorTree;
pub use crate::analysis::liveness::{LivenessResult, compute_liveness};
pub use crate::analysis::loops::LoopForest;
