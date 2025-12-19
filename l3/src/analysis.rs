mod dataflow;
mod def_use;
mod liveness;
mod reaching_def;

pub use def_use::DefUseChain;
pub use liveness::{LivenessResult, compute_liveness};
pub use reaching_def::{ReachingDefResult, compute_reaching_def};
