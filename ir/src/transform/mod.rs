mod alg_simplify;
mod const_fold;
mod const_prop;
mod dce;
mod pass;
mod pipeline;

pub use alg_simplify::algebraic_simplification;
pub use const_fold::constant_folding;
pub use const_prop::constant_propagation;
