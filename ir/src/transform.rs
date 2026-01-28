mod const_fold;
mod const_prop;
mod pass;
mod pipeline;

pub use const_fold::constant_folding;
pub use const_prop::constant_propagation;
