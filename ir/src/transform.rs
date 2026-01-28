mod const_fold;
mod const_prop;

pub use const_fold::constant_folding;
pub use const_prop::constant_propagation;
