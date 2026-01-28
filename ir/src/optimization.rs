mod const_prop;
mod peephole;

pub use const_prop::constant_propagation;
pub use peephole::run_peephole_passes;
