mod construct;
mod destroy;
mod hoist;
mod split;

pub use construct::construct_ssa_form;
pub use destroy::destroy_ssa_form;
pub use hoist::hoist_define_instructions;
pub use split::split_critical_edges;
