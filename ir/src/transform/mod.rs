mod constant_fold;
mod constant_prop;
mod copy_prop;
mod dce;
mod pass;
mod pipeline;

pub use constant_fold::run_constant_fold_pass;
pub use constant_prop::run_constant_prop_pass;
pub use copy_prop::run_copy_prop_pass;
pub use dce::run_dce_pass;
pub use pipeline::run_opt_pipeline;
