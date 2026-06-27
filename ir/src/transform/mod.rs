mod constant_fold;
mod constant_prop;
mod dce;
mod pass;
mod pipeline;

pub use pipeline::run_opt_pipeline;
