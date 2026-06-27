use ir::*;

use crate::ssa::{
    construct_ssa_form, destroy_ssa_form, hoist_define_instructions, split_critical_edges,
};
use crate::transform::constant_fold::run_constant_fold_pass;
use crate::transform::constant_prop::run_constant_prop_pass;
use crate::transform::dce::run_dce_pass;

pub fn run_opt_pipeline(prog: &mut Program) {
    hoist_define_instructions(prog);
    construct_ssa_form(prog);
    split_critical_edges(prog);

    for func in &mut prog.functions {
        loop {
            let mut modified = false;

            modified |= run_constant_prop_pass(func);
            modified |= run_constant_fold_pass(func);
            modified |= run_dce_pass(func);

            if !modified {
                break;
            }
        }
    }

    destroy_ssa_form(prog);
}
