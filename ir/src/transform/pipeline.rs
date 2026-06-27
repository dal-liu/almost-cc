use ir::*;

use crate::transform::{
    run_constant_fold_pass, run_constant_prop_pass, run_copy_prop_pass, run_dce_pass,
};

pub fn run_opt_pipeline(prog: &mut Program) {
    for func in &mut prog.functions {
        loop {
            let mut modified = false;

            modified |= run_dce_pass(func);
            modified |= run_copy_prop_pass(func);
            modified |= run_constant_prop_pass(func);
            modified |= run_constant_fold_pass(func);

            if !modified {
                break;
            }
        }
    }
}
