use std::any::{Any, TypeId};
use std::collections::HashMap;

use ir::*;

pub trait Pass {
    fn name(&self) -> &str;

    fn run(&mut self, func: &mut Function, ctx: &mut PassContext) -> bool;
}

#[derive(Debug)]
pub struct PassContext {
    cache: HashMap<TypeId, Box<dyn Any>>,
}
