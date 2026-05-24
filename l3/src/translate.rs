use l2;
use l3::*;

// TODO: not sure about this
#[derive(Debug)]
pub struct Translator;

impl Translator {
    pub fn symbol_id(id: SymbolId) -> l2::SymbolId {
        l2::SymbolId(id.0)
    }

    pub fn value(val: &Value) -> l2::Value {
        match val {
            Value::Number(num) => l2::Value::Number(*num),
            Value::Label(label) => l2::Value::Label(translate_symbol_id(*label)),
            Value::Function(callee) => l2::Value::Function(translate_symbol_id(*callee)),
            Value::Variable(var) => l2::Value::Variable(translate_symbol_id(*var)),
        }
    }
}

pub fn translate_symbol_id(id: SymbolId) -> l2::SymbolId {
    l2::SymbolId(id.0)
}

pub fn translate_value(val: &Value) -> l2::Value {
    match val {
        Value::Number(num) => l2::Value::Number(*num),
        Value::Label(label) => l2::Value::Label(translate_symbol_id(*label)),
        Value::Function(callee) => l2::Value::Function(translate_symbol_id(*callee)),
        Value::Variable(var) => l2::Value::Variable(translate_symbol_id(*var)),
    }
}
