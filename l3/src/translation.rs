use l2;
use l3::*;

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
