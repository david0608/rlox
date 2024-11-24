use crate::{
    value::{
        Value,
        Call,
        function::function_id,
    },
    error::RuntimeErrorEnum,
};

pub type NativeFunctionHandler = fn(Vec<Value>) -> Result<Value, RuntimeErrorEnum>;

#[derive(Clone)]
pub struct NativeFunction {
    id: usize,
    name: &'static str,
    handler: NativeFunctionHandler,
}

impl NativeFunction {
    pub fn new(name: &'static str, handler: NativeFunctionHandler) -> Self {
        NativeFunction {
            id: function_id(),
            name,
            handler,
        }
    }
}

impl Call for NativeFunction {
    fn call(&self, arguments: Vec<Value>) -> Result<Value, RuntimeErrorEnum> {
        (self.handler)(arguments)
    }
}

impl std::fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "NativeFunction {}", self.name)
    }
}

impl std::cmp::PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
