use std::rc::Rc;
use std::cell::RefCell;
use crate::evaluate::value::call::{
    Call,
    CallResult,
};
use crate::evaluate::value::function::function_id;
use crate::evaluate::value::value::Value;
use crate::scope::Scope;

pub type NativeFunctionHandler = fn(&Rc<RefCell<Scope>>, Vec<Value>) -> CallResult;

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

impl Call for NativeFunction {
    fn call(&self, scope: &Rc<RefCell<Scope>>, arguments: Vec<Value>) -> CallResult {
        (self.handler)(scope, arguments)
    }
}
