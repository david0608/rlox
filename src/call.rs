use std::rc::Rc;
use std::cell::RefCell;
use crate::value::Value;
use crate::error::RuntimeError;
use crate::scope::Scope;

#[derive(Debug, PartialEq)]
pub enum CallError {
    ArgumentNumberMismatch(usize, usize),
    NotCallable,
    RuntimeError(RuntimeError),
}

pub type CallResult = std::result::Result<Value, CallError>;

pub trait Call {
    fn call(&self, scope: &Rc<RefCell<Scope>>, arguments: Vec<Value>) -> CallResult;
}
