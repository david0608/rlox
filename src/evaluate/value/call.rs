use std::rc::Rc;
use std::cell::RefCell;
use super::value::Value;
use crate::execute::ExecuteError;
use crate::scope::Scope;

pub enum CallError {
    ArgumentNumberMismatch(usize, usize),
    NotCallable,
    ExecuteError(ExecuteError),
}

pub type CallResult = std::result::Result<Value, CallError>;

pub trait Call {
    fn call(&self, scope: &Rc<RefCell<Scope>>, arguments: Vec<Value>) -> CallResult;
}
