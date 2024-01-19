use crate::value::Value;
use crate::error::RuntimeError;

#[derive(Debug, PartialEq)]
pub enum CallError {
    ArgumentNumberMismatch(usize, usize),
    NotCallable,
    RuntimeError(RuntimeError),
}

pub type CallResult = std::result::Result<Value, CallError>;

pub trait Call {
    fn call(&self, arguments: Vec<Value>) -> CallResult;
}
