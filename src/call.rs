use crate::{
    value::Value,
    error::RuntimeError,
};

#[derive(Debug, PartialEq)]
pub enum CallError {
    ArgumentNumberMismatch(usize, usize),
    NotCallable,
    RuntimeError(RuntimeError),
}

pub trait Call {
    fn call(&self, arguments: Vec<Value>) -> Result<Value, CallError>;
}
