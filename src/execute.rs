use crate::{
    value::Value,
    environment::Environment,
    error::RuntimeError,
};

#[derive(PartialEq, Debug)]
pub enum ExecuteOk {
    KeepGoing,
    Break,
    Return(Value),
}

pub type ExecuteResult = std::result::Result<ExecuteOk, RuntimeError>;

pub trait Execute {
    fn execute(&self, env: &Environment) -> ExecuteResult;
}
