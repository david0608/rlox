use crate::{
    value::Value,
    environment::Environment,
    error::RuntimeError
};

pub trait Evaluate {
    fn evaluate(&self, env: &Environment) -> Result<Value, RuntimeError>;
}
