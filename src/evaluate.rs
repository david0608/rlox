use std::{
    rc::Rc,
    cell::RefCell,
};
use crate::{
    value::Value,
    environment::Environment,
    error::RuntimeError
};

pub trait Evaluate {
    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> Result<Value, RuntimeError>;
}
