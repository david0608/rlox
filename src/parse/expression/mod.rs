use std::{
    any::Any,
    rc::Rc,
    cell::RefCell,
    fmt::Debug,
    collections::HashSet,
};
use crate::{
    code::Code,
    value::Value,
    environment::Environment,
    error::{
        ResolveError,
        RuntimeError,
    },
    utils::Downcast,
};

pub mod assign;
pub mod binary;
pub mod call;
pub mod get;
pub mod grouping;
pub mod literal;
pub mod logical;
pub mod set;
pub mod unary;
pub mod variable;

pub trait Expression
    where
    Self: Code
        + Debug
        + Any
{
    fn resolve(&self, context: &mut Vec<HashSet<String>>) -> Result<Rc<dyn Expression>, ResolveError>;

    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> Result<Value, RuntimeError>;
}

impl Downcast for Rc<dyn Expression> {
    fn downcast<T: Any>(self) -> Option<Rc<T>> {
        (self as Rc<dyn Any>).downcast::<T>().ok()
    }

    fn downcast_ref<T: Any>(&self) -> Option<&T> {
        (self.as_ref() as &dyn Any).downcast_ref::<T>()
    }
}
