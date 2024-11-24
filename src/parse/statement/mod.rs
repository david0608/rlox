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
        RuntimeError,
        ResolveError,
    },
    utils::Downcast,
};

pub mod block;
pub mod r#break;
pub mod class_declare;
pub mod expression;
pub mod r#for;
pub mod fun_declare;
pub mod ifelse;
pub mod print;
pub mod r#return;
pub mod var_declare;
pub mod r#while;

pub trait Statement
    where
    Self: Code
        + Debug
        + Any
{
    fn resolve(&self, context: &mut Vec<HashSet<String>>) -> Result<Rc<dyn Statement>, ResolveError>;

    fn execute(&self, env: &Rc<RefCell<Environment>>) -> Result<ExecuteOk, RuntimeError>;
}

impl Downcast for Rc<dyn Statement> {
    fn downcast<T: Any>(self) -> Option<Rc<T>> {
        (self as Rc<dyn Any>).downcast::<T>().ok()
    }

    fn downcast_ref<T: 'static>(&self) -> Option<&T> {
        (self.as_ref() as &dyn Any).downcast_ref::<T>()
    }
}

#[derive(PartialEq, Debug)]
pub enum ExecuteOk {
    KeepGoing,
    Break,
    Return(Value),
}
