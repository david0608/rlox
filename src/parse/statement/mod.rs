use std::{
    any::Any,
    rc::Rc,
    fmt::Debug,
};
use crate::{
    code::Code,
    execute::Execute,
    print::Print,
    resolve::{
        ResolveCtx,
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
        + Print
        + Execute
        + Debug
        + Any
{
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Rc<dyn Statement>, ResolveError>;
}

impl Downcast for Rc<dyn Statement> {
    fn downcast<T: Any>(self) -> Option<Rc<T>> {
        (self as Rc<dyn Any>).downcast::<T>().ok()
    }

    fn downcast_ref<T: 'static>(&self) -> Option<&T> {
        (self.as_ref() as &dyn Any).downcast_ref::<T>()
    }
}
