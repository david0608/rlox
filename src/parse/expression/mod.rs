use std::{
    any::Any,
    rc::Rc,
    fmt::Debug,
};
use crate::{
    code::Code,
    evaluate::Evaluate,
    print::Print,
    resolve::{
        ResolveCtx,
        ResolveError,
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
        + Print
        + Evaluate
        + Debug
        + Any
{
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Rc<dyn Expression>, ResolveError>;
}

impl Downcast for Rc<dyn Expression> {
    fn downcast<T: Any>(self) -> Option<Rc<T>> {
        (self as Rc<dyn Any>).downcast::<T>().ok()
    }

    fn downcast_ref<T: Any>(&self) -> Option<&T> {
        (self.as_ref() as &dyn Any).downcast_ref::<T>()
    }
}
