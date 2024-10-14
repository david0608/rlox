use std::{
    any::Any,
    rc::Rc,
    ops::Deref,
};
use crate::{
    code::Code,
    evaluate::Evaluate,
    print::Print,
    resolve::{
        ResolveCtx,
        ResolveError,
    }
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

pub trait AsExpression
    where
    Self: Code
        + Print
        + Evaluate
        + std::fmt::Debug
        + Any
{
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Expression, ResolveError>;
}

#[derive(Clone, Debug)]
pub struct Expression(pub Rc<dyn AsExpression>);

impl Expression {
    pub fn downcast_ref<T: AsExpression>(&self) -> Option<&T> {
        (self.as_ref() as &dyn Any).downcast_ref::<T>()
    }

    pub fn downcast<T: AsExpression>(self) -> Result<Rc<T>, Rc<dyn Any>> {
        (self.0 as Rc<dyn Any>).downcast::<T>()
    }
}

impl Deref for Expression {
    type Target = Rc<dyn AsExpression>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
