use std::any::Any;
use crate::code::Code;
use crate::evaluate::Evaluate;
use crate::print::Print;
use crate::resolve::{
    ResolveCtx,
    ResolveError,
};
use crate::utils::AsAny;

pub mod assign;
pub mod binary;
pub mod call;
pub mod grouping;
pub mod literal;
pub mod logical;
pub mod unary;
pub mod variable;

pub trait Expression
    where
    Self: Code
        + Print
        + Evaluate
        + std::fmt::Debug
        + Any
{
    fn box_clone(&self) -> BoxedExpression;

    fn resolve(&self, context: &mut ResolveCtx) -> Result<BoxedExpression, ResolveError>;
}

pub type BoxedExpression = Box<dyn Expression>;

impl Clone for BoxedExpression {
    fn clone(&self) -> Self {
        self.as_ref().box_clone()
    }
}

impl AsAny for BoxedExpression {
    fn as_any_ref(&self) -> &dyn Any {
        self.as_ref() as &dyn Any
    }

    fn as_box_any(self) -> Box<dyn Any> {
        self
    }
}
