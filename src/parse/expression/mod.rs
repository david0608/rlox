use crate::code::Code;
use crate::visitor::print::Printable;
use crate::visitor::evaluate::Evaluable;

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
        + Printable
        + Evaluable
        + std::fmt::Debug
{
    fn box_clone(&self) -> BoxedExpression;
}

pub type BoxedExpression = Box<dyn Expression>;

impl Clone for BoxedExpression {
    fn clone(&self) -> Self {
        self.as_ref().box_clone()
    }
}
