use crate::scan::span::Span;
use crate::visitor::{
    Printable,
    Evaluable,
};

pub mod unary;

pub mod binary;

pub mod literal;

pub mod grouping;

pub mod variable;

pub mod assign;

pub mod logical;

pub type Expression = Box<dyn Expr>;

pub trait Expr
    where
    Self: Printable
        + Evaluable
        + std::fmt::Debug
{
    fn span(&self) -> Span;
}
