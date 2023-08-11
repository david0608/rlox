use crate::scan::span::Span;
use crate::visitor::{
    Printable,
    Executable,
};

pub mod block;

pub mod expression;

pub mod r#for;

pub mod ifelse;

pub mod print;

pub mod var_declare;

pub mod r#while;

pub type Statement = Box<dyn Stmt>;

pub trait Stmt
    where
    Self: Printable
        + Executable
        + std::fmt::Debug
{
    fn span(&self) -> Span;
}
