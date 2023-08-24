use crate::code::Code;
use crate::visitor::print::Printable;
use crate::visitor::execute::Executable;

pub mod block;
pub mod expression;
pub mod r#for;
pub mod ifelse;
pub mod print;
pub mod var_declare;
pub mod r#while;

pub trait Statement
    where
    Self: Code
        + Printable
        + Executable
        + std::fmt::Debug
{
    fn box_clone(&self) -> BoxedStatement;
}

pub type BoxedStatement = Box<dyn Statement>;

impl Clone for BoxedStatement {
    fn clone(&self) -> Self {
        self.as_ref().box_clone()
    }
}
