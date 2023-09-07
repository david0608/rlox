use crate::code::Code;
use crate::visitor::execute::Executable;
use crate::print::Print;

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
        + Print
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
