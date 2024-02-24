use std::any::Any;
use crate::code::Code;
use crate::execute::Execute;
use crate::print::Print;
use crate::resolve::{
    ResolveCtx,
    ResolveError,
};
use crate::utils::AsAny;

pub mod block;
pub mod r#break;
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
        + std::fmt::Debug
        + Any
{
    fn box_clone(&self) -> BoxedStatement;

    fn resolve(&self, context: &mut ResolveCtx) -> Result<BoxedStatement, ResolveError>;
}

pub type BoxedStatement = Box<dyn Statement>;

impl Clone for BoxedStatement {
    fn clone(&self) -> Self {
        self.as_ref().box_clone()
    }
}

impl AsAny for BoxedStatement {
    fn as_any_ref(&self) -> &dyn Any {
        self.as_ref() as &dyn Any
    }

    fn as_box_any(self) -> Box<dyn Any> {
        self
    }
}
