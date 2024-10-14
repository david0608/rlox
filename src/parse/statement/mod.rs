use std::{
    any::Any,
    rc::Rc,
    ops::Deref,
};
use crate::{
    code::Code,
    execute::Execute,
    print::Print,
    resolve::{
        ResolveCtx,
        ResolveError,
    }
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

pub trait AsStatement
    where
    Self: Code
        + Print
        + Execute
        + std::fmt::Debug
        + Any
{
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Statement, ResolveError>;
}

#[derive(Clone, Debug)]
pub struct Statement(pub Rc<dyn AsStatement>);

impl Statement {
    #[cfg(test)]
    pub fn downcast_ref<T: AsStatement>(&self) -> Option<&T> {
        (self.as_ref() as &dyn Any).downcast_ref::<T>()
    }

    #[cfg(test)]
    pub fn downcast<T: AsStatement>(self) -> Result<Rc<T>, Rc<dyn Any>> {
        (self.0 as Rc<dyn Any>).downcast::<T>()
    }
}

impl Deref for Statement {
    type Target = Rc<dyn AsStatement>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
