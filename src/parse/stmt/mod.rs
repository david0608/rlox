use crate::visitor::{
    Printable,
    Executable,
};

mod var_declare;
pub use var_declare::VarDeclareStatement;

mod expression;
pub use expression::ExpressionStatement;

mod print;
pub use print::PrintStatement;

pub type Statement<'src> = Box<dyn Stmt<'src>>;

pub trait Stmt<'src>
    where
    Self: 'src
        + Printable
        + Executable<'src>
        + std::fmt::Debug
{ }

impl<'src, T> Stmt<'src> for T
    where
    T: 'src
        + Printable
        + Executable<'src>
        + std::fmt::Debug
{ }
