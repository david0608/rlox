use crate::visitor::{
    Printable,
    Evaluable,
};

mod unary;
pub use unary::UnaryExpression;

mod binary;
pub use binary::BinaryExpression;

mod literal;
pub use literal::LiteralExpression;

mod grouping;
pub use grouping::GroupingExpression;

mod variable;
pub use variable::VariableExpression;

mod assign;
pub use assign::AssignExpression;

mod logical;
pub use logical::LogicalExpression;

pub type Expression<'src> = Box<dyn Expr<'src>>;

pub trait Expr<'src>
    where
    Self: 'src
        + Printable
        + Evaluable<'src>
        + std::fmt::Debug
{ }

impl<'src, T> Expr<'src> for T
    where
    T: 'src
        + Printable
        + Evaluable<'src>
        + std::fmt::Debug
{ }
