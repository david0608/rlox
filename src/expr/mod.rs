use crate::visit::Accept;

mod unary;
pub use unary::UnaryExpression;

mod binary;
pub use binary::BinaryExpression;

mod literal;
pub use literal::LiteralExpression;

mod grouping;
pub use grouping::GroupingExpression;

mod visitor;
use visitor::{
    Print,
    Evaluate,
    EvaluateResult,
};

pub type Expression<'a> = Box<dyn Expr<'a>>;

pub trait Expr<'a>:
    'a
    + for<'s> Accept<'s, Print, String>
    + for<'s> Accept<'s, Evaluate, EvaluateResult<'s, 'a>>
    + std::fmt::Debug
{
    fn print(&self) -> String {
        self.accept(Print)
    }

    fn evaluate<'s>(&'s self) -> EvaluateResult<'s, 'a> {
        self.accept(Evaluate)
    }
}

impl<'a, T> Expr<'a> for T
    where
    T: 'a
        + for<'s> Accept<'s, Print, String>
        + for<'s> Accept<'s, Evaluate, EvaluateResult<'s, 'a>>
        + std::fmt::Debug
{ }
