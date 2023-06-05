mod unary;
pub use unary::UnaryExpression;

mod binary;
pub use binary::BinaryExpression;

mod literal;
pub use literal::LiteralExpression;

mod grouping;
pub use grouping::GroupingExpression;

mod print;
use print::Printable;

mod evaluate;
use evaluate::Evaluable;

pub type Expression<'a> = Box<dyn Expr<'a>>;

pub trait Expr<'a>
    where
    Self: 'a
        + Printable
        + Evaluable<'a>
        + std::fmt::Debug
{ }

impl<'a, T> Expr<'a> for T
    where
    T: 'a
        + Printable
        + Evaluable<'a>
        + std::fmt::Debug
{ }
