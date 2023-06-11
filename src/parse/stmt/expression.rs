use crate::parse::expr::Expression;
use crate::visitor::Printable;

pub struct ExpressionStatement<'src>(pub Expression<'src>);

impl std::fmt::Debug for ExpressionStatement<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.print())
    }
}

#[macro_export]
macro_rules! expression_statement {
    ( $expr:expr ) => {
        Box::new(ExpressionStatement($expr))
    }
}
