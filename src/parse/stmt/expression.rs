use crate::parse::expr::Expression;
use crate::visitor::Printable;
use crate::impl_debug_for_printable;

pub struct ExpressionStatement<'src>(pub Expression<'src>);

impl_debug_for_printable!(ExpressionStatement<'_>);

#[macro_export]
macro_rules! expression_statement {
    ( $expr:expr ) => {
        Box::new(ExpressionStatement($expr))
    }
}
