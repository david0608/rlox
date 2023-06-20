use crate::visitor::Printable;
use crate::impl_debug_for_printable;
use super::Expression;

pub enum UnaryExpression<'src> {
    Negative(Expression<'src>),
    Not(Expression<'src>)
}

impl_debug_for_printable!(UnaryExpression<'_>);

#[macro_export]
macro_rules! unary_expression {
    ( $variant:ident, $rhs:expr ) => {
        Box::new(UnaryExpression::$variant($rhs))
    };
}
