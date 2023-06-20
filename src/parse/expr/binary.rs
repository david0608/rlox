use crate::visitor::Printable;
use crate::impl_debug_for_printable;
use super::Expression;

pub enum BinaryExpression<'srca, 'srcb> {
    Equal(Expression<'srca>, Expression<'srcb>),
    NotEqual(Expression<'srca>, Expression<'srcb>),
    Less(Expression<'srca>, Expression<'srcb>),
    LessEqual(Expression<'srca>, Expression<'srcb>),
    Greater(Expression<'srca>, Expression<'srcb>),
    GreaterEqual(Expression<'srca>, Expression<'srcb>),
    Plus(Expression<'srca>, Expression<'srcb>),
    Minus(Expression<'srca>, Expression<'srcb>),
    Multiply(Expression<'srca>, Expression<'srcb>),
    Divide(Expression<'srca>, Expression<'srcb>),
}

impl_debug_for_printable!(BinaryExpression<'_, '_>);

#[macro_export]
macro_rules! binary_expression {
    ( $variant:ident, $lhs:expr, $rhs:expr ) => {
        Box::new(BinaryExpression::$variant($lhs, $rhs))
    };
}
