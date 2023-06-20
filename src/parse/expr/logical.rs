use crate::visitor::Printable;
use crate::impl_debug_for_printable;
use super::Expression;

pub enum LogicalExpression<'srca, 'srcb> {
    And(Expression<'srca>, Expression<'srcb>),
    Or(Expression<'srca>, Expression<'srcb>),
}

impl_debug_for_printable!(LogicalExpression<'_, '_>);

#[macro_export]
macro_rules! logical_expression {
    ( $variant:ident, $lhs:expr, $rhs:expr ) => {
        Box::new(LogicalExpression::$variant($lhs, $rhs))
    }
}
