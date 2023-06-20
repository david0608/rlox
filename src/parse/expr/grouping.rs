use crate::visitor::Printable;
use crate::impl_debug_for_printable;
use super::Expression;

pub struct GroupingExpression<'src>(pub Expression<'src>);

impl_debug_for_printable!(GroupingExpression<'_>);

#[macro_export]
macro_rules! grouping_expression {
    ( $expr:expr ) => {
        Box::new(GroupingExpression($expr))
    };
}
