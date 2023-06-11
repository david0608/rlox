use crate::visitor::Printable;
use super::Expression;

pub struct GroupingExpression<'src>(pub Expression<'src>);

impl std::fmt::Debug for GroupingExpression<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.print())
    }
}

#[macro_export]
macro_rules! grouping_expression {
    ( $expr:expr ) => {
        Box::new(GroupingExpression($expr))
    };
}
