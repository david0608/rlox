use super::Expression;
use super::print::Printable;

pub struct GroupingExpression<'a>(pub Expression<'a>);

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
