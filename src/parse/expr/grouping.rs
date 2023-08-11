use crate::scan::span::Span;
use crate::visitor::Printable;
use crate::impl_debug_for_printable;
use super::{
    Expression,
    Expr,
};

pub struct GroupingExpression {
    expr: Expression,
    span: Span,
}

impl GroupingExpression {
    pub fn new(expr: Expression, span: Span) -> GroupingExpression {
        GroupingExpression {
            expr,
            span,
        }
    }

    pub fn expr(&self) -> &Expression {
        &self.expr
    }
}

impl Expr for GroupingExpression {
    fn span(&self) -> Span {
        self.span
    }
}

impl_debug_for_printable!(GroupingExpression);

#[macro_export]
macro_rules! grouping_expression {
    ( $expr:expr, $span:expr ) => {
        Box::new(
            GroupingExpression::new(
                $expr,
                $span
            )
        )
    };
}
