use crate::scan::span::Span;
use crate::visitor::Printable;
use crate::impl_debug_for_printable;
use super::{
    Expression,
    Expr,
};

#[derive(Clone, Copy)]
pub enum LogicalExpressionEnum {
    And,
    Or,
}

pub struct LogicalExpression {
    variant: LogicalExpressionEnum,
    lhs: Expression,
    rhs: Expression,
    span: Span,
}

impl LogicalExpression {
    pub fn new(
        variant: LogicalExpressionEnum,
        lhs: Expression,
        rhs: Expression,
        span: Span,
    ) -> LogicalExpression
    {
        LogicalExpression {
            variant,
            lhs,
            rhs,
            span,
        }
    }

    pub fn variant(&self) -> LogicalExpressionEnum {
        self.variant
    }

    pub fn lhs(&self) -> &Expression {
        &self.lhs
    }

    pub fn rhs(&self) -> &Expression {
        &self.rhs
    }
}

impl Expr for LogicalExpression {
    fn span(&self) -> Span {
        self.span
    }
}

impl_debug_for_printable!(LogicalExpression);

#[macro_export]
macro_rules! logical_expression {
    ( $variant:ident, $lhs:expr, $rhs:expr, $span:expr ) => {
        Box::new(
            LogicalExpression::new(
                LogicalExpressionEnum::$variant,
                $lhs,
                $rhs,
                $span
            )
        )
    }
}
