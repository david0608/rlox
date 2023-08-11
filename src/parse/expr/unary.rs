use crate::scan::span::Span;
use crate::visitor::Printable;
use crate::impl_debug_for_printable;
use super::{
    Expression,
    Expr,
};

#[derive(Clone, Copy)]
pub enum UnaryExpressionEnum {
    Negative,
    Not
}

pub struct UnaryExpression {
    variant: UnaryExpressionEnum,
    rhs: Expression,
    span: Span,
}

impl UnaryExpression {
    pub fn new(
        variant: UnaryExpressionEnum, 
        rhs: Expression,
        span: Span,
    ) -> UnaryExpression
    {
        UnaryExpression {
            variant,
            rhs,
            span,
        }
    }

    pub fn variant(&self) -> UnaryExpressionEnum {
        self.variant
    }

    pub fn rhs(&self) -> &Expression {
        &self.rhs
    }
}

impl Expr for UnaryExpression {
    fn span(&self) -> Span {
        self.span
    }
}

impl_debug_for_printable!(UnaryExpression);

#[macro_export]
macro_rules! unary_expression {
    ( $variant:ident, $rhs:expr, $span:expr ) => {
        Box::new(
            UnaryExpression::new(
                UnaryExpressionEnum::$variant,
                $rhs,
                $span,
            )
        )
    };
}
