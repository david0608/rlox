use crate::scan::span::Span;
use crate::visitor::Printable;
use crate::impl_debug_for_printable;
use super::{
    Expression,
    Expr,
};

#[derive(Clone, Copy)]
pub enum BinaryExpressionEnum {
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Plus,
    Minus,
    Multiply,
    Divide,
}

pub struct BinaryExpression {
    variant: BinaryExpressionEnum,
    lhs: Expression,
    rhs: Expression,
    span: Span,
}

impl BinaryExpression {
    pub fn new(
        variant: BinaryExpressionEnum,
        lhs: Expression,
        rhs: Expression,
        span: Span,
    ) -> BinaryExpression
    {
        BinaryExpression {
            variant,
            lhs,
            rhs,
            span,
        }
    }

    pub fn variant(&self) -> BinaryExpressionEnum {
        self.variant
    }

    pub fn lhs(&self) -> &Expression {
        &self.lhs
    }

    pub fn rhs(&self) -> &Expression {
        &self.rhs
    }
}

impl Expr for BinaryExpression {
    fn span(&self) -> Span {
        self.span
    }
}

impl_debug_for_printable!(BinaryExpression);

#[macro_export]
macro_rules! binary_expression {
    ( $variant:ident, $lhs:expr, $rhs:expr, $span:expr ) => {
        Box::new(
            BinaryExpression::new(
                BinaryExpressionEnum::$variant,
                $lhs,
                $rhs,
                $span
            )
        )
    };
}
