use crate::code::Code;
use crate::code::code_span::CodeSpan;
use crate::visitor::print::Printable;
use super::{
    Expression,
    BoxedExpression,
};
use crate::impl_debug_for_printable;

#[derive(Clone, Copy)]
pub enum UnaryExpressionEnum {
    Negative,
    Not
}

pub struct UnaryExpression {
    variant: UnaryExpressionEnum,
    rhs: BoxedExpression,
    code_span: CodeSpan,
}

impl UnaryExpression {
    pub fn new(
        variant: UnaryExpressionEnum, 
        rhs: BoxedExpression,
        code_span: CodeSpan,
    ) -> UnaryExpression
    {
        UnaryExpression {
            variant,
            rhs,
            code_span,
        }
    }

    pub fn variant(&self) -> UnaryExpressionEnum {
        self.variant
    }

    pub fn rhs(&self) -> &BoxedExpression {
        &self.rhs
    }
}

impl Code for UnaryExpression {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Expression for UnaryExpression {
    fn box_clone(&self) -> BoxedExpression {
        Box::new(
            UnaryExpression::new(
                self.variant(),
                self.rhs().clone(),
                self.code_span(),
            )
        )
    }
}

impl_debug_for_printable!(UnaryExpression);

#[macro_export]
macro_rules! unary_expression {
    ( $variant:ident, $rhs:expr, $code_span:expr ) => {
        Box::new(
            UnaryExpression::new(
                UnaryExpressionEnum::$variant,
                $rhs,
                $code_span,
            )
        )
    };
}
