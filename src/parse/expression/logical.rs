use crate::code::Code;
use crate::code::code_span::CodeSpan;
use super::{
    Expression,
    BoxedExpression,
};

#[derive(Clone, Copy)]
pub enum LogicalExpressionEnum {
    And,
    Or,
}

pub struct LogicalExpression {
    variant: LogicalExpressionEnum,
    lhs: BoxedExpression,
    rhs: BoxedExpression,
    code_span: CodeSpan,
}

impl LogicalExpression {
    pub fn new(
        variant: LogicalExpressionEnum,
        lhs: BoxedExpression,
        rhs: BoxedExpression,
        code_span: CodeSpan,
    ) -> LogicalExpression
    {
        LogicalExpression {
            variant,
            lhs,
            rhs,
            code_span,
        }
    }

    pub fn variant(&self) -> LogicalExpressionEnum {
        self.variant
    }

    pub fn lhs(&self) -> &BoxedExpression {
        &self.lhs
    }

    pub fn rhs(&self) -> &BoxedExpression {
        &self.rhs
    }
}

impl Code for LogicalExpression {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Expression for LogicalExpression {
    fn box_clone(&self) -> BoxedExpression {
        Box::new(
            LogicalExpression::new(
                self.variant(),
                self.lhs().clone(),
                self.rhs().clone(),
                self.code_span(),
            )
        )
    }
}

#[macro_export]
macro_rules! logical_expression {
    ( $variant:ident, $lhs:expr, $rhs:expr, $code_span:expr ) => {
        Box::new(
            LogicalExpression::new(
                LogicalExpressionEnum::$variant,
                $lhs,
                $rhs,
                $code_span
            )
        )
    }
}
