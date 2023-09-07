use crate::code::Code;
use crate::code::code_span::CodeSpan;
use super::{
    Expression,
    BoxedExpression,
};

pub struct GroupingExpression {
    expression: BoxedExpression,
    code_span: CodeSpan,
}

impl GroupingExpression {
    pub fn new(expression: BoxedExpression, code_span: CodeSpan) -> GroupingExpression {
        GroupingExpression {
            expression,
            code_span,
        }
    }

    pub fn expression(&self) -> &BoxedExpression {
        &self.expression
    }
}

impl Code for GroupingExpression {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Expression for GroupingExpression {
    fn box_clone(&self) -> BoxedExpression {
        Box::new(
            GroupingExpression::new(
                self.expression().clone(),
                self.code_span(),
            )
        )
    }
}

#[macro_export]
macro_rules! grouping_expression {
    ( $expression:expr, $code_span:expr ) => {
        Box::new(
            GroupingExpression::new(
                $expression,
                $code_span
            )
        )
    };
}
