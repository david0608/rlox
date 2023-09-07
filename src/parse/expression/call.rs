use crate::code::Code;
use crate::code::code_span::CodeSpan;
use super::{
    Expression,
    BoxedExpression,
};

pub struct CallExpression {
    callee: BoxedExpression,
    arguments: Vec<BoxedExpression>,
    code_span: CodeSpan,
}

impl CallExpression {
    pub fn new(
        callee: BoxedExpression,
        arguments: Vec<BoxedExpression>,
        code_span: CodeSpan,
    ) -> CallExpression
    {
        CallExpression {
            callee,
            arguments,
            code_span,
        }
    }

    pub fn callee(&self) -> &BoxedExpression {
        &self.callee
    }

    pub fn arguments(&self) -> &Vec<BoxedExpression> {
        &self.arguments
    }
}

impl Code for CallExpression {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Expression for CallExpression {
    fn box_clone(&self) -> BoxedExpression {
        Box::new(
            CallExpression::new(
                self.callee().clone(),
                self.arguments().clone(),
                self.code_span(),
            )
        )
    }
}

#[macro_export]
macro_rules! call_expression {
    ( $callee:expr, $arguments:expr, $code_span:expr ) => {
        Box::new(
            CallExpression::new(
                $callee,
                $arguments,
                $code_span
            )
        )
    }
}
