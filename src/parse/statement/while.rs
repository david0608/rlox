use crate::code::Code;
use crate::code::code_span::CodeSpan;
use crate::visitor::print::Printable;
use crate::parse::expression::BoxedExpression;
use super::{
    Statement,
    BoxedStatement,
};
use crate::impl_debug_for_printable;

pub struct WhileStatement {
    condition: Option<BoxedExpression>,
    body: BoxedStatement,
    code_span: CodeSpan,
}

impl WhileStatement {
    pub fn new(
        condition: Option<BoxedExpression>,
        body: BoxedStatement,
        code_span: CodeSpan
    ) -> WhileStatement
    {
        WhileStatement {
            condition,
            body,
            code_span,
        }
    }

    pub fn condition(&self) -> Option<&BoxedExpression> {
        self.condition.as_ref()
    }

    pub fn body(&self) -> &BoxedStatement {
        &self.body
    }
}

impl Code for WhileStatement {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Statement for WhileStatement {
    fn box_clone(&self) -> BoxedStatement {
        Box::new(
            WhileStatement::new(
                self.condition().map(|e| e.clone()),
                self.body().clone(),
                self.code_span(),
            )
        )
    }
}

impl_debug_for_printable!(WhileStatement);

#[macro_export]
macro_rules! while_statement {
    ( $body:expr, $span:expr ) => {
        Box::new(
            WhileStatement::new(
                None,
                $body,
                $span,
            )
        )
    };

    ( $condition:expr, $body:expr, $span:expr ) => {
        Box::new(
            WhileStatement::new(
                Some($condition),
                $body,
                $span,
            )
        )
    }
}
