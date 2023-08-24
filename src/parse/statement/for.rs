use crate::code::Code;
use crate::code::code_span::CodeSpan;
use crate::visitor::print::Printable;
use crate::parse::expression::BoxedExpression;
use super::{
    Statement,
    BoxedStatement,
};
use crate::impl_debug_for_printable;

pub struct ForStatement {
    initializer: Option<BoxedStatement>,
    condition: Option<BoxedExpression>,
    increment: Option<BoxedExpression>,
    body: BoxedStatement,
    code_span: CodeSpan,
}

impl ForStatement {
    pub fn new(
        initializer: Option<BoxedStatement>,
        condition: Option<BoxedExpression>,
        increment: Option<BoxedExpression>,
        body: BoxedStatement,
        code_span: CodeSpan,
    ) -> ForStatement
    {
        ForStatement {
            initializer,
            condition,
            increment,
            body,
            code_span,
        }
    }

    pub fn initializer(&self) -> Option<&BoxedStatement> {
        self.initializer.as_ref()
    }

    pub fn condition(&self) -> Option<&BoxedExpression> {
        self.condition.as_ref()
    }

    pub fn increment(&self) -> Option<&BoxedExpression> {
        self.increment.as_ref()
    }

    pub fn body(&self) -> &BoxedStatement {
        &self.body
    }
}

impl Code for ForStatement {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Statement for ForStatement {
    fn box_clone(&self) -> BoxedStatement {
        Box::new(
            ForStatement::new(
                self.initializer().map(|s| s.clone()),
                self.condition().map(|e| e.clone()),
                self.increment().map(|e| e.clone()),
                self.body().clone(),
                self.code_span(),
            )
        )
    }
}

impl_debug_for_printable!(ForStatement);

#[macro_export]
macro_rules! for_statement {
    (
        $initializer:expr,
        $condition:expr,
        $increment:expr,
        $body:expr,
        $code_span:expr,
    ) => {
        Box::new(
            ForStatement::new(
                $initializer,
                $condition,
                $increment,
                $body,
                $code_span,
            )
        )
    }
}
