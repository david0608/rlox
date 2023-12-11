use crate::code::Code;
use crate::code::code_span::CodeSpan;
use crate::parse::expression::BoxedExpression;
use super::{
    Statement,
    BoxedStatement,
};

pub struct ReturnStatement {
    expression: Option<BoxedExpression>,
    code_span: CodeSpan,
}

impl ReturnStatement {
    pub fn new(
        expression: Option<BoxedExpression>,
        code_span: CodeSpan,
    ) -> ReturnStatement
    {
        ReturnStatement {
            expression,
            code_span,
        }
    }

    pub fn expression(&self) -> Option<&BoxedExpression> {
        self.expression.as_ref()
    }
}

impl Code for ReturnStatement {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Statement for ReturnStatement {
    fn box_clone(&self) -> BoxedStatement {
        Box::new(
            ReturnStatement::new(
                self.expression().map(|s| s.clone()),
                self.code_span(),
            )
        )
    }
}

#[macro_export]
macro_rules! return_statement {
    ( $expression:expr, $code_span:expr ) => {
        Box::new(
            ReturnStatement::new(
                $expression,
                $code_span,
            )
        )
    }
}
