use crate::code::Code;
use crate::code::code_span::CodeSpan;
use crate::parse::expression::BoxedExpression;
use super::{
    Statement,
    BoxedStatement,
};

pub struct ExpressionStatement {
    expression: BoxedExpression,
    code_span: CodeSpan,
}

impl ExpressionStatement {
    pub fn new(expression: BoxedExpression, code_span: CodeSpan) -> ExpressionStatement {
        ExpressionStatement {
            expression,
            code_span,
        }
    }

    pub fn expression(&self) -> &BoxedExpression {
        &self.expression
    }
}

impl Code for ExpressionStatement {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Statement for ExpressionStatement {
    fn box_clone(&self) -> BoxedStatement {
        Box::new(
            ExpressionStatement::new(
                self.expression().clone(),
                self.code_span(),
            )
        )
    }
}

#[macro_export]
macro_rules! expression_statement {
    ( $expression:expr, $code_span:expr ) => {
        Box::new(
            ExpressionStatement::new(
                $expression,
                $code_span,
            )
        )
    }
}
