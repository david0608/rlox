use crate::scan::span::Span;
use crate::parse::expr::Expression;
use crate::visitor::Printable;
use crate::impl_debug_for_printable;
use super::Stmt;

pub struct ExpressionStatement {
    expression: Expression,
    span: Span,
}

impl ExpressionStatement {
    pub fn new(expression: Expression, span: Span) -> ExpressionStatement {
        ExpressionStatement {
            expression,
            span,
        }
    }

    pub fn expression(&self) -> &Expression {
        &self.expression
    }
}

impl Stmt for ExpressionStatement {
    fn span(&self) -> Span {
        self.span
    }
}

impl_debug_for_printable!(ExpressionStatement);

#[macro_export]
macro_rules! expression_statement {
    ( $expr:expr, $span:expr ) => {
        Box::new(
            ExpressionStatement::new(
                $expr,
                $span,
            )
        )
    }
}
