use crate::scan::span::Span;
use crate::parse::expr::Expression;
use crate::visitor::Printable;
use crate::impl_debug_for_printable;
use super::{
    Statement,
    Stmt,
};

pub struct WhileStatement {
    condition: Option<Expression>,
    body: Statement,
    span: Span,
}

impl WhileStatement {
    pub fn new(condition: Option<Expression>, body: Statement, span: Span) -> WhileStatement {
        WhileStatement {
            condition,
            body,
            span,
        }
    }

    pub fn condition(&self) -> Option<&Expression> {
        self.condition.as_ref()
    }

    pub fn body(&self) -> &Statement {
        &self.body
    }
}

impl Stmt for WhileStatement {
    fn span(&self) -> Span {
        self.span
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
