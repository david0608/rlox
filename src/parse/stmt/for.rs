use crate::scan::span::Span;
use crate::parse::expr::Expression;
use crate::visitor::Printable;
use crate::impl_debug_for_printable;
use super::{
    Statement,
    Stmt,
};

pub struct ForStatement {
    initializer: Option<Statement>,
    condition: Option<Expression>,
    increment: Option<Expression>,
    body: Statement,
    span: Span,
}

impl ForStatement {
    pub fn new(
        initializer: Option<Statement>,
        condition: Option<Expression>,
        increment: Option<Expression>,
        body: Statement,
        span: Span,
    ) -> ForStatement
    {
        ForStatement {
            initializer,
            condition,
            increment,
            body,
            span,
        }
    }

    pub fn initializer(&self) -> Option<&Statement> {
        self.initializer.as_ref()
    }

    pub fn condition(&self) -> Option<&Expression> {
        self.condition.as_ref()
    }

    pub fn increment(&self) -> Option<&Expression> {
        self.increment.as_ref()
    }

    pub fn body(&self) -> &Statement {
        &self.body
    }
}

impl Stmt for ForStatement {
    fn span(&self) -> Span {
        self.span
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
        $span:expr,
    ) => {
        Box::new(
            ForStatement::new(
                $initializer,
                $condition,
                $increment,
                $body,
                $span,
            )
        )
    }
}
