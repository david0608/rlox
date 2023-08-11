use crate::scan::span::Span;
use crate::parse::expr::Expression;
use crate::visitor::Printable;
use crate::impl_debug_for_printable;
use super::{
    Statement,
    Stmt,
};

pub struct IfStatement {
    condition: Expression,
    then_stmt: Statement,
    else_stmt: Option<Statement>,
    span: Span,
}

impl IfStatement {
    pub fn new(
        condition: Expression,
        then_stmt: Statement,
        else_stmt: Option<Statement>,
        span: Span,
    ) -> IfStatement
    {
        IfStatement {
            condition,
            then_stmt,
            else_stmt,
            span,
        }
    }

    pub fn condition(&self) -> &Expression {
        &self.condition
    }

    pub fn then_stmt(&self) -> &Statement {
        &self.then_stmt
    }

    pub fn else_stmt(&self) -> Option<&Statement> {
        self.else_stmt.as_ref()
    }
}

impl Stmt for IfStatement {
    fn span(&self) -> Span {
        self.span
    }
}

impl_debug_for_printable!(IfStatement);

#[macro_export]
macro_rules! if_statement {
    ( $condition:expr, $then_stmt:expr, $else_stmt:expr, $span:expr ) => {
        Box::new(
            IfStatement::new(
                $condition,
                $then_stmt,
                Some($else_stmt),
                $span,
            )
        )
    };

    ( $condition:expr, $then_stmt:expr, $span:expr ) => {
        Box::new(
            IfStatement::new(
                $condition,
                $then_stmt,
                None,
                $span,
            )
        )
    };
}
