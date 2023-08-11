use crate::scan::span::Span;
use crate::parse::expr::Expression;
use crate::visitor::Printable;
use crate::impl_debug_for_printable;
use super::Stmt;

pub struct PrintStatement {
    value: Expression,
    span: Span,
}

impl PrintStatement {
    pub fn new(value: Expression, span: Span) -> PrintStatement {
        PrintStatement {
            value,
            span,
        }
    }

    pub fn value(&self) -> &Expression {
        &self.value
    }
}

impl Stmt for PrintStatement {
    fn span(&self) -> Span {
        self.span
    }
}

impl_debug_for_printable!(PrintStatement);

#[macro_export]
macro_rules! print_statement {
    ( $expr:expr, $span:expr ) => {
        Box::new(
            PrintStatement::new(
                $expr,
                $span,
            )
        )
    }
}
