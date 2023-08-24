use crate::code::Code;
use crate::code::code_span::CodeSpan;
use crate::visitor::print::Printable;
use crate::parse::expression::BoxedExpression;
use super::{
    Statement,
    BoxedStatement,
};
use crate::impl_debug_for_printable;

pub struct PrintStatement {
    value: BoxedExpression,
    code_span: CodeSpan,
}

impl PrintStatement {
    pub fn new(value: BoxedExpression, code_span: CodeSpan) -> PrintStatement {
        PrintStatement {
            value,
            code_span,
        }
    }

    pub fn value(&self) -> &BoxedExpression {
        &self.value
    }
}

impl Code for PrintStatement {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Statement for PrintStatement {
    fn box_clone(&self) -> BoxedStatement {
        Box::new(
            PrintStatement::new(
                self.value().clone(),
                self.code_span(),
            )
        )
    }
}

impl_debug_for_printable!(PrintStatement);

#[macro_export]
macro_rules! print_statement {
    ( $expression:expr, $code_span:expr ) => {
        Box::new(
            PrintStatement::new(
                $expression,
                $code_span,
            )
        )
    }
}
