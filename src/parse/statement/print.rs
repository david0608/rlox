use crate::code::Code;
use crate::code::code_span::CodeSpan;
use crate::parse::expression::BoxedExpression;
use super::{
    Statement,
    BoxedStatement,
};

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
