use crate::code::Code;
use crate::code::code_span::CodeSpan;
use super::{
    Statement,
    BoxedStatement,
};

pub struct BreakStatement {
    code_span: CodeSpan,
}

impl BreakStatement {
    pub fn new(
        code_span: CodeSpan,
    ) -> BreakStatement
    {
        BreakStatement {
            code_span,
        }
    }
}

impl Code for BreakStatement {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Statement for BreakStatement {
    fn box_clone(&self) -> BoxedStatement {
        Box::new(
            BreakStatement::new(
                self.code_span(),
            )
        )
    }
}

#[macro_export]
macro_rules! break_statement {
    ( $code_span:expr ) => {
        Box::new(
            BreakStatement::new(
                $code_span,
            )
        )
    }
}
