use crate::code::Code;
use crate::code::code_span::CodeSpan;
use super::{
    Statement,
    BoxedStatement,
};

pub struct BlockStatement {
    statements: Vec<BoxedStatement>,
    code_span: CodeSpan,
}

impl BlockStatement {
    pub fn new(statements: Vec<BoxedStatement>, code_span: CodeSpan) -> BlockStatement {
        BlockStatement {
            statements,
            code_span,
        }
    }

    pub fn statements(&self) -> &Vec<BoxedStatement> {
        &self.statements
    }
}

impl Code for BlockStatement {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Statement for BlockStatement {
    fn box_clone(&self) -> BoxedStatement {
        Box::new(
            BlockStatement::new(
                self.statements.clone(),
                self.code_span(),
            )
        )
    }
}

#[macro_export]
macro_rules! block_statement {
    ( $statements:expr, $code_span:expr ) => {
        Box::new(
            BlockStatement::new(
                $statements,
                $code_span,
            )
        )
    }
}