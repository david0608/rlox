use crate::scan::span::Span;
use crate::visitor::Printable;
use crate::impl_debug_for_printable;
use super::{
    Statement,
    Stmt,
};

pub struct BlockStatement {
    stmts: Vec<Statement>,
    span: Span,
}

impl BlockStatement {
    pub fn new(stmts: Vec<Statement>, span: Span) -> BlockStatement {
        BlockStatement {
            stmts,
            span,
        }
    }

    pub fn stmts(&self) -> &Vec<Statement> {
        &self.stmts
    }
}

impl Stmt for BlockStatement {
    fn span(&self) -> Span {
        self.span
    }
}

impl_debug_for_printable!(BlockStatement);

#[macro_export]
macro_rules! block_statement {
    ( $stmts:expr, $span:expr ) => {
        Box::new(
            BlockStatement::new(
                $stmts,
                $span,
            )
        )
    }
}
