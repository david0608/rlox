use crate::visitor::Printable;

use super::Statement;

pub struct BlockStatement<'src>(pub Vec<Statement<'src>>);

impl std::fmt::Debug for BlockStatement<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.print())
    }
}

#[macro_export]
macro_rules! block_statement {
    ( $stmts:expr ) => {
        Box::new(BlockStatement($stmts))
    }
}
