use crate::visitor::Printable;
use crate::impl_debug_for_printable;
use super::Statement;

pub struct BlockStatement<'src>(pub Vec<Statement<'src>>);

impl_debug_for_printable!(BlockStatement<'_>);

#[macro_export]
macro_rules! block_statement {
    ( $stmts:expr ) => {
        Box::new(BlockStatement($stmts))
    }
}
