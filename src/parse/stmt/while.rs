use crate::parse::expr::Expression;
use crate::visitor::Printable;
use crate::impl_debug_for_printable;
use super::Statement;

pub struct WhileStatement<'src> {
    pub condition: Expression<'src>,
    pub body: Statement<'src>,
}

impl_debug_for_printable!(WhileStatement<'_>);

#[macro_export]
macro_rules! while_statement {
    ( $condition:expr, $body:expr ) => {
        Box::new(
            WhileStatement {
                condition: $condition,
                body: $body,
            }
        )
    }
}
