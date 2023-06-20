use crate::parse::expr::Expression;
use crate::visitor::Printable;
use crate::impl_debug_for_printable;

pub struct PrintStatement<'src>(pub Expression<'src>);

impl_debug_for_printable!(PrintStatement<'_>);

#[macro_export]
macro_rules! print_statement {
    ( $expr:expr ) => {
        Box::new(PrintStatement($expr))
    }
}
