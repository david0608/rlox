use crate::scan::token::IdentToken;
use crate::visitor::Printable;
use crate::impl_debug_for_printable;

pub struct VariableExpression<'src>(pub IdentToken<'src>);

impl_debug_for_printable!(VariableExpression<'_>);

#[macro_export]
macro_rules! variable_expression {
    ( $token:expr ) => {
        Box::new(VariableExpression($token))
    }
}
