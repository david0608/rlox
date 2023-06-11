use crate::scan::token::IdentToken;
use crate::visitor::Printable;

pub struct VariableExpression<'src>(pub IdentToken<'src>);

impl std::fmt::Debug for VariableExpression<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.print())
    }
}

#[macro_export]
macro_rules! variable_expression {
    ( $token:expr ) => {
        Box::new(VariableExpression($token))
    }
}
