use crate::scan::token::IdentToken;
use crate::visitor::Printable;
use crate::impl_debug_for_printable;
use super::Expression;

pub struct AssignExpression<'src> {
    pub name: IdentToken<'src>,
    pub value: Expression<'src>,
}

impl_debug_for_printable!(AssignExpression<'_>);

#[macro_export]
macro_rules! assign_expression {
    ( $name:expr, $value:expr ) => {
        Box::new(
            AssignExpression {
                name: $name,
                value: $value,
            }
        )
    };
}
