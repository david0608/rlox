use crate::scan::token::IdentToken;
use crate::parse::expr::Expression;
use crate::visitor::Printable;

pub struct AssignExpression<'src> {
    pub name: IdentToken<'src>,
    pub value: Expression<'src>,
}

impl std::fmt::Debug for AssignExpression<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.print())
    }
}

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
