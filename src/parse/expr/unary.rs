use crate::visitor::Printable;
use super::Expression;

pub enum UnaryExpression<'src> {
    Negative(Expression<'src>),
    Not(Expression<'src>)
}

impl std::fmt::Debug for UnaryExpression<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.print())
    }
}

#[macro_export]
macro_rules! unary_expression {
    ( $variant:ident, $rhs:expr ) => {
        Box::new(UnaryExpression::$variant($rhs))
    };
}
