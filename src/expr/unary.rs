use super::{
    Expr,
    Expression,
};

pub enum UnaryExpression<'a> {
    Negative(Expression<'a>),
    Not(Expression<'a>)
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
