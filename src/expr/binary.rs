use super::{
    Expr,
    Expression,
};

pub enum BinaryExpression<'a, 'b> {
    Equal(Expression<'a>, Expression<'b>),
    NotEqual(Expression<'a>, Expression<'b>),
    Less(Expression<'a>, Expression<'b>),
    LessEqual(Expression<'a>, Expression<'b>),
    Greater(Expression<'a>, Expression<'b>),
    GreaterEqual(Expression<'a>, Expression<'b>),
    Plus(Expression<'a>, Expression<'b>),
    Minus(Expression<'a>, Expression<'b>),
    Multiply(Expression<'a>, Expression<'b>),
    Divide(Expression<'a>, Expression<'b>),
}

impl<'a> std::fmt::Debug for BinaryExpression<'a, 'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.print())
    }
}

#[macro_export]
macro_rules! binary_expression {
    ( $variant:ident, $lhs:expr, $rhs:expr ) => {
        Box::new(BinaryExpression::$variant($lhs, $rhs))
    };
}
