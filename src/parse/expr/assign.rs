use crate::scan::span::Span;
use crate::visitor::Printable;
use crate::impl_debug_for_printable;
use super::{
    Expression,
    Expr,
};

pub struct AssignExpression {
    name: String,
    value: Expression,
    span: Span,
}

impl AssignExpression {
    pub fn new(name: &str, value: Expression, span: Span) -> AssignExpression {
        AssignExpression {
            name: name.to_owned(),
            value,
            span,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn value(&self) -> &Expression {
        &self.value
    }
}

impl Expr for AssignExpression {
    fn span(&self) -> Span {
        self.span
    }
}

impl_debug_for_printable!(AssignExpression);

#[macro_export]
macro_rules! assign_expression {
    ( $name:expr, $value:expr, $span:expr ) => {
        Box::new(
            AssignExpression::new(
                $name,
                $value,
                $span,
            )
        )
    };
}
