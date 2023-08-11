use crate::scan::span::Span;
use crate::visitor::Printable;
use crate::impl_debug_for_printable;
use super::Expr;

pub struct VariableExpression {
    name: String,
    span: Span,
}

impl VariableExpression {
    pub fn new(name: &str, span: Span) -> VariableExpression {
        VariableExpression {
            name: name.to_owned(),
            span,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

impl Expr for VariableExpression {
    fn span(&self) -> Span {
        self.span
    }
}

impl_debug_for_printable!(VariableExpression);

#[macro_export]
macro_rules! variable_expression {
    ( $name:expr, $span:expr ) => {
        Box::new(
            VariableExpression::new(
                $name,
                $span,
            )
        )
    }
}
