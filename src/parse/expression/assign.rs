use crate::code::Code;
use crate::code::code_span::CodeSpan;
use super::{
    Expression,
    BoxedExpression,
};

pub struct AssignExpression {
    name: String,
    value: BoxedExpression,
    code_span: CodeSpan,
}

impl AssignExpression {
    pub fn new(name: &str, value: BoxedExpression, code_span: CodeSpan) -> AssignExpression {
        AssignExpression {
            name: name.to_owned(),
            value,
            code_span,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn value(&self) -> &BoxedExpression {
        &self.value
    }
}

impl Code for AssignExpression {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Expression for AssignExpression {
    fn box_clone(&self) -> BoxedExpression {
        Box::new(
            AssignExpression::new(
                self.name(),
                self.value().clone(),
                self.code_span(),
            )
        )
    }
}

#[macro_export]
macro_rules! assign_expression {
    ( $name:expr, $value:expr, $code_span:expr ) => {
        Box::new(
            AssignExpression::new(
                $name,
                $value,
                $code_span,
            )
        )
    };
}
