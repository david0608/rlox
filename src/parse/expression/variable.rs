use crate::code::Code;
use crate::code::code_span::CodeSpan;
use super::{
    Expression,
    BoxedExpression,
};

pub struct VariableExpression {
    name: String,
    code_span: CodeSpan,
}

impl VariableExpression {
    pub fn new(name: &str, code_span: CodeSpan) -> VariableExpression {
        VariableExpression {
            name: name.to_owned(),
            code_span,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

impl Code for VariableExpression {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Expression for VariableExpression {
    fn box_clone(&self) -> BoxedExpression {
        Box::new(
            VariableExpression::new(
                self.name(),
                self.code_span(),
            )
        )
    }
}

#[macro_export]
macro_rules! variable_expression {
    ( $name:expr, $code_span:expr ) => {
        Box::new(
            VariableExpression::new(
                $name,
                $code_span,
            )
        )
    }
}
