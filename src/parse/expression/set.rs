use std::rc::Rc;
use crate::code::{
    Code,
    code_span::CodeSpan,
};
use crate::parse::expression::{
    Expression,
    AsExpression,
};
use crate::scan::token::identifier::IdentifierToken;
use crate::value::Value;
use crate::environment::Environment;
use crate::error::{
    RuntimeError,
    RuntimeErrorEnum,
};
use crate::evaluate::{
    Evaluate,
    EvaluateResult,
};
use crate::print::Print;
use crate::resolve::{
    ResolveCtx,
    ResolveError,
};
use crate::{
    impl_debug_for_printable,
    runtime_error,
};

pub struct SetExpression {
    object: Expression,
    name: IdentifierToken,
    value: Expression,
    code_span: CodeSpan,
}

impl SetExpression {
    pub fn new(
        object: Expression,
        name: IdentifierToken,
        value: Expression,
        code_span: CodeSpan,
    )
        -> SetExpression
    {
        SetExpression {
            object,
            name,
            value,
            code_span,
        }
    }
}

impl Code for SetExpression {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Print for SetExpression {
    fn print(&self) -> String {
        todo!()
    }
}

impl_debug_for_printable!(SetExpression);

impl Evaluate for SetExpression {
    fn evaluate(&self, env: &Environment) -> EvaluateResult {
        let value = self.object.evaluate(env)?;
        let object = if let Value::Object(object) = value {
            object
        }
        else {
            return Err(
                runtime_error!(
                    RuntimeErrorEnum::CanNotSetProperty(value),
                    self.code_span,
                )
            );
        };
        return Ok(
            object.set(
                self.name.name(),
                self.value.evaluate(env)?,
            )
        );
    }
}

impl AsExpression for SetExpression {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Expression, ResolveError> {
        Ok(
            Expression(
                Rc::new(
                    SetExpression {
                        object: self.object.resolve(context)?,
                        name: self.name.clone(),
                        value: self.value.resolve(context)?,
                        code_span: self.code_span.clone(),
                    }
                )
            )
        )
    }
}

#[macro_export]
macro_rules! set_expression {
    ( $object:expr, $name:expr, $value:expr, $code_span:expr ) => {
        Expression(
            Rc::new(
                SetExpression::new(
                    $object,
                    $name,
                    $value,
                    $code_span,
                )
            )
        )
    }
}
