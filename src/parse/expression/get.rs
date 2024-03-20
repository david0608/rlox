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

pub struct GetExpression {
    object: Expression,
    name: IdentifierToken,
    code_span: CodeSpan,
}

impl GetExpression {
    pub fn new(
        object: Expression,
        name: IdentifierToken,
        code_span: CodeSpan,
    )
        -> GetExpression
    {
        GetExpression {
            object,
            name,
            code_span,
        }
    }

    pub fn object(&self) -> &Expression {
        &self.object
    }

    pub fn name(&self) -> &IdentifierToken {
        &self.name
    }
}

impl Code for GetExpression {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Print for GetExpression {
    fn print(&self) -> String {
        format!("(. {} {})", self.object.print(), self.name.name())
    }
}

impl_debug_for_printable!(GetExpression);

impl Evaluate for GetExpression {
    fn evaluate(&self, env: &Environment) -> EvaluateResult {
        let value = self.object.evaluate(env)?;
        let object = if let Value::Object(object) = value {
            object
        }
        else {
            return Err(
                runtime_error!(
                    RuntimeErrorEnum::CanNotGetProperty(value),
                    self.code_span,
                )
            );
        };
        return Ok(object.get(self.name.name()));
    }
}

impl AsExpression for GetExpression {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Expression, ResolveError> {
        Ok(
            Expression(
                Rc::new(
                    GetExpression {
                        object: self.object.resolve(context)?,
                        name: self.name.clone(),
                        code_span: self.code_span.clone(),
                    }
                )
            )
        )
    }
}

#[macro_export]
macro_rules! get_expression {
    ( $object:expr, $name:expr, $code_span:expr ) => {
        Expression(
            Rc::new(
                GetExpression::new(
                    $object,
                    $name,
                    $code_span,
                )
            )
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        parse::expression::get::GetExpression,
        print::Print,
        utils::test_utils::{
            TestContext,
            parse_expression_unknown,
        },
    };

    #[test]
    fn test_get_expression_print() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        let expr = ctx.resolve_expression::<GetExpression>(
            parse_expression_unknown("foo.bar").as_ref()
        )
            .unwrap();
        assert_eq!(expr.print(), "(. foo bar)");
    }
}
