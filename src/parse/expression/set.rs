use std::rc::Rc;
use crate::{
    code::{
        Code,
        code_span::CodeSpan,
    },
    parse::expression::{
        Expression,
        AsExpression,
    },
    scan::token::identifier::IdentifierToken,
    value::Value,
    environment::Environment,
    error::{
        RuntimeError,
        RuntimeErrorEnum,
    },
    evaluate::Evaluate,
    print::Print,
    resolve::{
        ResolveCtx,
        ResolveError,
    },
    impl_debug_for_printable,
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
        format!("(.{}= {} {})", self.name.name(), self.object.print(), self.value.print())
    }
}

impl_debug_for_printable!(SetExpression);

impl Evaluate for SetExpression {
    fn evaluate(&self, env: &Environment) -> Result<Value, RuntimeError> {
        let value = match self.object.evaluate(env) {
            Ok(v) => v,
            Err(e) => {
                return Err(RuntimeError::wrap(e, self.code_span))
            }
        };
        let object = if let Value::Object(object) = value {
            object
        }
        else {
            return Err(
                RuntimeError::new(
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

#[cfg(test)]
mod tests {
    use crate::{
        code::code_span::new_code_span,
        parse::expression::{
            set::SetExpression,
            variable::VariableExpression,
        },
        value::Value,
        print::Print,
        error::{
            RuntimeError,
            RuntimeErrorEnum,
        },
        utils::test_utils::{
            TestContext,
            parse_expression,
            parse_expression_unknown,
        },
    };

    #[test]
    fn test_set_expression_print() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        let expr = ctx.resolve_expression::<SetExpression>(
            parse_expression_unknown("foo.bar = true").as_ref()
        )
            .unwrap();
        assert_eq!(expr.print(), "(.bar= foo true)");
    }

    #[test]
    fn test_set_expression_evaluate() {
        let mut ctx = TestContext::new();
        ctx.execute_src("class Foo {}");
        ctx.execute_src("var foo = Foo();");
        let set_expr = ctx.resolve_expression::<SetExpression>(
            parse_expression_unknown("foo.bar = true").as_ref()
        )
            .unwrap();
        assert_eq!(
            ctx.evaluate(set_expr.as_ref()).unwrap(),
            Value::Bool(true)
        );
    }

    #[test]
    fn test_set_expression_evaluate_object_evaluate_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.evaluate(
                parse_expression::<SetExpression>("(1 + true).foo = true").as_ref()
            )
                .unwrap_err(),
            RuntimeError::wrap(
                RuntimeError::wrap(
                    RuntimeError::new(
                        RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Bool(true)),
                        new_code_span(0, 1, 0, 9),
                    ),
                    new_code_span(0, 0, 0, 10),
                ),
                new_code_span(0, 0, 0, 21),
            )
        );
    }

    #[test]
    fn test_set_expression_evaluate_can_not_set_property_error() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo = true;");
        let set_expr = ctx.resolve_expression::<SetExpression>(
            parse_expression_unknown("foo.bar = false").as_ref()
        )
            .unwrap();
        assert_eq!(
            ctx.evaluate(set_expr.as_ref()).unwrap_err(),
            RuntimeError::new(
                RuntimeErrorEnum::CanNotSetProperty(Value::Bool(true)),
                new_code_span(0, 0, 0, 15),
            )
        );
    }

    #[test]
    fn test_set_expression_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        ctx.execute_src("var val;");
        let set_expr = ctx.resolve_expression::<SetExpression>(
            parse_expression_unknown("foo.bar = val").as_ref()
        )
            .unwrap();
        assert_eq!(
            set_expr.object.downcast_ref::<VariableExpression>().unwrap().binding(),
            0,
        );
        assert_eq!(
            set_expr.value.downcast_ref::<VariableExpression>().unwrap().binding(),
            0,
        );
    }
}
