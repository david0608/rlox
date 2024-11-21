use std::{
    rc::Rc,
    cell::RefCell,
};
use crate::{
    code::{
        Code,
        CodeSpan,
    },
    parse::expression::Expression,
    scan::token::identifier::IdentifierToken,
    value::Value,
    environment::Environment,
    error::{
        RuntimeError,
        RuntimeErrorEnum,
    },
    resolve::{
        ResolveCtx,
        ResolveError,
    },
};

#[derive(Debug)]
pub struct SetExpression {
    object: Rc<dyn Expression>,
    name: Rc<IdentifierToken>,
    value: Rc<dyn Expression>,
    code_span: CodeSpan,
}

impl SetExpression {
    pub fn new(
        object: Rc<dyn Expression>,
        name: Rc<IdentifierToken>,
        value: Rc<dyn Expression>,
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
    fn code_span(&self) -> &CodeSpan {
        &self.code_span
    }

    fn to_string(&self) -> String {
        format!("(.{}= {} {})", self.name.name(), self.object.to_string(), self.value.to_string())
    }
}

impl Expression for SetExpression {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Rc<dyn Expression>, ResolveError> {
        Ok(
            Rc::new(
                SetExpression {
                    object: self.object.resolve(context)?,
                    name: self.name.clone(),
                    value: self.value.resolve(context)?,
                    code_span: self.code_span.clone(),
                }
            )
        )
    }

    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
        let lhs = match self.object.evaluate(env) {
            Ok(v) => v,
            Err(e) => {
                return Err(RuntimeError::wrap(e, self.code_span))
            }
        };

        let object = if let Value::Object(object) = lhs {
            object
        }
        else {
            return Err(
                RuntimeError::new(
                    RuntimeErrorEnum::CanNotSetProperty(lhs),
                    self.code_span,
                )
            );
        };

        let rhs = self.value.evaluate(env)?;
        object.borrow_mut().set(self.name.name().to_string(), rhs.clone());
        return Ok(rhs);
    }
}

#[macro_export]
macro_rules! set_expression {
    ( $object:expr, $name:expr, $value:expr, $code_span:expr ) => {
        Rc::new(
            SetExpression::new(
                $object,
                $name,
                $value,
                $code_span,
            )
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        code::{
            Code,
            CodeSpan,
        },
        parse::expression::{
            set::SetExpression,
            variable::VariableExpression,
        },
        value::Value,
        error::{
            RuntimeError,
            RuntimeErrorEnum,
        },
        utils::{
            Downcast,
            test_utils::{
                TestContext,
                parse_expression,
                parse_expression_unknown,
            },
        }
    };

    #[test]
    fn test_set_expression_print() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        let expr = ctx.resolve_expression::<SetExpression>(
            parse_expression_unknown("foo.bar = true").as_ref()
        )
            .unwrap();
        assert_eq!(expr.to_string(), "(.bar= foo true)");
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
                        CodeSpan::new(0, 1, 0, 9),
                    ),
                    CodeSpan::new(0, 0, 0, 10),
                ),
                CodeSpan::new(0, 0, 0, 21),
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
                CodeSpan::new(0, 0, 0, 15),
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
