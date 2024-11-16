use std::{
    rc::Rc,
    cell::RefCell,
};
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
    value::{
        Value,
        method::Method,
    },
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

pub struct GetExpression {
    object: Expression,
    name: Rc<IdentifierToken>,
    code_span: CodeSpan,
}

impl GetExpression {
    pub fn new(
        object: Expression,
        name: Rc<IdentifierToken>,
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

    pub fn name(&self) -> &Rc<IdentifierToken> {
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
    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
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
                    RuntimeErrorEnum::CanNotGetProperty(value),
                    self.code_span,
                )
            );
        };

        if let Some(v) = object.borrow().properties().get(self.name().name()) {
            return Ok(v.clone());
        }
        else if let Some(md) = object.borrow().class().method_definitions().get(self.name().name()) {
            return Ok(
                Value::Method(
                    Rc::new(
                        Method::new(
                            md.clone(),
                            object.clone(),
                        )
                    )
                )
            );
        }
        else {
            return Ok(Value::Nil);
        };
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
                        code_span: self.code_span,
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
        code::code_span::new_code_span,
        parse::expression::{
            get::GetExpression,
            variable::VariableExpression,
        },
        value::Value,
        print::Print,
        error::{
            RuntimeError,
            RuntimeErrorEnum,
        },
        resolve::{
            ResolveError,
            ResolveErrorEnum,
        },
        utils::test_utils::{
            TestContext,
            parse_expression,
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

    #[test]
    fn test_get_expression_evaluate() {
        let mut ctx = TestContext::new();
        ctx.execute_src("class Foo {}");
        ctx.execute_src("var foo = Foo();");
        ctx.execute_src("foo.bar = true;");
        let get_expr = ctx.resolve_expression::<GetExpression>(
            parse_expression_unknown("foo.bar").as_ref()
        )
            .unwrap();
        assert_eq!(
            ctx.evaluate(get_expr.as_ref()).unwrap(),
            Value::Bool(true)
        );
    }

    #[test]
    fn test_get_expression_evaluate_object_evaluate_error() {
        let mut ctx = TestContext::new();
        let get_expr = ctx.resolve_expression::<GetExpression>(
            parse_expression_unknown("(1 + true).foo").as_ref()
        )
            .unwrap();
        assert_eq!(
            ctx.evaluate(get_expr.as_ref()).unwrap_err(),
            RuntimeError::wrap(
                RuntimeError::wrap(
                    RuntimeError::new(
                        RuntimeErrorEnum::InvalidArithmetic(Value::Number(1.0), Value::Bool(true)),
                        new_code_span(0, 1, 0, 9),
                    ),
                    new_code_span(0, 0, 0, 10)
                ),
                new_code_span(0, 0, 0, 14),
            )
        );
    }

    #[test]
    fn test_get_expression_evaluate_can_not_get_property_error() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo = true;");
        let get_expr = ctx.resolve_expression::<GetExpression>(
            parse_expression_unknown("foo.bar").as_ref()
        )
            .unwrap();
        assert_eq!(
            ctx.evaluate(get_expr.as_ref()).unwrap_err(),
            RuntimeError::new(
                RuntimeErrorEnum::CanNotGetProperty(Value::Bool(true)),
                new_code_span(0, 0, 0, 7),
            )
        );
    }

    #[test]
    fn test_get_expression_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        let get_expr = ctx.resolve_expression::<GetExpression>(
            parse_expression_unknown("foo.bar").as_ref()
        )
            .unwrap();
        assert_eq!(
            get_expr.object().downcast_ref::<VariableExpression>().unwrap().binding(),
            0
        );
    }

    #[test]
    fn test_get_expression_resolve_object_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_expression_unknown(
                parse_expression::<GetExpression>("foo.bar").as_ref()
            )
                .unwrap_err(),
            ResolveError::new(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 0, 0, 3),
            )
        );
    }
}
