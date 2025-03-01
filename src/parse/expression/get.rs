use std::{
    rc::Rc,
    cell::RefCell,
    collections::HashSet,
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
        ResolveError,
    },
};

#[derive(Debug)]
pub struct GetExpression {
    object: Rc<dyn Expression>,
    name: Rc<IdentifierToken>,
    code_span: CodeSpan,
}

impl GetExpression {
    pub fn new(
        object: Rc<dyn Expression>,
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

    pub fn object(&self) -> &Rc<dyn Expression> {
        &self.object
    }

    pub fn name(&self) -> &Rc<IdentifierToken> {
        &self.name
    }
}

impl Code for GetExpression {
    fn code_span(&self) -> &CodeSpan {
        &self.code_span
    }

    fn to_string(&self) -> String {
        format!("(. {} {})", self.object.to_string(), self.name.name())
    }
}

impl Expression for GetExpression {
    fn resolve(&self, context: &mut Vec<HashSet<String>>) -> Result<Rc<dyn Expression>, ResolveError> {
        Ok(
            Rc::new(
                GetExpression {
                    object: self.object.resolve(context)?,
                    name: self.name.clone(),
                    code_span: self.code_span,
                }
            )
        )
    }

    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
        let object = match self.object.evaluate(env) {
            Ok(v) => v,
            Err(e) => {
                return Err(RuntimeError::wrap(e, self.code_span))
            }
        };

        return object.get(self.name().name())
            .map_err(
                |_| RuntimeError::new(RuntimeErrorEnum::CanNotGetProperty(object), self.code_span)
            );
    }
}

#[macro_export]
macro_rules! get_expression {
    ( $object:expr, $name:expr, $code_span:expr ) => {
        Rc::new(
            GetExpression::new(
                $object,
                $name,
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
            get::GetExpression,
            variable::{
                VariableExpressionNotResolved,
                VariableExpression,
            },
        },
        value::Value,
        error::{
            RuntimeError,
            RuntimeErrorEnum,
            ResolveError,
            ResolveErrorEnum,
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
    fn test_get_expression_print() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        let expr = ctx.resolve_expression::<GetExpression>(
            parse_expression_unknown("foo.bar").as_ref()
        )
            .unwrap();
        assert_eq!(expr.to_string(), "(. foo bar)");
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

        let mut ctx = TestContext::new();
        ctx.execute_src(
            "
            class Foo {
                hello() {
                    print \"hello!\";
                }
            }
            "
        );
        ctx.execute_src(
            "
            class Bar > Foo {}
            "
        );
        ctx.execute_src("var bar = Bar();");
        let hello_method = if let Value::Method(m) = ctx.evaluate(
            parse_expression::<GetExpression>("bar.hello").as_ref()
        )
            .unwrap()
        {
            m
        }
        else {
            panic!("Expect method value.");
        };
        let bar_object = if let Value::Object(o) = ctx.evaluate(
            parse_expression::<VariableExpressionNotResolved>("bar").as_ref()
        )
            .unwrap()
        {
            o
        }
        else {
            panic!("Expect object value.");
        };
        assert_eq!(
            hello_method.this(),
            &bar_object,
        );
        let foo_class = if let Value::Class(c) = ctx.evaluate(
            parse_expression::<VariableExpressionNotResolved>("Foo").as_ref()
        )
            .unwrap()
        {
            c
        }
        else {
            panic!("Expect class value.");
        };
        assert_eq!(
            hello_method.definition(),
            &foo_class.method_definition("hello").unwrap(),
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
                        CodeSpan::new(0, 1, 0, 9),
                    ),
                    CodeSpan::new(0, 0, 0, 10)
                ),
                CodeSpan::new(0, 0, 0, 14),
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
                CodeSpan::new(0, 0, 0, 7),
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
                CodeSpan::new(0, 0, 0, 3),
            )
        );
    }
}
