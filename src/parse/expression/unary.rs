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

#[derive(Clone, Copy, Debug)]
pub enum UnaryExpressionEnum {
    Negative,
    Not
}

#[derive(Debug)]
pub struct UnaryExpression {
    variant: UnaryExpressionEnum,
    rhs: Rc<dyn Expression>,
    code_span: CodeSpan,
}

impl UnaryExpression {
    pub fn new(
        variant: UnaryExpressionEnum, 
        rhs: Rc<dyn Expression>,
        code_span: CodeSpan,
    ) -> UnaryExpression
    {
        UnaryExpression {
            variant,
            rhs,
            code_span,
        }
    }

    pub fn variant(&self) -> UnaryExpressionEnum {
        self.variant
    }

    pub fn rhs(&self) -> &Rc<dyn Expression> {
        &self.rhs
    }
}

impl Code for UnaryExpression {
    fn code_span(&self) -> &CodeSpan {
        &self.code_span
    }

    fn to_string(&self) -> String {
        match self.variant() {
            UnaryExpressionEnum::Negative => format!("(- {})", self.rhs().to_string()),
            UnaryExpressionEnum::Not => format!("(! {})", self.rhs().to_string()),
        }
    }
}

impl Expression for UnaryExpression {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Rc<dyn Expression>, ResolveError> {
        Ok(
            Rc::new(
                UnaryExpression::new(
                    self.variant,
                    self.rhs.resolve(context)?,
                    self.code_span.clone(),
                )
            )
        )
    }

    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
        let rhs = match self.rhs().evaluate(env) {
            Ok(val) => val,
            Err(err) => {
                return Err(RuntimeError::wrap(err, self.code_span().clone()));
            }
        };
        match self.variant() {
            UnaryExpressionEnum::Negative => {
                match rhs {
                    Value::Nil
                    | Value::Bool(_)
                    | Value::String(_)
                    | Value::Function(_)
                    | Value::NativeFunction(_)
                    | Value::Class(_)
                    | Value::Object(_)
                    | Value::Method(_) => {
                        Err(
                            RuntimeError::new(
                                RuntimeErrorEnum::InvalidNegate(rhs),
                                self.code_span().clone(),
                            )
                        )
                    }
                    Value::Number(rhs) => {
                        Ok(Value::Number(-rhs))
                    }
                }
            }
            UnaryExpressionEnum::Not => {
                Ok(Value::Bool(!rhs.is_truthy()))
            }
        }
    }
}

#[macro_export]
macro_rules! unary_expression {
    ( $variant:ident, $rhs:expr, $code_span:expr ) => {
        Rc::new(
            UnaryExpression::new(
                UnaryExpressionEnum::$variant,
                $rhs,
                $code_span,
            )
        )
    };
}

#[cfg(test)]
mod tests {
    use crate::{
        code::{
            Code,
            CodeSpan,
        },
        native::add_native_clock,
        parse::expression::{
            unary::UnaryExpression,
            variable::VariableExpression,
        },
        value::Value,
        environment::EnvironmentT,
        error::{
            RuntimeError,
            RuntimeErrorEnum,
        },
        resolve::{
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
    fn test_unary_expression_print() {
        let tests: Vec<(&str, &str)> = vec![
            ("-123", "(- 123)"),
            ("!123", "(! 123)"),
        ];
        for (src, expect) in tests {
            assert_eq!(parse_expression::<UnaryExpression>(src).to_string(), expect);
        }
    }

    #[test]
    fn test_unary_expression_evaluate() {
        let mut ctx = TestContext::new();
        add_native_clock(&mut ctx.resolve_context, &ctx.environment);
        ctx.execute_src(
            "
            fun foo() {
                print \"hello\";
            }
            "
        );
        assert_eq!(
            ctx.evaluate(
                parse_expression::<UnaryExpression>("-nil").as_ref()
            ),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidNegate(Value::Nil),
                    CodeSpan::new(0, 0, 0, 4),
                )
            ),
        );
        assert_eq!(
            ctx.evaluate(
                parse_expression::<UnaryExpression>("-true").as_ref()
            ),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidNegate(Value::Bool(true)),
                    CodeSpan::new(0, 0, 0, 5),
                )
            ),
        );
        assert_eq!(
            ctx.evaluate(
                parse_expression::<UnaryExpression>("-\"hello\"").as_ref()
            ),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidNegate(Value::String("hello".to_owned())),
                    CodeSpan::new(0, 0, 0, 8),
                )
            ),
        );
        assert_eq!(
            ctx.evaluate(
                parse_expression::<UnaryExpression>("-foo").as_ref()
            ),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidNegate(ctx.environment.get("foo", 0).unwrap()),
                    CodeSpan::new(0, 0, 0, 4),
                )
            ),
        );
        assert_eq!(
            ctx.evaluate(
                parse_expression::<UnaryExpression>("-clock").as_ref()
            ),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidNegate(ctx.environment.get("clock", 0).unwrap()),
                    CodeSpan::new(0, 0, 0, 6),
                )
            ),
        );
        assert_eq!(
            ctx.evaluate(
                parse_expression::<UnaryExpression>("-1.0").as_ref()
            ),
            Ok(Value::Number(-1.0)),
        );
        assert_eq!(
            ctx.evaluate(
                parse_expression::<UnaryExpression>("!true").as_ref()
            ),
            Ok(Value::Bool(false)),
        );
        assert_eq!(
            ctx.evaluate(
                parse_expression::<UnaryExpression>("!false").as_ref()
            ),
            Ok(Value::Bool(true)),
        );
        assert_eq!(
            ctx.evaluate(
                parse_expression::<UnaryExpression>("!1.0").as_ref()
            ),
            Ok(Value::Bool(false)),
        );
        assert_eq!(
            ctx.evaluate(
                parse_expression::<UnaryExpression>("!0.0").as_ref()
            ),
            Ok(Value::Bool(true)),
        );
        assert_eq!(
            ctx.evaluate(
                parse_expression::<UnaryExpression>("!\"hello\"").as_ref()
            ),
            Ok(Value::Bool(false)),
        );
        assert_eq!(
            ctx.evaluate(
                parse_expression::<UnaryExpression>("!\"\"").as_ref()
            ),
            Ok(Value::Bool(true)),
        );
    }

    #[test]
    fn test_unary_expression_evaluate_right_evaluate_error() {
        let mut ctx = TestContext::new();
        let expr = parse_expression::<UnaryExpression>("!(true + 1)");
        assert_eq!(
            ctx.evaluate(expr.as_ref()),
            Err(
                RuntimeError::wrap(
                    RuntimeError::wrap(
                        RuntimeError::new(
                            RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                            CodeSpan::new(0, 2, 0, 10),
                        ),
                        CodeSpan::new(0, 1, 0, 11)
                    ),
                    CodeSpan::new(0, 0, 0, 11),
                )
            )
        );
    }

    #[test]
    fn test_unary_expression_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");

        let unary_expr = ctx.resolve_expression::<UnaryExpression>(
            parse_expression_unknown("!foo").as_ref()
        )
            .unwrap();
        assert_eq!(
            unary_expr.rhs().downcast_ref::<VariableExpression>().unwrap().binding(),
            0
        );
    }

    #[test]
    fn test_unary_expression_resolve_rhs_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_expression_unknown(
                parse_expression::<UnaryExpression>("!foo").as_ref()
            )
                .unwrap_err(),
            ResolveError::new(
                ResolveErrorEnum::VariableNotDeclared,
                CodeSpan::new(0, 1, 0, 4)
            )
        );
    }
}
