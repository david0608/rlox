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

#[derive(Clone, Copy)]
pub enum UnaryExpressionEnum {
    Negative,
    Not
}

pub struct UnaryExpression {
    variant: UnaryExpressionEnum,
    rhs: Expression,
    code_span: CodeSpan,
}

impl UnaryExpression {
    pub fn new(
        variant: UnaryExpressionEnum, 
        rhs: Expression,
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

    pub fn rhs(&self) -> &Expression {
        &self.rhs
    }
}

impl Code for UnaryExpression {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Print for UnaryExpression {
    fn print(&self) -> String {
        match self.variant() {
            UnaryExpressionEnum::Negative => format!("(- {})", self.rhs().print()),
            UnaryExpressionEnum::Not => format!("(! {})", self.rhs().print()),
        }
    }
}

impl_debug_for_printable!(UnaryExpression);

impl Evaluate for UnaryExpression {
    fn evaluate(&self, env: &Environment) -> Result<Value, RuntimeError> {
        let rhs = match self.rhs().evaluate(env) {
            Ok(val) => val,
            Err(err) => {
                return Err(RuntimeError::wrap(err, self.code_span()));
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
                                self.code_span(),
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

impl AsExpression for UnaryExpression {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Expression, ResolveError> {
        Ok(
            Expression(
                Rc::new(
                    UnaryExpression::new(
                        self.variant,
                        self.rhs.resolve(context)?,
                        self.code_span.clone(),
                    )
                )
            )
        )
    }
}

#[macro_export]
macro_rules! unary_expression {
    ( $variant:ident, $rhs:expr, $code_span:expr ) => {
        Expression(
            Rc::new(
                UnaryExpression::new(
                    UnaryExpressionEnum::$variant,
                    $rhs,
                    $code_span,
                )
            )
        )
    };
}

#[cfg(test)]
mod tests {
    use crate::{
        code::code_span::new_code_span,
        native::add_native_clock,
        parse::expression::{
            unary::UnaryExpression,
            variable::VariableExpression,
        },
        value::Value,
        environment::EnvironmentOps,
        error::{
            RuntimeError,
            RuntimeErrorEnum,
        },
        print::Print,
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
    fn test_unary_expression_print() {
        let tests: Vec<(&str, &str)> = vec![
            ("-123", "(- 123)"),
            ("!123", "(! 123)"),
        ];
        for (src, expect) in tests {
            assert_eq!(parse_expression::<UnaryExpression>(src).print(), expect);
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
                    new_code_span(0, 0, 0, 4),
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
                    new_code_span(0, 0, 0, 5),
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
                    new_code_span(0, 0, 0, 8),
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
                    new_code_span(0, 0, 0, 4),
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
                    new_code_span(0, 0, 0, 6),
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
                            new_code_span(0, 2, 0, 10),
                        ),
                        new_code_span(0, 1, 0, 11)
                    ),
                    new_code_span(0, 0, 0, 11),
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
                new_code_span(0, 1, 0, 4)
            )
        );
    }
}
