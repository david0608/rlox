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
    call::{
        Call,
        CallError,
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

pub struct CallExpression {
    callee: Expression,
    arguments: Vec<Expression>,
    code_span: CodeSpan,
}

impl CallExpression {
    pub fn new(
        callee: Expression,
        arguments: Vec<Expression>,
        code_span: CodeSpan,
    ) -> CallExpression
    {
        CallExpression {
            callee,
            arguments,
            code_span,
        }
    }

    pub fn callee(&self) -> &Expression {
        &self.callee
    }

    pub fn arguments(&self) -> &Vec<Expression> {
        &self.arguments
    }
}

impl Code for CallExpression {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Print for CallExpression {
    fn print(&self) -> String {
        format!(
            "(call {} {})",
            self.callee().print(),
            self.arguments().iter().map(|s| s.print()).collect::<Vec<String>>().join(" "),
        )
    }
}

impl_debug_for_printable!(CallExpression);

impl Evaluate for CallExpression {
    fn evaluate(&self, env: &Environment) -> Result<Value, RuntimeError> {
        let callee = match self.callee().evaluate(env) {
            Ok(val) => val,
            Err(err) => {
                return Err(RuntimeError::wrap(err, self.code_span()));
            }
        };
        let mut arguments = Vec::new();
        for a in self.arguments() {
            arguments.push(
                match a.evaluate(env) {
                    Ok(val) => val,
                    Err(err) => {
                        return Err(RuntimeError::wrap(err, self.code_span()));
                    }
                }
            );
        }
        match callee.call(arguments) {
            Ok(v) => {
                return Ok(v);
            }
            Err(err) => {
                match err {
                    CallError::ArgumentNumberMismatch(ec, pc) => {
                        return Err(
                            RuntimeError::new(
                                RuntimeErrorEnum::ArgumentNumberMismatch(ec, pc),
                                self.code_span(),
                            )
                        );
                    }
                    CallError::NotCallable => {
                        return Err(
                            RuntimeError::new(
                                RuntimeErrorEnum::NotCallable(callee),
                                self.code_span(),
                            )
                        );
                    }
                    CallError::RuntimeError(err) => {
                        return Err(RuntimeError::wrap(err, self.code_span()));
                    }
                }
            }
        }
    }
}

impl AsExpression for CallExpression {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Expression, ResolveError> {
        Ok(
            Expression(
                Rc::new(
                    CallExpression::new(
                        self.callee.resolve(context)?,
                        self.arguments.iter().try_fold(
                            Vec::new(),
                            |mut args, a| {
                                args.push(a.resolve(context)?);
                                Ok(args)
                            },
                        )?,
                        self.code_span.clone(),
                    )
                )
            )
        )
    }
}

#[macro_export]
macro_rules! call_expression {
    ( $callee:expr, $arguments:expr, $code_span:expr ) => {
        Expression(
            Rc::new(
                CallExpression::new(
                    $callee,
                    $arguments,
                    $code_span
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
            call::CallExpression,
            variable::VariableExpression,
        },
        value::Value,
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
            parse_expression_unknown
        }
    };

    #[test]
    fn test_call_expression_print() {
        let tests: Vec<(&str, &str)> = vec![
            ("foo()", "(call foo )"),
            ("bar(foo, true, 123)", "(call bar foo true 123)"),
        ];
        for (src, expect) in tests {
            assert_eq!(
                parse_expression::<CallExpression>(src).print(),
                expect
            );
        }
    }

    #[test]
    fn test_call_expression_evaluate() {
        let mut ctx = TestContext::new();
        ctx.execute_src(
            "
            fun foo(a, b) {
                return a + b;
            }
            "
        );
        assert_eq!(
            ctx.evaluate(parse_expression::<CallExpression>("foo(1, 2)").as_ref()),
            Ok(Value::Number(3.0))
        );
    }

    #[test]
    fn test_call_expression_evaluate_callee_evaluate_error() {
        let mut ctx = TestContext::new();
        let expr = ctx.resolve_expression::<CallExpression>(
            parse_expression_unknown("(true + 1)(1, 2)").as_ref()
        )
            .unwrap();
        assert_eq!(
            ctx.evaluate(expr.as_ref()),
            Err(
                RuntimeError::wrap(
                    RuntimeError::wrap(
                        RuntimeError::new(
                            RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                            new_code_span(0, 1, 0, 9),
                        ),
                        new_code_span(0, 0, 0, 10)
                    ),
                    new_code_span(0, 0, 0, 16),
                )
            )
        );
    }

    #[test]
    fn test_call_expression_evaluate_argument_evaluate_error() {
        let mut ctx = TestContext::new();
        ctx.execute_src("fun foo(a) { }");
        let expr = ctx.resolve_expression::<CallExpression>(
            parse_expression_unknown("foo(true + 1);").as_ref()
        )
            .unwrap();
        assert_eq!(
            ctx.evaluate(expr.as_ref()),
            Err(
                RuntimeError::wrap(
                    RuntimeError::new(
                        RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                        new_code_span(0, 4, 0, 12),
                    ),
                    new_code_span(0, 0, 0, 13),
                )
            )
        );
    }

    #[test]
    fn test_call_expression_evaluate_argument_number_mismatch_error() {
        let mut ctx = TestContext::new();
        ctx.execute_src(
            "
            fun foo(a, b) {
                return a + b;
            }
            "
        );
        assert_eq!(
            ctx.evaluate(parse_expression::<CallExpression>("foo(1, 2, 3)").as_ref()),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::ArgumentNumberMismatch(2, 3),
                    new_code_span(0, 0, 0, 12),
                )
            )
        );
    }

    #[test]
    fn test_call_expression_evaluate_not_callable_error() {
        let mut ctx = TestContext::new();
        ctx.execute_src(
            "
            var foo = true;
            "
        );
        assert_eq!(
            ctx.evaluate(parse_expression::<CallExpression>("foo()").as_ref()),
            Err(
                RuntimeError::new(
                    RuntimeErrorEnum::NotCallable(Value::Bool(true)),
                    new_code_span(0, 0, 0, 5),
                )
            ),
        );
    }

    #[test]
    fn test_call_expression_evaluate_call_error() {
        let mut ctx = TestContext::new();
        ctx.execute_src(
            "
            fun foo() {
                return true + 1;
            }
            "
        );
        let expr = ctx.resolve_expression::<CallExpression>(
            parse_expression_unknown("foo()").as_ref()
        )
            .unwrap();
        assert_eq!(
            ctx.evaluate(expr.as_ref()),
            Err(
                RuntimeError::wrap(
                    RuntimeError::wrap(
                        RuntimeError::new(
                            RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                            new_code_span(2, 7, 2, 15),
                        ),
                        new_code_span(2, 0, 2, 16),
                    ),
                    new_code_span(0, 0, 0, 5),
                )
            )
        );
    }

    #[test]
    fn test_call_expression_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        ctx.resolve_context.begin();
        ctx.execute_src("var bar;");
        ctx.resolve_context.begin();
        ctx.execute_src("fun hello(a, b) { return a + b; }");

        let call_expr = ctx.resolve_expression::<CallExpression>(
            parse_expression_unknown("hello(foo, bar)").as_ref()
        )
            .unwrap();
        assert_eq!(
            call_expr.callee().downcast_ref::<VariableExpression>().unwrap().binding(),
            0
        );
        assert_eq!(
            call_expr.arguments()[0].downcast_ref::<VariableExpression>().unwrap().binding(),
            2
        );
        assert_eq!(
            call_expr.arguments()[1].downcast_ref::<VariableExpression>().unwrap().binding(),
            1
        );
    }

    #[test]
    fn test_call_expression_resolve_callee_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_expression_unknown(
                parse_expression::<CallExpression>("hello(foo, bar)").as_ref()
            )
                .unwrap_err(),
            ResolveError::new(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 0, 0, 5)
            )
        );
    }

    #[test]
    fn test_call_expression_resolve_argument_resolve_error() {
        let mut ctx = TestContext::new();
        ctx.execute_src("fun hello(a, b) { return a + b; }");
        assert_eq!(
            ctx.resolve_expression_unknown(
                parse_expression::<CallExpression>("hello(foo, bar)").as_ref()
            )
                .unwrap_err(),
            ResolveError::new(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 6, 0, 9)
            )
        );
        assert_eq!(
            ctx.resolve_expression_unknown(
                parse_expression::<CallExpression>("hello(1, bar)").as_ref()
            )
                .unwrap_err(),
            ResolveError::new(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 9, 0, 12)
            )
        );
    }
}
