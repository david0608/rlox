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
    call::{
        Call,
        CallError,
    },
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
pub struct CallExpression {
    callee: Rc<dyn Expression>,
    arguments: Vec<Rc<dyn Expression>>,
    code_span: CodeSpan,
}

impl CallExpression {
    pub fn new(
        callee: Rc<dyn Expression>,
        arguments: Vec<Rc<dyn Expression>>,
        code_span: CodeSpan,
    ) -> CallExpression
    {
        CallExpression {
            callee,
            arguments,
            code_span,
        }
    }

    pub fn callee(&self) -> &Rc<dyn Expression> {
        &self.callee
    }

    pub fn arguments(&self) -> &Vec<Rc<dyn Expression>> {
        &self.arguments
    }
}

impl Code for CallExpression {
    fn code_span(&self) -> &CodeSpan {
        &self.code_span
    }

    fn to_string(&self) -> String {
        format!(
            "(call {} {})",
            self.callee().to_string(),
            self.arguments().iter().map(|s| s.to_string()).collect::<Vec<String>>().join(" "),
        )
    }
}

impl Expression for CallExpression {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Rc<dyn Expression>, ResolveError> {
        Ok(
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
    }

    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
        let callee = match self.callee().evaluate(env) {
            Ok(val) => val,
            Err(err) => {
                return Err(RuntimeError::wrap(err, self.code_span().clone()));
            }
        };
        let mut arguments = Vec::new();
        for a in self.arguments() {
            arguments.push(
                match a.evaluate(env) {
                    Ok(val) => val,
                    Err(err) => {
                        return Err(RuntimeError::wrap(err, self.code_span().clone()));
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
                                self.code_span().clone(),
                            )
                        );
                    }
                    CallError::NotCallable => {
                        return Err(
                            RuntimeError::new(
                                RuntimeErrorEnum::NotCallable(callee),
                                self.code_span().clone(),
                            )
                        );
                    }
                    CallError::RuntimeError(err) => {
                        return Err(RuntimeError::wrap(err, self.code_span().clone()));
                    }
                }
            }
        }
    }
}

#[macro_export]
macro_rules! call_expression {
    ( $callee:expr, $arguments:expr, $code_span:expr ) => {
        Rc::new(
            CallExpression::new(
                $callee,
                $arguments,
                $code_span
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
            call::CallExpression,
            variable::VariableExpression,
        },
        value::Value,
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
                parse_expression_unknown
            }
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
                parse_expression::<CallExpression>(src).to_string(),
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
                            CodeSpan::new(0, 1, 0, 9),
                        ),
                        CodeSpan::new(0, 0, 0, 10)
                    ),
                    CodeSpan::new(0, 0, 0, 16),
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
                        CodeSpan::new(0, 4, 0, 12),
                    ),
                    CodeSpan::new(0, 0, 0, 13),
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
                    CodeSpan::new(0, 0, 0, 12),
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
                    CodeSpan::new(0, 0, 0, 5),
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
                            CodeSpan::new(2, 7, 2, 15),
                        ),
                        CodeSpan::new(2, 0, 2, 16),
                    ),
                    CodeSpan::new(0, 0, 0, 5),
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
                CodeSpan::new(0, 0, 0, 5)
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
                CodeSpan::new(0, 6, 0, 9)
            )
        );
        assert_eq!(
            ctx.resolve_expression_unknown(
                parse_expression::<CallExpression>("hello(1, bar)").as_ref()
            )
                .unwrap_err(),
            ResolveError::new(
                ResolveErrorEnum::VariableNotDeclared,
                CodeSpan::new(0, 9, 0, 12)
            )
        );
    }
}
