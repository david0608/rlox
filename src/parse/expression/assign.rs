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
    value::Value,
    environment::{
        Environment,
        EnvironmentT,
    },
    error::RuntimeError,
    evaluate::Evaluate,
    print::Print,
    resolve::{
        ResolveCtx,
        ResolveError,
        ResolveErrorEnum,
    },
    impl_debug_for_printable,
};

pub struct AssignExpressionNotResolved {
    to: Rc<IdentifierToken>,
    value: Expression,
    code_span: CodeSpan,
}

impl AssignExpressionNotResolved {
    pub fn new(
        to: Rc<IdentifierToken>,
        value: Expression,
        code_span: CodeSpan,
    ) -> AssignExpressionNotResolved
    {
        AssignExpressionNotResolved {
            to,
            value,
            code_span,
        }
    }

    pub fn to(&self) -> &IdentifierToken {
        &self.to
    }

    pub fn value(&self) -> &Expression {
        &self.value
    }
}

impl Code for AssignExpressionNotResolved {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Print for AssignExpressionNotResolved {
    fn print(&self) -> String {
        format!("(= {} {})", self.to().name(), self.value().print())
    }
}

impl_debug_for_printable!(AssignExpressionNotResolved);

impl Evaluate for AssignExpressionNotResolved {
    fn evaluate(&self, _: &Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
        panic!("AssignExpressionNotResolved can not evaluate.");
    }
}

impl AsExpression for AssignExpressionNotResolved {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Expression, ResolveError> {
        if let Some(d) = context.find(self.to.name()) {
            return Ok(
                Expression(
                    Rc::new(
                        AssignExpression::new(
                            self.to.clone(),
                            self.value.resolve(context)?,
                            self.code_span.clone(),
                            d,
                        )
                    )
                )
            );
        }
        else {
            return Err(
                ResolveError::new(
                    ResolveErrorEnum::VariableNotDeclared,
                    self.to.code_span()
                )
            );
        }
    }
}

#[macro_export]
macro_rules! assign_expression_not_resolved {
    ( $to:expr, $value:expr, $code_span:expr ) => {
        Expression(
            Rc::new(
                AssignExpressionNotResolved::new(
                    $to,
                    $value,
                    $code_span
                )
            )
        )
    };
}

pub struct AssignExpression {
    to: Rc<IdentifierToken>,
    value: Expression,
    code_span: CodeSpan,
    binding: usize,
}

impl AssignExpression {
    pub fn new(
        to: Rc<IdentifierToken>,
        value: Expression,
        code_span: CodeSpan,
        binding: usize,
    ) -> AssignExpression
    {
        AssignExpression {
            to,
            value,
            code_span,
            binding,
        }
    }

    pub fn to(&self) -> &IdentifierToken {
        &self.to
    }

    pub fn value(&self) -> &Expression {
        &self.value
    }

    pub fn binding(&self) -> usize {
        self.binding
    }
}

impl Code for AssignExpression {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Print for AssignExpression {
    fn print(&self) -> String {
        format!("(= {} {})", self.to().name(), self.value().print())
    }
}

impl_debug_for_printable!(AssignExpression);

impl Evaluate for AssignExpression {
    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
        let v = match self.value().evaluate(env) {
            Ok(val) => val,
            Err(err) => {
                return Err(RuntimeError::wrap(err, self.code_span()));
            }
        };
        if env.set(
            self.to().name(),
            self.binding(),
            v.clone(),
        ).is_ok()
        {
            return Ok(v);
        }
        else {
            unreachable!("Variable not declared should not be runtime error.");
        }
    }
}

impl AsExpression for AssignExpression {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Expression, ResolveError> {
        Ok(
            Expression(
                Rc::new(
                    AssignExpression::new(
                        self.to.clone(),
                        self.value.resolve(context)?,
                        self.code_span.clone(),
                        self.binding,
                    )
                )
            )
        )
    }
}

#[macro_export]
macro_rules! assign_expression {
    ( $to:expr, $value:expr, $code_span:expr, $binding:expr ) => {
        Expression(
            Rc::new(
                AssignExpression::new(
                    $to,
                    $value,
                    $code_span,
                    $binding
                )
            )
        )
    };
}

#[cfg(test)]
mod tests {
    use crate::{
        code::code_span::new_code_span,
        parse::expression::{
            assign::{
                AssignExpressionNotResolved,
                AssignExpression,
            },
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
    fn test_assign_expression_not_resolved_print() {
        assert_eq!(
            parse_expression::<AssignExpressionNotResolved>("foo = true").print(),
            "(= foo true)"
        );
    }

    #[test]
    #[should_panic]
    fn test_assign_expression_not_resolved_evaluate_panic() {
        let mut ctx = TestContext::new();
        let _ = ctx.evaluate(
            parse_expression::<AssignExpressionNotResolved>("foo = true").as_ref()
        );
    }

    #[test]
    fn test_assign_expression_not_resolved_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        ctx.resolve_context.begin();
        ctx.execute_src("var bar;");

        let assign_expr = ctx.resolve_expression::<AssignExpression>(
            parse_expression::<AssignExpressionNotResolved>("foo = true").as_ref()
        )
            .unwrap();
        assert_eq!(assign_expr.binding(), 1);

        let assign_expr = ctx.resolve_expression::<AssignExpression>(
            parse_expression::<AssignExpressionNotResolved>("bar = foo").as_ref()
        )
            .unwrap();
        assert_eq!(assign_expr.binding(), 0);

        let var_expr = assign_expr.value.downcast_ref::<VariableExpression>().unwrap();
        assert_eq!(var_expr.binding(), 1);
    }

    #[test]
    fn test_assign_expression_not_resolved_resolve_variable_not_found_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_expression_unknown(
                parse_expression::<AssignExpressionNotResolved>("foo = true").as_ref()
            )
                .unwrap_err(),
            ResolveError::new(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 0, 0, 3)
            )
        );
    }

    #[test]
    fn test_assign_expression_not_resolved_resolve_value_resolve_error() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        assert_eq!(
            ctx.resolve_expression_unknown(
                parse_expression::<AssignExpressionNotResolved>("foo = bar").as_ref()
            )
                .unwrap_err(),
            ResolveError::new(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 6, 0, 9)
            )
        );
    }

    #[test]
    fn test_assign_expression_print() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo = false;");
        assert_eq!(
            ctx.resolve_expression::<AssignExpression>(
                parse_expression_unknown("foo = true").as_ref()
            )
                .unwrap()
                .print(),
            "(= foo true)",
        );
    }

    #[test]
    fn test_assign_expression_evaluate() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo = false;");
        let expr = ctx.resolve_expression::<AssignExpression>(
            parse_expression_unknown("foo = true").as_ref()
        )
            .unwrap();
        assert_eq!(
            ctx.evaluate(expr.as_ref()),
            Ok(Value::Bool(true))
        );
    }

    #[test]
    fn test_assign_expression_evaluate_value_evaluate_error() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        let expr = ctx.resolve_expression::<AssignExpression>(
            parse_expression_unknown("foo = 1 < true").as_ref()
        )
            .unwrap();
        assert_eq!(
            ctx.evaluate(expr.as_ref()).unwrap_err(),
            RuntimeError::wrap(
                RuntimeError::new(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(1.0), Value::Bool(true)),
                    new_code_span(0, 6, 0, 14),
                ),
                new_code_span(0, 0, 0, 14),
            )
        );
    }

    #[test]
    #[should_panic]
    fn test_assign_expression_evaluate_var_not_declared_panic() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        ctx.execute_src("var bar;");
        let expr = ctx.resolve_expression::<AssignExpression>(
            parse_expression_unknown("foo = bar").as_ref()
        )
            .unwrap();
        let mut ctx = TestContext::new();
        ctx.execute_src("var bar;");
        ctx.evaluate(expr.as_ref()).expect("evaluate");
    }

    #[test]
    fn test_assign_expression_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        ctx.resolve_context.begin();
        ctx.execute_src("var bar;");
        let assign_expr = ctx.resolve_expression::<AssignExpression>(
            parse_expression::<AssignExpressionNotResolved>("bar = foo").as_ref()
        )
            .unwrap();
        let assign_expr = ctx.resolve_expression::<AssignExpression>(assign_expr.as_ref()).unwrap();
        assert_eq!(assign_expr.binding(), 0);
        let var_expr = assign_expr.value.downcast_ref::<VariableExpression>().unwrap();
        assert_eq!(var_expr.binding(), 1);
    }
}
