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
    environment::{
        Environment,
        EnvironmentT,
    },
    error::{
        RuntimeError,
        ResolveError,
        ResolveErrorEnum,
    },
    resolve_context::ResolveContext,
};

#[derive(Debug)]
pub struct AssignExpressionNotResolved {
    to: Rc<IdentifierToken>,
    value: Rc<dyn Expression>,
    code_span: CodeSpan,
}

impl AssignExpressionNotResolved {
    pub fn new(
        to: Rc<IdentifierToken>,
        value: Rc<dyn Expression>,
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

    pub fn value(&self) -> &Rc<dyn Expression> {
        &self.value
    }
}

impl Code for AssignExpressionNotResolved {
    fn code_span(&self) -> &CodeSpan {
        &self.code_span
    }

    fn to_string(&self) -> String {
        format!("(= {} {})", self.to().name(), self.value.to_string())
    }
}

impl Expression for AssignExpressionNotResolved {
    fn resolve(&self, context: &mut Vec<HashSet<String>>) -> Result<Rc<dyn Expression>, ResolveError> {
        if let Some(d) = context.find(self.to.name()) {
            return Ok(
                Rc::new(
                    AssignExpression::new(
                        self.to.clone(),
                        self.value.resolve(context)?,
                        self.code_span.clone(),
                        d,
                    )
                )
            );
        }
        else {
            return Err(
                ResolveError::new(
                    ResolveErrorEnum::VariableNotDeclared,
                    self.to.code_span().clone()
                )
            );
        }
    }

    fn evaluate(&self, _: &Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
        panic!("AssignExpressionNotResolved can not evaluate.");
    }
}

#[macro_export]
macro_rules! assign_expression_not_resolved {
    ( $to:expr, $value:expr, $code_span:expr ) => {
        Rc::new(
            AssignExpressionNotResolved::new(
                $to,
                $value,
                $code_span
            )
        )
    };
}

#[derive(Debug)]
pub struct AssignExpression {
    to: Rc<IdentifierToken>,
    value: Rc<dyn Expression>,
    code_span: CodeSpan,
    binding: usize,
}

impl AssignExpression {
    pub fn new(
        to: Rc<IdentifierToken>,
        value: Rc<dyn Expression>,
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

    pub fn value(&self) -> &Rc<dyn Expression> {
        &self.value
    }

    pub fn binding(&self) -> usize {
        self.binding
    }
}

impl Code for AssignExpression {
    fn code_span(&self) -> &CodeSpan {
        &self.code_span
    }

    fn to_string(&self) -> String {
        format!("(= {} {})", self.to().name(), self.value().to_string())
    }
}

impl Expression for AssignExpression {
    fn resolve(&self, context: &mut Vec<HashSet<String>>) -> Result<Rc<dyn Expression>, ResolveError> {
        Ok(
            Rc::new(
                AssignExpression::new(
                    self.to.clone(),
                    self.value.resolve(context)?,
                    self.code_span.clone(),
                    self.binding,
                )
            )
        )
    }

    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
        let v = match self.value().evaluate(env) {
            Ok(val) => val,
            Err(err) => {
                return Err(RuntimeError::wrap(err, self.code_span().clone()));
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
        code::{
            Code,
            CodeSpan,
        },
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
            ResolveError,
            ResolveErrorEnum,
        },
        resolve_context::ResolveContext,
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
    fn test_assign_expression_not_resolved_print() {
        assert_eq!(
            parse_expression::<AssignExpressionNotResolved>("foo = true").to_string(),
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
                CodeSpan::new(0, 0, 0, 3)
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
                CodeSpan::new(0, 6, 0, 9)
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
                .to_string(),
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
                    CodeSpan::new(0, 6, 0, 14),
                ),
                CodeSpan::new(0, 0, 0, 14),
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
