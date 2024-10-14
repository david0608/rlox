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
    environment::{
        Environment,
        EnvironmentOps,
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

pub struct VariableExpressionNotResolved {
    from: IdentifierToken,
}

impl VariableExpressionNotResolved {
    pub fn new(
        from: IdentifierToken,
    ) -> VariableExpressionNotResolved
    {
        VariableExpressionNotResolved {
            from,
        }
    }

    pub fn from(&self) -> &IdentifierToken {
        &self.from
    }
}

impl Code for VariableExpressionNotResolved {
    fn code_span(&self) -> CodeSpan {
        self.from.code_span()
    }
}

impl Print for VariableExpressionNotResolved {
    fn print(&self) -> String {
        self.from().name().to_string()
    }
}

impl_debug_for_printable!(VariableExpressionNotResolved);

impl Evaluate for VariableExpressionNotResolved {
    fn evaluate(&self, _: &Environment) -> Result<Value, RuntimeError> {
        panic!("VariableExpressionNotResolved can not evaluate");
    }
}

impl AsExpression for VariableExpressionNotResolved {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Expression, ResolveError> {
        let binding = if let Some(d) = context.find(self.from.name()) {
            d
        }
        else {
            return Err(
                ResolveError::new(
                    ResolveErrorEnum::VariableNotDeclared,
                    self.from.code_span()
                )
            );
        };

        Ok(
            Expression(
                Rc::new(
                    VariableExpression::new(
                        self.from.clone(),
                        binding,
                    )
                )
            )
        )
    }
}

#[macro_export]
macro_rules! variable_expression_not_resolved {
    ( $identifier:expr ) => {
        Expression(
            Rc::new(
                VariableExpressionNotResolved::new(
                    $identifier
                )
            )
        )
    }
}

pub struct VariableExpression {
    from: IdentifierToken,
    binding: usize,
}

impl VariableExpression {
    pub fn new(
        from: IdentifierToken,
        binding: usize,
    ) -> VariableExpression
    {
        VariableExpression {
            from,
            binding,
        }
    }

    pub fn from(&self) -> &IdentifierToken {
        &self.from
    }

    pub fn binding(&self) -> usize {
        self.binding
    }
}

impl Code for VariableExpression {
    fn code_span(&self) -> CodeSpan {
        self.from.code_span()
    }
}

impl Print for VariableExpression {
    fn print(&self) -> String {
        self.from().name().to_string()
    }
}

impl_debug_for_printable!(VariableExpression);

impl Evaluate for VariableExpression {
    fn evaluate(&self, env: &Environment) -> Result<Value, RuntimeError> {
        if let Some(v) = env.get(
            self.from().name(),
            self.binding(),
        )
        {
            return Ok(v);
        }
        else {
            unreachable!("Variable not declared should not be runtime error.");
        }
    }
}

impl AsExpression for VariableExpression {
    fn resolve(&self, _: &mut ResolveCtx) -> Result<Expression, ResolveError> {
        Ok(
            Expression(
                Rc::new(
                    VariableExpression::new(
                        self.from.clone(),
                        self.binding,
                    )
                )
            )
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        code::code_span::new_code_span,
        parse::expression::variable::{
            VariableExpressionNotResolved,
            VariableExpression,
        },
        value::Value,
        print::Print,
        resolve::{
            ResolveError,
            ResolveErrorEnum,
        },
        utils::test_utils::{
            TestContext,
            parse_expression,
            parse_expression_unknown,
        }
    };

    #[test]
    fn test_variable_expression_not_resolved_print() {
        assert_eq!(
            parse_expression::<VariableExpressionNotResolved>("foo").print(),
            "foo"
        );
    }

    #[test]
    fn test_variable_expression_print() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        assert_eq!(
            ctx.resolve_expression::<VariableExpression>(
                parse_expression_unknown("foo").as_ref(),
            )
                .unwrap()
                .print(),
            "foo"
        );
    }

    #[test]
    fn test_variable_expression_evaluate() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo = true;");
        let expr = ctx.resolve_expression::<VariableExpression>(
            parse_expression_unknown("foo").as_ref()
        )
            .unwrap();
        assert_eq!(
            ctx.evaluate(expr.as_ref()),
            Ok(Value::Bool(true)),
        );
    }

    #[test]
    #[should_panic]
    fn test_variable_expression_evaluate_not_declare_panic() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        let expr = ctx.resolve_expression::<VariableExpression>(
            parse_expression_unknown("foo").as_ref()
        )
            .unwrap();
        let mut ctx = TestContext::new();
        ctx.evaluate(expr.as_ref()).expect("evaluate");
    }

    #[test]
    fn test_variable_expression_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        ctx.resolve_context.begin();
        ctx.execute_src("var bar;");

        let var_expr = ctx.resolve_expression::<VariableExpression>(
            parse_expression_unknown("foo").as_ref()
        )
            .unwrap();
        assert_eq!(
            var_expr.binding(),
            1
        );

        let var_expr = ctx.resolve_expression::<VariableExpression>(
            parse_expression_unknown("bar").as_ref()
        )
            .unwrap();
        assert_eq!(
            var_expr.binding(),
            0
        );
    }

    #[test]
    fn test_variable_expression_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_expression::<VariableExpression>(
                parse_expression_unknown("foo").as_ref()
            )
                .unwrap_err(),
            ResolveError::new(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 0, 0, 3)
            )
        );
    }
}
