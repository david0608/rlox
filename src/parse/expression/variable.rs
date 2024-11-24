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
pub struct VariableExpressionNotResolved {
    from: Rc<IdentifierToken>,
}

impl VariableExpressionNotResolved {
    pub fn new(
        from: Rc<IdentifierToken>,
    ) -> VariableExpressionNotResolved
    {
        VariableExpressionNotResolved {
            from,
        }
    }

    pub fn from(&self) -> &Rc<IdentifierToken> {
        &self.from
    }
}

impl Code for VariableExpressionNotResolved {
    fn code_span(&self) -> &CodeSpan {
        self.from.code_span()
    }

    fn to_string(&self) -> String {
        self.from().name().to_string()
    }
}

impl Expression for VariableExpressionNotResolved {
    fn resolve(&self, context: &mut Vec<HashSet<String>>) -> Result<Rc<dyn Expression>, ResolveError> {
        let binding = if let Some(d) = context.find(self.from.name()) {
            d
        }
        else {
            return Err(
                ResolveError::new(
                    ResolveErrorEnum::VariableNotDeclared,
                    self.from.code_span().clone()
                )
            );
        };

        Ok(
            Rc::new(
                VariableExpression::new(
                    self.from.clone(),
                    binding,
                )
            )
        )
    }

    fn evaluate(&self, _: &Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
        panic!("VariableExpressionNotResolved can not evaluate");
    }
}

#[macro_export]
macro_rules! variable_expression_not_resolved {
    ( $identifier:expr ) => {
        Rc::new(
            VariableExpressionNotResolved::new(
                $identifier
            )
        )
    }
}

#[derive(Debug)]
pub struct VariableExpression {
    from: Rc<IdentifierToken>,
    binding: usize,
}

impl VariableExpression {
    pub fn new(
        from: Rc<IdentifierToken>,
        binding: usize,
    ) -> VariableExpression
    {
        VariableExpression {
            from,
            binding,
        }
    }

    pub fn from(&self) -> &Rc<IdentifierToken> {
        &self.from
    }

    pub fn binding(&self) -> usize {
        self.binding
    }
}

impl Code for VariableExpression {
    fn code_span(&self) -> &CodeSpan {
        self.from.code_span()
    }

    fn to_string(&self) -> String {
        self.from().name().to_string()
    }
}

impl Expression for VariableExpression {
    fn resolve(&self, _: &mut Vec<HashSet<String>>) -> Result<Rc<dyn Expression>, ResolveError> {
        Ok(
            Rc::new(
                VariableExpression::new(
                    self.from.clone(),
                    self.binding,
                )
            )
        )
    }

    fn evaluate(&self, env: &Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
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

#[cfg(test)]
mod tests {
    use crate::{
        code::{
            Code,
            CodeSpan,
        },
        parse::expression::variable::{
            VariableExpressionNotResolved,
            VariableExpression,
        },
        value::Value,
        error::{
            ResolveError,
            ResolveErrorEnum,
        },
        resolve_context::ResolveContext,
        utils::test_utils::{
            TestContext,
            parse_expression,
            parse_expression_unknown,
        }
    };

    #[test]
    fn test_variable_expression_not_resolved_print() {
        assert_eq!(
            parse_expression::<VariableExpressionNotResolved>("foo").to_string(),
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
                .to_string(),
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
                CodeSpan::new(0, 0, 0, 3)
            )
        );
    }
}
