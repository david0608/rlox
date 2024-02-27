use std::rc::Rc;
use crate::code::Code;
use crate::code::code_span::CodeSpan;
use crate::scan::token::identifier::IdentifierToken;
use crate::resolve::{
    ResolveCtx,
    ResolveError,
    ResolveErrorEnum,
};
use crate::resolve_error;
use super::{
    Expression,
    AsExpression,
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

impl AsExpression for VariableExpressionNotResolved {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Expression, ResolveError> {
        let binding = if let Some(d) = context.find(self.from.name()) {
            d
        }
        else {
            return Err(
                resolve_error!(
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
    use crate::code::code_span::new_code_span;
    use crate::parse::expression::variable::VariableExpression;
    use crate::resolve::{
        ResolveError,
        ResolveErrorEnum,
    };
    use crate::utils::test_utils::{
        TestContext,
        parse_expression_unknown,
    };
    use crate::resolve_error;

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
            resolve_error!(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 0, 0, 3)
            )
        );
    }
}
