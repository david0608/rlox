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
    BoxedExpression,
};

pub struct AssignExpressionNotResolved {
    to: IdentifierToken,
    value: BoxedExpression,
    code_span: CodeSpan,
}

impl AssignExpressionNotResolved {
    pub fn new(
        to: IdentifierToken,
        value: BoxedExpression,
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

    pub fn value(&self) -> &BoxedExpression {
        &self.value
    }
}

impl Code for AssignExpressionNotResolved {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Expression for AssignExpressionNotResolved {
    fn box_clone(&self) -> BoxedExpression {
        Box::new(
            AssignExpressionNotResolved::new(
                self.to.clone(),
                self.value.clone(),
                self.code_span.clone(),
            )
        )
    }

    fn resolve(&self, context: &mut ResolveCtx) -> Result<BoxedExpression, ResolveError> {
        if let Some(d) = context.find(self.to.name()) {
            return Ok(
                Box::new(
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
                resolve_error!(
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
        Box::new(
            AssignExpressionNotResolved::new(
                $to,
                $value,
                $code_span
            )
        )
    };
}

pub struct AssignExpression {
    to: IdentifierToken,
    value: BoxedExpression,
    code_span: CodeSpan,
    binding: usize,
}

impl AssignExpression {
    pub fn new(
        to: IdentifierToken,
        value: BoxedExpression,
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

    pub fn value(&self) -> &BoxedExpression {
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

impl Expression for AssignExpression {
    fn box_clone(&self) -> BoxedExpression {
        Box::new(
            AssignExpression::new(
                self.to.clone(),
                self.value.clone(),
                self.code_span.clone(),
                self.binding,
            )
        )
    }

    fn resolve(&self, _: &mut ResolveCtx) -> Result<BoxedExpression, ResolveError> {
        Ok(self.box_clone())
    }
}

#[macro_export]
macro_rules! assign_expression {
    ( $to:expr, $value:expr, $code_span:expr, $binding:expr ) => {
        Box::new(
            AssignExpression::new(
                $to,
                $value,
                $code_span,
                $binding
            )
        )
    };
}

#[cfg(test)]
mod tests {
    use crate::code::code_span::new_code_span;
    use crate::parse::expression::{
        assign::{
            AssignExpression,
            AssignExpressionNotResolved,
        },
        variable::VariableExpression,
    };
    use crate::resolve::{
        ResolveError,
        ResolveErrorEnum,
    };
    use crate::utils::{
        AsAny,
        test_utils::{
            TestContext,
            parse_expression,
            parse_expression_unknown,
        },
    };
    use crate::{
        resolve_error,
        box_downcast,
    };

    #[test]
    fn test_assign_expression_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        ctx.resolve_context.begin();
        ctx.execute_src("var bar;");

        let assign_expr = ctx.resolve_expression::<AssignExpression>(
            parse_expression_unknown("foo = true").as_ref()
        )
            .unwrap();
        assert_eq!(assign_expr.binding(), 1);

        let assign_expr = ctx.resolve_expression::<AssignExpression>(
            parse_expression_unknown("bar = foo").as_ref()
        )
            .unwrap();
        assert_eq!(assign_expr.binding(), 0);

        let var_expr = box_downcast!(assign_expr.value, VariableExpression);
        assert_eq!(var_expr.binding(), 1);
    }

    #[test]
    fn test_assign_expression_resolve_variable_not_found_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_expression_unknown(
                parse_expression::<AssignExpressionNotResolved>("foo = true").as_ref()
            )
                .unwrap_err(),
            resolve_error!(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 0, 0, 3)
            )
        );
    }

    #[test]
    fn test_assign_expression_resolve_value_resolve_error() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        assert_eq!(
            ctx.resolve_expression_unknown(
                parse_expression::<AssignExpressionNotResolved>("foo = bar").as_ref()
            )
                .unwrap_err(),
            resolve_error!(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 6, 0, 9)
            )
        );
    }
}
