use crate::code::Code;
use crate::code::code_span::CodeSpan;
use crate::resolve::{
    ResolveCtx,
    ResolveError,
};
use super::{
    Expression,
    BoxedExpression,
};

#[derive(Clone, Copy)]
pub enum UnaryExpressionEnum {
    Negative,
    Not
}

pub struct UnaryExpression {
    variant: UnaryExpressionEnum,
    rhs: BoxedExpression,
    code_span: CodeSpan,
}

impl UnaryExpression {
    pub fn new(
        variant: UnaryExpressionEnum, 
        rhs: BoxedExpression,
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

    pub fn rhs(&self) -> &BoxedExpression {
        &self.rhs
    }
}

impl Code for UnaryExpression {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Expression for UnaryExpression {
    fn box_clone(&self) -> BoxedExpression {
        Box::new(
            UnaryExpression::new(
                self.variant(),
                self.rhs().clone(),
                self.code_span(),
            )
        )
    }

    fn resolve(&self, context: &mut ResolveCtx) -> Result<BoxedExpression, ResolveError> {
        Ok(
            Box::new(
                UnaryExpression::new(
                    self.variant,
                    self.rhs.resolve(context)?,
                    self.code_span.clone(),
                )
            )
        )
    }
}

#[macro_export]
macro_rules! unary_expression {
    ( $variant:ident, $rhs:expr, $code_span:expr ) => {
        Box::new(
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
    use crate::code::code_span::new_code_span;
    use crate::parse::expression::{
        unary::UnaryExpression,
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
        downcast_ref,
    };

    #[test]
    fn test_unary_expression_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");

        let unary_expr = ctx.resolve_expression::<UnaryExpression>(
            parse_expression_unknown("!foo").as_ref()
        )
            .unwrap();
        assert_eq!(
            downcast_ref!(unary_expr.rhs(), VariableExpression).binding(),
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
            resolve_error!(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 1, 0, 4)
            )
        );
    }
}
