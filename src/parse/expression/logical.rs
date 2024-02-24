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
pub enum LogicalExpressionEnum {
    And,
    Or,
}

pub struct LogicalExpression {
    variant: LogicalExpressionEnum,
    lhs: BoxedExpression,
    rhs: BoxedExpression,
    code_span: CodeSpan,
}

impl LogicalExpression {
    pub fn new(
        variant: LogicalExpressionEnum,
        lhs: BoxedExpression,
        rhs: BoxedExpression,
        code_span: CodeSpan,
    ) -> LogicalExpression
    {
        LogicalExpression {
            variant,
            lhs,
            rhs,
            code_span,
        }
    }

    pub fn variant(&self) -> LogicalExpressionEnum {
        self.variant
    }

    pub fn lhs(&self) -> &BoxedExpression {
        &self.lhs
    }

    pub fn rhs(&self) -> &BoxedExpression {
        &self.rhs
    }
}

impl Code for LogicalExpression {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Expression for LogicalExpression {
    fn box_clone(&self) -> BoxedExpression {
        Box::new(
            LogicalExpression::new(
                self.variant(),
                self.lhs().clone(),
                self.rhs().clone(),
                self.code_span(),
            )
        )
    }

    fn resolve(&self, context: &mut ResolveCtx) -> Result<BoxedExpression, ResolveError> {
        Ok(
            Box::new(
                LogicalExpression::new(
                    self.variant,
                    self.lhs.resolve(context)?,
                    self.rhs.resolve(context)?,
                    self.code_span.clone(),
                )
            )
        )
    }
}

#[macro_export]
macro_rules! logical_expression {
    ( $variant:ident, $lhs:expr, $rhs:expr, $code_span:expr ) => {
        Box::new(
            LogicalExpression::new(
                LogicalExpressionEnum::$variant,
                $lhs,
                $rhs,
                $code_span
            )
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::code::code_span::new_code_span;
    use crate::parse::expression::{
        logical::LogicalExpression,
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
    fn test_logical_expression_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        ctx.resolve_context.begin();
        ctx.execute_src("var bar;");

        let logical_expr = ctx.resolve_expression::<LogicalExpression>(
            parse_expression_unknown("foo or bar").as_ref()
        )
            .unwrap();
        assert_eq!(
            downcast_ref!(logical_expr.lhs(), VariableExpression).binding(),
            1
        );
        assert_eq!(
            downcast_ref!(logical_expr.rhs(), VariableExpression).binding(),
            0
        );
    }

    #[test]
    fn test_logical_expression_resolve_lhs_rhs_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_expression_unknown(
                parse_expression::<LogicalExpression>("bar or true").as_ref()
            )
                .unwrap_err(),
            resolve_error!(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 0, 0, 3)
            )
        );
        assert_eq!(
            ctx.resolve_expression_unknown(
                parse_expression::<LogicalExpression>("true or bar").as_ref()
            )
                .unwrap_err(),
            resolve_error!(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 8, 0, 11)
            )
        );
    }
}
