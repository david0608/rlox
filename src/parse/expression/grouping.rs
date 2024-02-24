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

pub struct GroupingExpression {
    expression: BoxedExpression,
    code_span: CodeSpan,
}

impl GroupingExpression {
    pub fn new(expression: BoxedExpression, code_span: CodeSpan) -> GroupingExpression {
        GroupingExpression {
            expression,
            code_span,
        }
    }

    pub fn expression(&self) -> &BoxedExpression {
        &self.expression
    }
}

impl Code for GroupingExpression {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Expression for GroupingExpression {
    fn box_clone(&self) -> BoxedExpression {
        Box::new(
            GroupingExpression::new(
                self.expression().clone(),
                self.code_span(),
            )
        )
    }

    fn resolve(&self, context: &mut ResolveCtx) -> Result<BoxedExpression, ResolveError> {
        Ok(
            Box::new(
                GroupingExpression::new(
                    self.expression.resolve(context)?,
                    self.code_span.clone(),
                )
            )
        )
    }
}

#[macro_export]
macro_rules! grouping_expression {
    ( $expression:expr, $code_span:expr ) => {
        Box::new(
            GroupingExpression::new(
                $expression,
                $code_span
            )
        )
    };
}

#[cfg(test)]
mod tests {
    use crate::code::code_span::new_code_span;
    use crate::parse::expression::{
        grouping::GroupingExpression,
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
    fn test_grouping_expression_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");

        let group_expr = ctx.resolve_expression::<GroupingExpression>(
            parse_expression_unknown("(foo)").as_ref()
        )
            .unwrap();
        assert_eq!(
            downcast_ref!(group_expr.expression(), VariableExpression).binding(),
            0
        );
    }

    #[test]
    fn test_grouping_expression_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_expression_unknown(
                parse_expression::<GroupingExpression>("(foo)").as_ref()
            )
                .unwrap_err(),
            resolve_error!(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 1, 0, 4)
            )
        );
    }
}
