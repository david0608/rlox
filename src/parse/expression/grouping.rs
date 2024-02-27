use std::rc::Rc;
use crate::code::Code;
use crate::code::code_span::CodeSpan;
use crate::resolve::{
    ResolveCtx,
    ResolveError,
};
use super::{
    Expression,
    AsExpression,
};

pub struct GroupingExpression {
    expression: Expression,
    code_span: CodeSpan,
}

impl GroupingExpression {
    pub fn new(expression: Expression, code_span: CodeSpan) -> GroupingExpression {
        GroupingExpression {
            expression,
            code_span,
        }
    }

    pub fn expression(&self) -> &Expression {
        &self.expression
    }
}

impl Code for GroupingExpression {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl AsExpression for GroupingExpression {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Expression, ResolveError> {
        Ok(
            Expression(
                Rc::new(
                    GroupingExpression::new(
                        self.expression.resolve(context)?,
                        self.code_span.clone(),
                    )
                )
            )
        )
    }
}

#[macro_export]
macro_rules! grouping_expression {
    ( $expression:expr, $code_span:expr ) => {
        Expression(
            Rc::new(
                GroupingExpression::new(
                    $expression,
                    $code_span
                )
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
    use crate::utils::test_utils::{
        TestContext,
        parse_expression,
        parse_expression_unknown,
    };
    use crate::resolve_error;

    #[test]
    fn test_grouping_expression_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");

        let group_expr = ctx.resolve_expression::<GroupingExpression>(
            parse_expression_unknown("(foo)").as_ref()
        )
            .unwrap();
        assert_eq!(
            group_expr.expression().downcast_ref::<VariableExpression>().unwrap().binding(),
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
