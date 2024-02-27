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

#[derive(Clone, Copy)]
pub enum BinaryExpressionEnum {
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Plus,
    Minus,
    Multiply,
    Divide,
}

pub struct BinaryExpression {
    variant: BinaryExpressionEnum,
    lhs: Expression,
    rhs: Expression,
    code_span: CodeSpan,
}

impl BinaryExpression {
    pub fn new(
        variant: BinaryExpressionEnum,
        lhs: Expression,
        rhs: Expression,
        code_span: CodeSpan,
    ) -> BinaryExpression
    {
        BinaryExpression {
            variant,
            lhs,
            rhs,
            code_span,
        }
    }

    pub fn variant(&self) -> BinaryExpressionEnum {
        self.variant
    }

    pub fn lhs(&self) -> &Expression {
        &self.lhs
    }

    pub fn rhs(&self) -> &Expression {
        &self.rhs
    }
}

impl Code for BinaryExpression {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl AsExpression for BinaryExpression {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Expression, ResolveError> {
        Ok(
            Expression(
                Rc::new(
                    BinaryExpression::new(
                        self.variant,
                        self.lhs.resolve(context)?,
                        self.rhs.resolve(context)?,
                        self.code_span.clone(),
                    )
                )
            )
        )
    }
}

#[macro_export]
macro_rules! binary_expression {
    ( $variant:ident, $lhs:expr, $rhs:expr, $code_span:expr ) => {
        Expression(
            Rc::new(
                BinaryExpression::new(
                    BinaryExpressionEnum::$variant,
                    $lhs,
                    $rhs,
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
        binary::BinaryExpression,
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
    fn test_binary_expression_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        ctx.resolve_context.begin();
        ctx.execute_src("var bar;");

        let binary_expr = ctx.resolve_expression::<BinaryExpression>(
            parse_expression_unknown("foo == bar").as_ref()
        )
            .unwrap();
        assert_eq!(
            binary_expr.lhs().downcast_ref::<VariableExpression>().unwrap().binding(),
            1
        );
        assert_eq!(
            binary_expr.rhs().downcast_ref::<VariableExpression>().unwrap().binding(),
            0
        );
    }

    #[test]
    fn test_binary_expression_resolve_lhs_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_expression_unknown(
                parse_expression::<BinaryExpression>("foo == bar").as_ref()
            )
                .unwrap_err(),
            resolve_error!(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 0, 0, 3)
            )
        );
    }

    #[test]
    fn test_binary_expression_resolve_rhs_resolve_error() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        assert_eq!(
            ctx.resolve_expression_unknown(
                parse_expression::<BinaryExpression>("foo == bar").as_ref()
            )
                .unwrap_err(),
            resolve_error!(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 7, 0, 10)
            )
        );
    }
}
