use crate::code::Code;
use crate::code::code_span::CodeSpan;
use crate::scan::token::{
    number::NumberToken,
    string::StringToken,
};
use crate::resolve::{
    ResolveCtx,
    ResolveError,
};
use super::{
    Expression,
    BoxedExpression,
};

#[derive(Clone, Debug, PartialEq)]
pub enum LiteralExpressionEnum {
    Number(NumberToken),
    String(StringToken),
    True,
    False,
    Nil
}

pub struct LiteralExpression {
    variant: LiteralExpressionEnum,
    code_span: CodeSpan,
}

impl LiteralExpression {
    pub fn new(variant: LiteralExpressionEnum, code_span: CodeSpan) -> LiteralExpression {
        LiteralExpression {
            variant,
            code_span,
        }
    }

    pub fn variant(&self) -> LiteralExpressionEnum {
        self.variant.clone()
    }
}

impl Code for LiteralExpression {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Expression for LiteralExpression {
    fn box_clone(&self) -> BoxedExpression {
        Box::new(
            LiteralExpression::new(
                self.variant(),
                self.code_span(),
            )
        )
    }

    fn resolve(&self, _: &mut ResolveCtx) -> Result<BoxedExpression, ResolveError> {
        Ok(
            Box::new(
                LiteralExpression::new(
                    self.variant.clone(),
                    self.code_span.clone(),
                )
            )
        )
    }
}

#[macro_export]
macro_rules! literal_expression {
    ( $variant:ident, $code_span:expr ) => {
        Box::new(
            LiteralExpression::new(
                LiteralExpressionEnum::$variant,
                $code_span,
            )
        )
    };

    ( $variant:ident, $token:expr, $code_span:expr ) => {
        Box::new(
            LiteralExpression::new(
                LiteralExpressionEnum::$variant($token),
                $code_span,
            )
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::code::{
        Code,
        code_span::new_code_span,
    };
    use crate::parse::expression::literal::{
        LiteralExpression,
        LiteralExpressionEnum,
    };
    use crate::utils::test_utils::{
        TestContext,
        parse_expression_unknown,
    };

    #[test]
    fn test_literal_expression_resolve() {
        let mut ctx = TestContext::new();
        let literal_expr = ctx.resolve_expression::<LiteralExpression>(
            parse_expression_unknown("true").as_ref()
        )
            .unwrap();
        assert_eq!(
            literal_expr.variant(),
            LiteralExpressionEnum::True,
        );
        assert_eq!(
            literal_expr.code_span(),
            new_code_span(0, 0, 0, 4),
        );
    }
}
