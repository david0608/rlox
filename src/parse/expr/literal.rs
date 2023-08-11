use crate::scan::span::Span;
use crate::scan::token::number::NumberToken;
use crate::scan::token::string::StringToken;
use crate::visitor::Printable;
use crate::impl_debug_for_printable;
use super::Expr;

#[derive(Clone)]
pub enum LiteralExpressionEnum {
    Number(NumberToken),
    String(StringToken),
    True,
    False,
    Nil
}

pub struct LiteralExpression {
    variant: LiteralExpressionEnum,
    span: Span,
}

impl LiteralExpression {
    pub fn new(variant: LiteralExpressionEnum, span: Span) -> LiteralExpression {
        LiteralExpression {
            variant,
            span,
        }
    }

    pub fn variant(&self) -> LiteralExpressionEnum {
        self.variant.clone()
    }
}

impl Expr for LiteralExpression {
    fn span(&self) -> Span {
        self.span
    }
}

impl_debug_for_printable!(LiteralExpression);

#[macro_export]
macro_rules! literal_expression {
    ( $variant:ident, $span:expr ) => {
        Box::new(
            LiteralExpression::new(
                LiteralExpressionEnum::$variant,
                $span,
            )
        )
    };

    ( $variant:ident, $token:expr, $span:expr ) => {
        Box::new(
            LiteralExpression::new(
                LiteralExpressionEnum::$variant($token),
                $span,
            )
        )
    }
}
