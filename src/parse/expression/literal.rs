use crate::code::Code;
use crate::code::code_span::CodeSpan;
use crate::scan::token::number::NumberToken;
use crate::scan::token::string::StringToken;
use crate::visitor::print::Printable;
use super::{
    Expression,
    BoxedExpression,
};
use crate::impl_debug_for_printable;

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
}

impl_debug_for_printable!(LiteralExpression);

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
