use crate::code::Code;
use crate::code::code_span::CodeSpan;
use super::{
    Expression,
    BoxedExpression,
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
    lhs: BoxedExpression,
    rhs: BoxedExpression,
    code_span: CodeSpan,
}

impl BinaryExpression {
    pub fn new(
        variant: BinaryExpressionEnum,
        lhs: BoxedExpression,
        rhs: BoxedExpression,
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

    pub fn lhs(&self) -> &BoxedExpression {
        &self.lhs
    }

    pub fn rhs(&self) -> &BoxedExpression {
        &self.rhs
    }
}

impl Code for BinaryExpression {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Expression for BinaryExpression {
    fn box_clone(&self) -> BoxedExpression {
        Box::new(
            BinaryExpression::new(
                self.variant(),
                self.lhs().clone(),
                self.rhs().clone(),
                self.code_span(),
            )
        )
    }
}

#[macro_export]
macro_rules! binary_expression {
    ( $variant:ident, $lhs:expr, $rhs:expr, $code_span:expr ) => {
        Box::new(
            BinaryExpression::new(
                BinaryExpressionEnum::$variant,
                $lhs,
                $rhs,
                $code_span
            )
        )
    };
}
