use crate::scan::token::{
    NumberToken,
    StringToken,
};
use crate::visitor::Printable;
use crate::impl_debug_for_printable;

pub enum LiteralExpression<'src> {
    Number(NumberToken<'src>),
    String(StringToken<'src>),
    True,
    False,
    Nil
}

impl_debug_for_printable!(LiteralExpression<'_>);

#[macro_export]
macro_rules! literal_expression {
    ( $variant:ident, $token:ident, $lexeme:expr, $literal:expr ) => {
        Box::new(LiteralExpression::$variant($token::new($lexeme, $literal)))
    };

    ( $variant:ident, $token:expr ) => {
        Box::new(LiteralExpression::$variant($token))
    };

    ( $variant:ident ) => {
        Box::new(LiteralExpression::$variant)
    };

    ( Number, $lexeme:expr, $literal:expr ) => {
        literal_expression!(Number, NumberToken, $lexeme, $literal)
    };

    ( String, $lexeme:expr, $literal:expr ) => {
        literal_expression!(String, StringToken, $lexeme, $literal)
    };
}
