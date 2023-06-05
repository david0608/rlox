use crate::scan::{
    NumberToken,
    StringToken,
};
use super::print::Printable;

pub enum LiteralExpression<'a> {
    Number(NumberToken<'a>),
    String(StringToken<'a>),
    True,
    False,
    Nil
}

impl std::fmt::Debug for LiteralExpression<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.print())
    }
}

#[macro_export]
macro_rules! literal_expression {
    ( $variant:ident, $token:ident, $lexeme:expr, $literal:expr ) => {
        Box::new(LiteralExpression::$variant($token::new($lexeme, $literal)))
    };

    ( $variant:ident, $token:expr ) => {
        Box::new(LiteralExpression::$variant(*$token))
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
