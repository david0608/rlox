// use rlox_macro::*;

use crate::visit::Accept;
use crate::token::{
    NumberToken,
    StringToken,
};

mod print;
use print::Print;

pub type Expression<'a> = Box<dyn Expr + 'a>;

pub enum UnaryExpression<'a> {
    Negative(Expression<'a>),
    Not(Expression<'a>)
}

#[macro_export]
macro_rules! unary_expression {
    ( $variant:ident, $rhs:expr ) => {
        Box::new(UnaryExpression::$variant($rhs))
    };
}

pub enum BinaryExpression<'a, 'b> {
    Equal(Expression<'a>, Expression<'b>),
    NotEqual(Expression<'a>, Expression<'b>),
    Less(Expression<'a>, Expression<'b>),
    LessEqual(Expression<'a>, Expression<'b>),
    Greater(Expression<'a>, Expression<'b>),
    GreaterEqual(Expression<'a>, Expression<'b>),
    Plus(Expression<'a>, Expression<'b>),
    Minus(Expression<'a>, Expression<'b>),
    Multiply(Expression<'a>, Expression<'b>),
    Divide(Expression<'a>, Expression<'b>),
}

#[macro_export]
macro_rules! binary_expression {
    ( $variant:ident, $lhs:expr, $rhs:expr ) => {
        Box::new(BinaryExpression::$variant($lhs, $rhs))
    };
}

pub enum LiteralExpression<'a> {
    Number(NumberToken<'a>),
    String(StringToken<'a>),
    True,
    False,
    Nil
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

pub struct GroupingExpression<'a>(pub Expression<'a>);

#[macro_export]
macro_rules! grouping_expression {
    ( $expr:expr ) => {
        Box::new(GroupingExpression($expr))
    };
}

pub trait Expr:
    Accept<Print, String>
{
    fn print(&self) -> String {
        self.accept(Print)
    }
}

impl<T> Expr for T
    where
    T: Accept<Print, String>
{ }
