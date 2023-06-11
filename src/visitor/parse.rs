use crate::scan::token::Token;
use crate::parse::{
    Parser,
    ParserOutput,
};
use super::{
    Visit,
    Accept,
};

pub struct Parse;

pub trait Parsable
    where
    Self: for<'this> Accept<'this, Parse, ParserOutput<'this>>
{
    fn parse(&self) -> ParserOutput {
        self.accept(Parse)
    }
}

impl<T> Parsable for T
    where
    T: for<'this> Accept<'this, Parse, ParserOutput<'this>>
{ }

impl<'that> Visit<'that, Vec<Token<'_>>, ParserOutput<'that>> for Parse {
    fn visit(tokens: &'that Vec<Token>) -> ParserOutput<'that> {
        Parser::parse(tokens)
    }
}
