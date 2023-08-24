use crate::scan::token::Token;
use crate::parse::parser::{
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
    Self: for<'this> Accept<'this, Parse, ParserOutput>
{
    fn parse(&self) -> ParserOutput {
        self.accept(Parse)
    }
}

impl<T> Parsable for T
    where
    T: for<'this> Accept<'this, Parse, ParserOutput>
{ }

impl<'that> Visit<'that, Vec<Token>, ParserOutput> for Parse {
    fn visit(tokens: &'that Vec<Token>) -> ParserOutput {
        Parser::parse(tokens)
    }
}
