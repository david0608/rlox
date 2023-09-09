pub mod parser;
pub mod expression;
pub mod statement;

use crate::scan::token::Token;
use parser::{
    Parser,
    ParserOutput,
};

pub trait Parse {
    fn parse(&self) -> ParserOutput;
}

impl Parse for Vec<Token> {
    fn parse(&self) -> ParserOutput {
        Parser::parse(self)
    }
}
