pub mod parser;
pub mod expression;
pub mod statement;

use std::rc::Rc;
use crate::scan::token::Token;
use parser::{
    Parser,
    ParserOutput,
};

pub trait Parse {
    fn parse(&self) -> ParserOutput;
}

impl Parse for Vec<Rc<Token>> {
    fn parse(&self) -> ParserOutput {
        Parser::parse(self)
    }
}
