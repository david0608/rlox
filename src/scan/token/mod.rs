use std::rc::Rc;
use crate::code::{
    Code,
    code_span::CodeSpan,
};

pub mod identifier;
use identifier::IdentifierToken;

pub mod number;
use number::NumberToken;

pub mod simple;
use simple::{
    SimpleToken,
    SimpleTokenEnum,
};

pub mod string;
use string:: StringToken;

#[derive(Debug)]
pub enum Token {
    Identifier(Rc<IdentifierToken>),
    Number(Rc<NumberToken>),
    Simple(Rc<SimpleToken>),
    String(Rc<StringToken>),
}

impl Token {
    pub fn new_identifier(name: &str, code_span: CodeSpan) -> Token {
        Token::Identifier(Rc::new(IdentifierToken::new(name, code_span)))
    }

    pub fn new_number(literal: f64, lexeme: &str, code_span: CodeSpan) -> Token {
        Token::Number(Rc::new(NumberToken::new(literal, lexeme, code_span)))
    }

    pub fn new_simple(variant: SimpleTokenEnum, code_span: CodeSpan) -> Token {
        Token::Simple(Rc::new(SimpleToken::new(variant, code_span)))
    }

    pub fn new_string(literal: &str, lexeme: &str, code_span: CodeSpan) -> Token {
        Token::String(Rc::new(StringToken::new(literal, lexeme, code_span)))
    }

    #[allow(dead_code)]
    pub fn lexeme(&self) -> &str {
        match self {
            Token::Simple(t) => t.lexeme(),
            Token::Number(t) => t.lexeme(),
            Token::String(t) => t.lexeme(),
            Token::Identifier(t) => t.lexeme(),
        }
    }
}

impl Code for Token {
    fn code_span(&self) -> CodeSpan {
        match self {
            Token::Identifier(t) => t.code_span(),
            Token::Number(t) => t.code_span(),
            Token::Simple(t) => t.code_span(),
            Token::String(t) => t.code_span(),
        }
    }
}
