use crate::code::Code;
use crate::code::code_span::CodeSpan;

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
    Identifier(IdentifierToken),
    Number(NumberToken),
    Simple(SimpleToken),
    String(StringToken),
}

impl Token {
    pub fn new_identifier(name: &str, code_span: CodeSpan) -> Token {
        Token::Identifier(IdentifierToken::new(name, code_span))
    }

    pub fn new_number(literal: f64, lexeme: &str, code_span: CodeSpan) -> Token {
        Token::Number(NumberToken::new(literal, lexeme, code_span))
    }

    pub fn new_simple(variant: SimpleTokenEnum, code_span: CodeSpan) -> Token {
        Token::Simple(SimpleToken::new(variant, code_span))
    }

    pub fn new_string(literal: &str, lexeme: &str, code_span: CodeSpan) -> Token {
        Token::String(StringToken::new(literal, lexeme, code_span))
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
