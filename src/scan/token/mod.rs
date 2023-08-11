use super::span::Span;

pub mod simple;
use simple::{
    SimpleToken,
    SimpleTokenEnum,
};

pub mod number;
use number::NumberToken;

pub mod string;
use string:: StringToken;

pub mod identifier;
use identifier::IdentifierToken;

pub enum Token {
    Simple(SimpleToken),
    Number(NumberToken),
    String(StringToken),
    Identifier(IdentifierToken),
}

impl Token {
    pub fn new_simple(variant: SimpleTokenEnum, span: Span) -> Token {
        Token::Simple(SimpleToken::new(variant, span))
    }

    pub fn new_number(literal: f64, lexeme: &str, span: Span) -> Token {
        Token::Number(NumberToken::new(literal, lexeme, span))
    }

    pub fn new_string(literal: &str, lexeme: &str, span: Span) -> Token {
        Token::String(StringToken::new(literal, lexeme, span))
    }

    pub fn new_identifier(name: &str, span: Span) -> Token {
        Token::Identifier(IdentifierToken::new(name, span))
    }

    pub fn lexeme(&self) -> &str {
        match self {
            Token::Simple(t) => t.lexeme(),
            Token::Number(t) => t.lexeme(),
            Token::String(t) => t.lexeme(),
            Token::Identifier(t) => t.lexeme(),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Token::Simple(t) => t.span(),
            Token::Number(t) => t.span(),
            Token::String(t) => t.span(),
            Token::Identifier(t) => t.span(),
        }
    }
}
