use std::fmt;
use crate::token::{
    Token,
    TokenType,
    SimpleToken,
    RIGHT_PAREN_LEXEME,
};
use crate::expr::{
    UnaryExpression,
    BinaryExpression,
    Expression,
    LiteralExpression,
    GroupingExpression,
};
use crate::{
    literal_expression,
    grouping_expression,
    unary_expression,
    binary_expression,
};

#[derive(Debug)]
enum Error<'a> {
    UnexpectedEnd,
    UnexpectedToken(&'a str, usize),
    ExpectTokenMismatch(&'a str, &'a str, usize),
    ExpectTokenNotFound(&'a str),
}

impl fmt::Display for Error<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::UnexpectedEnd => write!(f, "Unexpected end of source."),
            Error::UnexpectedToken(t, l) => write!(f, "line {}: Unexpected token: {}.", l, t),
            Error::ExpectTokenMismatch(et, ft, l) => write!(f, "line {}: Expect token: {} but found: {}.", l, et, ft),
            Error::ExpectTokenNotFound(t) => write!(f, "Expect token: {} but not found.", t),
        }
    }
}

pub struct Parser<'a, 'b> {
    tokens: &'a Vec<Token<'b>>,
    current: usize,
}

impl<'a, 'b, 'c> Parser<'b, 'c>
    where
    'b: 'c
{
    pub fn new(tokens: &'b Vec<Token<'c>>) -> Parser<'b, 'c> {
        Parser {
            tokens,
            current: 0,
        }
    }

    fn advance(&mut self) {
        self.current += 1;
    }

    fn is_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn peek(&'a self) -> Option<&'b Token<'c>> {
        if self.is_end() {
            None
        }
        else {
            Some(&self.tokens[self.current])
        }
    }

    fn consume_right_paren(&'a mut self) -> Result<(), Error<'c>> {
        if let Some(t) = self.peek() {
            match t.token_type() {
                TokenType::Simple(SimpleToken::RightParen) => {
                    self.advance();
                    Ok(())
                }
                _ => Err(Error::ExpectTokenMismatch(RIGHT_PAREN_LEXEME, t.lexeme(), t.line()))
            }
        }
        else {
            Err(Error::ExpectTokenNotFound(RIGHT_PAREN_LEXEME))
        }
    }

    fn synchronize(&mut self) {
        loop {
            if let Some(t) = self.peek() {
                match t.token_type() {
                    TokenType::Simple(SimpleToken::Class) |
                    TokenType::Simple(SimpleToken::Fun) |
                    TokenType::Simple(SimpleToken::Var) |
                    TokenType::Simple(SimpleToken::For) |
                    TokenType::Simple(SimpleToken::If) |
                    TokenType::Simple(SimpleToken::While) |
                    TokenType::Simple(SimpleToken::Print) |
                    TokenType::Simple(SimpleToken::Return) => {
                        return;
                    }
                    TokenType::Simple(SimpleToken::Semicolon) => {
                        self.advance();
                        return;
                    }
                    _ => {
                        self.advance();
                        continue;
                    }
                }
            }
            else {
                return;
            }
        }
    }

    fn expression(&'a mut self) -> Result<Expression<'c>, Error<'c>> {
        self.equality()
    }

    fn equality(&'a mut self) -> Result<Expression<'c>, Error<'c>> {
        let mut e = self.comparison()?;
        loop {
            if let Some(t) = self.peek() {
                match t.token_type() {
                    TokenType::Simple(SimpleToken::EqualEqual) => {
                        self.advance();
                        e = binary_expression!(
                            Equal,
                            e,
                            self.comparison()?
                        );
                        continue;
                    }
                    TokenType::Simple(SimpleToken::BangEqual) => {
                        self.advance();
                        e = binary_expression!(
                            NotEqual,
                            e,
                            self.comparison()?
                        );
                        continue;
                    }
                    _ => break
                }
            }
            else {
                break;
            }
        }
        Ok(e)
    }

    fn comparison(&'a mut self) -> Result<Expression<'c>, Error<'c>> {
        let mut e = self.term()?;
        loop {
            if let Some(t) = self.peek() {
                match t.token_type() {
                    TokenType::Simple(SimpleToken::Less) => {
                        self.advance();
                        e = binary_expression!(
                            Less,
                            e,
                            self.term()?
                        );
                        continue;
                    }
                    TokenType::Simple(SimpleToken::LessEqual) => {
                        self.advance();
                        e = binary_expression!(
                            LessEqual,
                            e,
                            self.term()?
                        );
                        continue;
                    }
                    TokenType::Simple(SimpleToken::Greater) => {
                        self.advance();
                        e = binary_expression!(
                            Greater,
                            e,
                            self.term()?
                        );
                        continue;
                    }
                    TokenType::Simple(SimpleToken::GreaterEqual) => {
                        self.advance();
                        e = binary_expression!(
                            GreaterEqual,
                            e,
                            self.term()?
                        );
                        continue;
                    }
                    _ => break
                }
            }
            else {
                break;
            }
        }
        Ok(e)
    }

    fn term(&'a mut self) -> Result<Expression<'c>, Error<'c>> {
        let mut e = self.factor()?;
        loop {
            if let Some(t) = self.peek() {
                match t.token_type() {
                    TokenType::Simple(SimpleToken::Plus) => {
                        self.advance();
                        e = binary_expression!(
                            Plus,
                            e,
                            self.factor()?
                        );
                        continue;
                    }
                    TokenType::Simple(SimpleToken::Minus) => {
                        self.advance();
                        e = binary_expression!(
                            Minus,
                            e,
                            self.factor()?
                        );
                        continue;
                    }
                    _ => break
                }
            }
            else {
                break;
            }
        }
        Ok(e)
    }

    fn factor(&'a mut self) -> Result<Expression<'c>, Error<'c>> {
        let mut e = self.unary()?;
        loop {
            if let Some(t) = self.peek() {
                match t.token_type() {
                    TokenType::Simple(SimpleToken::Star) => {
                        self.advance();
                        e = binary_expression!(
                            Multiply,
                            e,
                            self.unary()?
                        );
                        continue;
                    }
                    TokenType::Simple(SimpleToken::Slash) => {
                        self.advance();
                        e = binary_expression!(
                            Divide,
                            e,
                            self.unary()?
                        );
                        continue;
                    }
                    _ => break
                }
            }
            else {
                break;
            }
        }
        Ok(e)
    }

    fn unary(&'a mut self) -> Result<Expression<'c>, Error<'c>> {
        if let Some(t) = self.peek() {
            match t.token_type() {
                TokenType::Simple(SimpleToken::Bang) => {
                    self.advance();
                    Ok(unary_expression!(
                        Not,
                        self.unary()?
                    ))
                }
                TokenType::Simple(SimpleToken::Minus) => {
                    self.advance();
                    Ok(unary_expression!(
                        Negative,
                        self.unary()?
                    ))
                }
                _ => self.primary()
            }
        }
        else {
            Err(Error::UnexpectedEnd)
        }
    }

    fn primary(&'a mut self) -> Result<Expression<'c>, Error<'c>> {
        if let Some(t) = self.peek() {
            match t.token_type() {
                TokenType::Number(nt) => {
                    self.advance();
                    Ok(literal_expression!(Number, nt))
                }
                TokenType::String(st) => {
                    self.advance();
                    Ok(literal_expression!(String, st))
                }
                TokenType::Simple(SimpleToken::True) => {
                    self.advance();
                    Ok(literal_expression!(True))
                }
                TokenType::Simple(SimpleToken::False) => {
                    self.advance();
                    Ok(literal_expression!(False))
                }
                TokenType::Simple(SimpleToken::Nil) => {
                    self.advance();
                    Ok(literal_expression!(Nil))
                }
                TokenType::Simple(SimpleToken::LeftParen) => {
                    self.advance();
                    let e = self.expression()?;
                    self.consume_right_paren()?;
                    Ok(grouping_expression!(e))
                }
                _ => Err(Error::UnexpectedToken(t.lexeme(), t.line()))
            }
        }
        else {
            Err(Error::UnexpectedEnd)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::scanner::Scanner;
    use super::{
        Parser,
        Error,
    };

    #[test]
    fn test_number() {
        let s = Scanner::scan("1.23");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "1.23");
    }

    #[test]
    fn test_string() {
        let s = Scanner::scan("\"hello\"");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "\"hello\"");
    }

    #[test]
    fn test_true() {
        let s = Scanner::scan("true");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "true");
    }

    #[test]
    fn test_false() {
        let s = Scanner::scan("false");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "false");
    }

    #[test]
    fn test_nil() {
        let s = Scanner::scan("nil");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "nil");
    }

    #[test]
    fn test_grouping() {
        let s = Scanner::scan("(1 + 1)");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "(group (+ 1 1))");
    }

    #[test]
    fn test_not() {
        let s = Scanner::scan("!1");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "(! 1)");
    }

    #[test]
    fn test_not_not() {
        let s = Scanner::scan("!!1");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "(! (! 1))");
    }

    #[test]
    fn test_negative() {
        let s = Scanner::scan("-1");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "(- 1)");
    }

    #[test]
    fn test_negative_negative() {
        let s = Scanner::scan("--1");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "(- (- 1))");
    }

    #[test]
    fn test_minus_grouping() {
        let s = Scanner::scan("-(1 + 2)");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "(- (group (+ 1 2)))");
    }

    #[test]
    fn test_multiply() {
        let s = Scanner::scan("1 * 2");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "(* 1 2)");
    }

    #[test]
    fn test_divide() {
        let s = Scanner::scan("1 / 2");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "(/ 1 2)");
    }

    #[test]
    fn test_multiply_divide() {
        let s = Scanner::scan("1 * 2 / 3");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "(/ (* 1 2) 3)");
    }

    #[test]
    fn test_multiply_negative() {
        let s = Scanner::scan("1 * -2");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "(* 1 (- 2))");
    }

    #[test]
    fn test_plus() {
        let s = Scanner::scan("1 + 2");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "(+ 1 2)");
    }

    #[test]
    fn test_plus_plus() {
        let s = Scanner::scan("1 + 2 + 3");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "(+ (+ 1 2) 3)");
    }

    #[test]
    fn test_minus() {
        let s = Scanner::scan("1 - 2");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "(- 1 2)");
    }

    #[test]
    fn test_minus_minus() {
        let s = Scanner::scan("1 - 2 - 3");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "(- (- 1 2) 3)");
    }

    #[test]
    fn test_plus_multiply() {
        let s = Scanner::scan("1 + 2 * 3");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "(+ 1 (* 2 3))");
    }

    #[test]
    fn test_less() {
        let s = Scanner::scan("1 < 2");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "(< 1 2)");
    }

    #[test]
    fn test_lessequal() {
        let s = Scanner::scan("1 <= 2");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "(<= 1 2)");
    }

    #[test]
    fn test_greater() {
        let s = Scanner::scan("1 > 2");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "(> 1 2)");
    }

    #[test]
    fn test_greaterequal() {
        let s = Scanner::scan("1 >= 2");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "(>= 1 2)");
    }

    #[test]
    fn test_greater_add() {
        let s = Scanner::scan("1 > 2 + 3");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "(> 1 (+ 2 3))");
    }

    #[test]
    fn test_greater_add_greater() {
        let s = Scanner::scan("1 > 2 + 3 > 4");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "(> (> 1 (+ 2 3)) 4)");
    }

    #[test]
    fn test_add_greater_add() {
        let s = Scanner::scan("1 + 2 > 3 + 4");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "(> (+ 1 2) (+ 3 4))");
    }

    #[test]
    fn test_equal() {
        let s = Scanner::scan("1 == 2");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "(== 1 2)");
    }

    #[test]
    fn test_notequal() {
        let s = Scanner::scan("1 != 2");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "(!= 1 2)");
    }

    #[test]
    fn test_equal_add() {
        let s = Scanner::scan("1 == 2 + 3");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "(== 1 (+ 2 3))");
    }

    #[test]
    fn test_add_equal_add() {
        let s = Scanner::scan("1 + 2 == 3 + 4");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "(== (+ 1 2) (+ 3 4))");
    }

    #[test]
    fn test_equal_add_equal() {
        let s = Scanner::scan("1 == 2 + 3 == 4");
        let mut p = Parser::new(s.tokens());
        assert_eq!(p.expression().unwrap().print(), "(== (== 1 (+ 2 3)) 4)");
    }

    #[test]
    fn test_unexpected_end() {
        let s = Scanner::scan("");
        let mut p = Parser::new(s.tokens());
        p.synchronize();
        match p.expression().err().unwrap() {
            Error::UnexpectedEnd => { },
            _ => panic!("Should be UnexpectedEnd error.")
        }
    }

    #[test]
    fn test_unexpected_token() {
        let s = Scanner::scan("print");
        let mut p = Parser::new(s.tokens());
        match p.expression().err().unwrap() {
            Error::UnexpectedToken(t, l) => {
                assert_eq!(t, "print");
                assert_eq!(l, 1);
            },
            _ => panic!("Should be UnexpectedEnd error.")
        }
    }

    #[test]
    fn test_right_paren_mismatch() {
        let s = Scanner::scan("(1 + 1");
        let mut p = Parser::new(s.tokens());
        let e = p.expression().err().unwrap();
        match e {
            Error::ExpectTokenMismatch(et, ft, l) => {
                assert_eq!(et, ")");
                assert_eq!(ft, "eof");
                assert_eq!(l, 1);
            },
            _ => panic!("Should be ExpectTokenMismatch error.")
        }
    }

    #[test]
    fn test_unary_unexpected_end() {
        let s = Scanner::scan("");
        let mut p = Parser::new(s.tokens());
        p.synchronize();
        match p.unary().err().unwrap() {
            Error::UnexpectedEnd => { },
            _ => panic!("Should be UnexpectedEnd error.")
        }
    }
}
