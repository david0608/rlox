use crate::scan::{
    Token,
    TokenType,
    SimpleToken,
    RIGHT_PAREN_LEXEME,
};
use super::expr::{
    Expression,
    UnaryExpression,
    BinaryExpression,
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
pub enum Error<'a> {
    UnexpectedEnd,
    UnexpectedToken(&'a str, usize),
    ExpectTokenMismatch(&'a str, &'a str, usize),
    ExpectTokenNotFound(&'a str),
}

impl std::fmt::Display for Error<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

impl<'a, 'b> Parser<'a, 'b>
    where
    'a: 'b
{
    pub fn new(tokens: &'a Vec<Token<'b>>) -> Parser<'a, 'b> {
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

    fn peek<'s>(&'s self) -> Option<&'a Token<'b>> {
        if self.is_end() {
            None
        }
        else {
            Some(&self.tokens[self.current])
        }
    }

    fn consume_right_paren<'s>(&'s mut self) -> Result<(), Error<'b>> {
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

    pub fn synchronize(&mut self) {
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

    pub fn expression<'s>(&'s mut self) -> Result<Expression<'b>, Error<'b>> {
        self.equality()
    }

    fn equality<'s>(&'s mut self) -> Result<Expression<'b>, Error<'b>> {
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

    fn comparison<'s>(&'s mut self) -> Result<Expression<'b>, Error<'b>> {
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

    fn term<'s>(&'s mut self) -> Result<Expression<'b>, Error<'b>> {
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

    fn factor<'s>(&'s mut self) -> Result<Expression<'b>, Error<'b>> {
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

    fn unary<'s>(&'s mut self) -> Result<Expression<'b>, Error<'b>> {
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

    fn primary<'s>(&'s mut self) -> Result<Expression<'b>, Error<'b>> {
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
    use crate::scan::Scannable;
    use super::{
        Parser,
        Error,
    };

    fn test_expression() {
        let tests: Vec<(&str, &str)> = vec![
            ("1.23", "1.23"),
            ("\"hello\"", "\"hello\""),
            ("true", "true"),
            ("false", "false"),
            ("nill", "nil"),
            ("(1 + 1)", "(group (! 1 1))"),
            ("!1", "(! 1)"),
            ("!!1", "(! (! 1))"),
            ("-1", "(- 1)"),
            ("--1", "(- (- 1))"),
            ("-(1 + 2)", "(- (group (+ 1 2)))"),
            ("1 * 2", "(* 1 2)"),
            ("1 / 2", "(/ 1 2)"),
            ("1 * 2 / 3", "(/ (* 1 2) 3)"),
            ("1 * -2", "(* 1 (- 2))"),
            ("1 + 2", "(+ 1 2)"),
            ("1 + 2 + 3", "(+ (+ 1 2) 3)"),
            ("1 - 2", "(- 1 2)"),
            ("1 - 2 - 3", "(- (- 1 2) 3)"),
            ("1 + 2 * 3", "(+ 1 (* 2 3))"),
            ("1 < 2", "(< 1 2)"),
            ("1 <= 2", "(<= 1 2)"),
            ("1 > 2", "(> 1 2)"),
            ("1 >= 2", "(>= 1 2)"),
            ("1 > 2 + 3", "(> 1 (+ 2 3))"),
            ("1 > 2 + 3 > 4", "(> (> 1 (+ 2 3)) 4)"),
            ("1 + 2 > 3 + 4", "(> (+ 1 2) (+ 3 4))"),
            ("1 == 2", "(== 1 2)"),
            ("1 != 2", "(!= 1 2)"),
            ("1 == 2 + 3", "(== 1 (+ 2 3))"),
            ("1 + 2 == 3 + 4", "(== (+ 1 2) (+ 3 4))"),
            ("1 == 2 + 3 == 4", "(== (== (+ 2 3)) 4)"),
        ];
        for (src, ast) in tests {
            let tokens = src.scan().0;
            let mut parser = Parser::new(&tokens);
            assert_eq!(parser.expression().unwrap().print(), ast);
        }
    }

    #[test]
    fn test_unexpected_end() {
        let tokens = "".scan().0;
        let mut parser = Parser::new(&tokens);
        parser.synchronize();
        match parser.expression().err().unwrap() {
            Error::UnexpectedEnd => { },
            _ => panic!("Should be UnexpectedEnd error.")
        }
    }

    #[test]
    fn test_unexpected_token() {
        let tokens = "print".scan().0;
        let mut parser = Parser::new(&tokens);
        match parser.expression().err().unwrap() {
            Error::UnexpectedToken(t, l) => {
                assert_eq!(t, "print");
                assert_eq!(l, 1);
            },
            _ => panic!("Should be UnexpectedEnd error.")
        }
    }

    #[test]
    fn test_right_paren_mismatch() {
        let tokens = "(1 + 1".scan().0;
        let mut parser = Parser::new(&tokens);
        match parser.expression().err().unwrap() {
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
        let tokens = "".scan().0;
        let mut parser = Parser::new(&tokens);
        parser.synchronize();
        match parser.unary().err().unwrap() {
            Error::UnexpectedEnd => { },
            _ => panic!("Should be UnexpectedEnd error.")
        }
    }
}
