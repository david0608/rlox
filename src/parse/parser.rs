use crate::scan::token::{
    Token,
    TokenType,
    SimpleToken,
    IdentToken,
};
use crate::parse::expr::{
    Expression,
    UnaryExpression,
    BinaryExpression,
    LiteralExpression,
    GroupingExpression,
    VariableExpression,
    AssignExpression,
};
use crate::parse::stmt::{
    Statement,
    VarDeclareStatement,
    BlockStatement,
    ExpressionStatement,
    PrintStatement,
};
use crate::{
    literal_expression,
    grouping_expression,
    unary_expression,
    binary_expression,
    variable_expression,
    assign_expression,
    var_declare_statement,
    block_statement,
    expression_statement,
    print_statement,
};

#[derive(Debug)]
pub enum Error<'src> {
    UnexpectedEnd,
    UnexpectedToken(&'src str, usize),
    ExpectTokenMismatch(&'src str, &'src str, usize),
    ExpectTokenNotFound(&'src str),
    ExpectIdentifier(usize),
}

impl std::fmt::Display for Error<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::UnexpectedEnd => write!(f, "Unexpected end of code."),
            Error::UnexpectedToken(t, l) => write!(f, "line {}: Unexpected token: {}.", l, t),
            Error::ExpectTokenMismatch(et, ft, l) => write!(f, "line {}: Expect token: {} but found: {}.", l, et, ft),
            Error::ExpectTokenNotFound(t) => write!(f, "Expect token: {} but not found.", t),
            Error::ExpectIdentifier(l) => write!(f, "line {}: Expect identifier.", l),
        }
    }
}

pub type ParserOutput<'src> = (Vec<Statement<'src>>, Vec<Error<'src>>);

pub struct Parser<'tokens, 'src> {
    tokens: &'tokens Vec<Token<'src>>,
    current: usize,
}

impl<'tokens, 'src> Parser<'tokens, 'src> {
    pub fn new(tokens: &'tokens Vec<Token<'src>>) -> Parser<'tokens, 'src> {
        Parser {
            tokens,
            current: 0,
        }
    }

    fn advance(&mut self) {
        self.current += 1;
    }

    fn fallback(&mut self) {
        self.current -= 1;
    }

    fn is_end(&self) -> bool {
        if self.current >= self.tokens.len() {
            return true;
        }
        else if let TokenType::Simple(SimpleToken::Eof) = self.tokens[self.current].token_type() {
            return true;
        }
        else {
            return false;
        }
    }

    fn peek(&self) -> Option<&'tokens Token<'src>> {
        if self.is_end() {
            None
        }
        else {
            Some(&self.tokens[self.current])
        }
    }

    fn peek_last(&self) -> &'tokens Token<'src> {
        &self.tokens[self.current - 1]
    }

    fn consume(&mut self, token: SimpleToken) -> Result<(), Error<'src>> {
        if let Some(t) = self.peek() {
            if let TokenType::Simple(st) = t.token_type() {
                if st == &token {
                    self.advance();
                    Ok(())
                }
                else {
                    Err(Error::ExpectTokenMismatch(token.lexeme(), t.lexeme(), t.line()))
                }
            }
            else {
                Err(Error::ExpectTokenMismatch(token.lexeme(), t.lexeme(), t.line()))
            }
        }
        else {
            Err(Error::ExpectTokenNotFound(token.lexeme()))
        }
    }

    fn consume_identifier(&mut self) -> Result<&'tokens IdentToken<'src>, Error<'src>> {
        if let Some(t) = self.peek() {
            match t.token_type() {
                TokenType::Ident(i) => {
                    self.advance();
                    Ok(i)
                }
                _ => Err(Error::ExpectIdentifier(t.line()))
            }
        }
        else {
            Err(Error::UnexpectedEnd)
        }
    }

    pub fn synchronize(&mut self) {
        self.advance();
        while !self.is_end() {
            if let TokenType::Simple(SimpleToken::Semicolon) = self.peek_last().token_type() {
                return;
            }
            else {
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
    }

    pub fn expression(&mut self) -> Result<Expression<'src>, Error<'src>> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expression<'src>, Error<'src>> {
        if let Ok(i) = self.consume_identifier() {
            if self.consume(SimpleToken::Equal).is_ok() {
                return Ok(assign_expression!(*i, self.assignment()?));
            }
            else {
                self.fallback();
            }
        }

        return self.equality();
    }

    fn equality(&mut self) -> Result<Expression<'src>, Error<'src>> {
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

    fn comparison(&mut self) -> Result<Expression<'src>, Error<'src>> {
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

    fn term(&mut self) -> Result<Expression<'src>, Error<'src>> {
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

    fn factor(&mut self) -> Result<Expression<'src>, Error<'src>> {
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

    fn unary(&mut self) -> Result<Expression<'src>, Error<'src>> {
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

    fn primary(&mut self) -> Result<Expression<'src>, Error<'src>> {
        if let Some(t) = self.peek() {
            match t.token_type() {
                TokenType::Number(nt) => {
                    self.advance();
                    Ok(literal_expression!(Number, *nt))
                }
                TokenType::String(st) => {
                    self.advance();
                    Ok(literal_expression!(String, *st))
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
                TokenType::Ident(it) => {
                    self.advance();
                    Ok(variable_expression!(*it))
                }
                TokenType::Simple(SimpleToken::LeftParen) => {
                    self.advance();
                    let e = self.expression()?;
                    self.consume(SimpleToken::RightParen)?;
                    Ok(grouping_expression!(e))
                }
                TokenType::Simple(SimpleToken::Eof) => {
                    Err(Error::UnexpectedEnd)
                }
                _ => {
                    Err(Error::UnexpectedToken(t.lexeme(), t.line()))
                }
            }
        }
        else {
            Err(Error::UnexpectedEnd)
        }
    }

    pub fn statement(&mut self) -> Result<Statement<'src>, Error<'src>> {
        self.declaration()
    }

    fn declaration(&mut self) -> Result<Statement<'src>, Error<'src>> {
        if let Some(t) = self.peek() {
            match t.token_type() {
                TokenType::Simple(SimpleToken::LeftBrace) => {
                    self.advance();
                    self.block()
                }
                TokenType::Simple(SimpleToken::Var) => {
                    self.advance();
                    self.var_declare_statement()
                }
                TokenType::Simple(SimpleToken::Print) => {
                    self.advance();
                    self.print_statement()
                }
                _ => {
                    self.expression_statement()
                }
            }
        }
        else {
            Err(Error::UnexpectedEnd)
        }
    }

    fn block(&mut self) -> Result<Statement<'src>, Error<'src>> {
        let mut stmts = Vec::new();
        loop {
            if self.consume(SimpleToken::RightBrace).is_ok() {
                return Ok(block_statement!(stmts));
            }
            else if self.is_end() {
                return Err(Error::ExpectTokenNotFound(SimpleToken::RightBrace.lexeme()));
            }
            else {
                stmts.push(self.declaration()?);
            }
        }
    }

    fn var_declare_statement(&mut self) -> Result<Statement<'src>, Error<'src>> {
        let name = *self.consume_identifier()?;
        let mut initializer = None;
        if self.consume(SimpleToken::Equal).is_ok() {
            initializer = Some(self.expression()?);
        }
        self.consume(SimpleToken::Semicolon)?;
        Ok(var_declare_statement!(name, initializer))
    }

    fn print_statement(&mut self) -> Result<Statement<'src>, Error<'src>> {
        let expr = self.expression()?;
        self.consume(SimpleToken::Semicolon)?;
        Ok(print_statement!(expr))
    }

    fn expression_statement(&mut self) -> Result<Statement<'src>, Error<'src>> {
        let expr = self.expression()?;
        self.consume(SimpleToken::Semicolon)?;
        Ok(expression_statement!(expr))
    }

    pub fn parse(tokens: &'tokens Vec<Token<'src>>) -> ParserOutput<'src> {
        let mut p = Parser::new(tokens);
        let mut stmts: Vec<Statement<'src>> = Vec::new();
        let mut errors: Vec<Error<'src>> = Vec::new();

        while !p.is_end() {
            match p.statement() {
                Ok(s) => stmts.push(s),
                Err(e) => {
                    errors.push(e);
                    p.synchronize();
                }
            }
        }

        return (stmts, errors);
    }
}

#[cfg(test)]
mod tests {
    use crate::visitor::Scannable;
    use super::{
        Parser,
        Error,
    };

    #[test]
    fn test_expression() {
        let tests: Vec<(&str, &str)> = vec![
            // Primary.
            ("1.23", "1.23"),
            ("\"hello\"", "\"hello\""),
            ("true", "true"),
            ("false", "false"),
            ("nil", "nil"),
            ("(1 + 1)", "(group (+ 1 1))"),
            // Unary.
            ("!1", "(! 1)"),
            ("!!1", "(! (! 1))"),
            ("-1", "(- 1)"),
            ("--1", "(- (- 1))"),
            ("-(1 + 2)", "(- (group (+ 1 2)))"),
            // Factor.
            ("1 * 2", "(* 1 2)"),
            ("1 / 2", "(/ 1 2)"),
            ("1 * 2 / 3", "(/ (* 1 2) 3)"),
            ("1 * -2", "(* 1 (- 2))"),
            // Term.
            ("1 + 2", "(+ 1 2)"),
            ("1 + 2 + 3", "(+ (+ 1 2) 3)"),
            ("1 - 2", "(- 1 2)"),
            ("1 - 2 - 3", "(- (- 1 2) 3)"),
            ("1 + 2 * 3", "(+ 1 (* 2 3))"),
            // Comparison.
            ("1 < 2", "(< 1 2)"),
            ("1 <= 2", "(<= 1 2)"),
            ("1 > 2", "(> 1 2)"),
            ("1 >= 2", "(>= 1 2)"),
            ("1 > 2 + 3", "(> 1 (+ 2 3))"),
            ("1 > 2 + 3 > 4", "(> (> 1 (+ 2 3)) 4)"),
            ("1 + 2 > 3 + 4", "(> (+ 1 2) (+ 3 4))"),
            // Equality.
            ("1 == 2", "(== 1 2)"),
            ("1 != 2", "(!= 1 2)"),
            ("1 == 2 + 3", "(== 1 (+ 2 3))"),
            ("1 + 2 == 3 + 4", "(== (+ 1 2) (+ 3 4))"),
            ("1 == 2 + 3 == 4", "(== (== 1 (+ 2 3)) 4)"),
            // Assignment.
            ("foo = true", "(= foo true)"),
            ("foo = bar = true", "(= foo (= bar true))"),
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
        let tokens = "(1 + 1}".scan().0;
        let mut parser = Parser::new(&tokens);
        match parser.expression().err().unwrap() {
            Error::ExpectTokenMismatch(et, ft, l) => {
                assert_eq!(et, ")");
                assert_eq!(ft, "}");
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

    #[test]
    fn test_statement() {
        let tests: Vec<(&str, Result<&str, &str>)> = vec![
            // Variable declaration statement.
            ("var foo;", Ok("var foo;")),
            ("var foo = true;", Ok("var foo = true;")),
            ("var true;", Err("line 1: Expect identifier.")),
            ("var foo =", Err("Unexpected end of code.")),
            ("var foo = true", Err("Expect token: ; but not found.")),
            // Block statement.
            ("{var foo = true; foo = false;}", Ok("{var foo = true; (= foo false);}")),
            ("{var foo}", Err("line 1: Expect token: ; but found: }.")),
            ("{var foo;", Err("Expect token: } but not found.")),
            // Print statement.
            ("print \"hello\";", Ok("print \"hello\";")),
            ("print", Err("Unexpected end of code.")),
            ("print fun", Err("line 1: Unexpected token: fun.")),
            ("print \"hello\"", Err("Expect token: ; but not found.")),
            // Expression statement.
            ("true;", Ok("true;")),
            ("", Err("Unexpected end of code.")),
            ("true", Err("Expect token: ; but not found.")),
        ];
        for (src, expect) in tests {
            let tokens = src.scan().0;
            let mut parser = Parser::new(&tokens);
            match expect {
                Ok(s) => assert_eq!(parser.statement().unwrap().print(), s),
                Err(e) => assert_eq!(format!("{}", parser.statement().unwrap_err()), e),
            }
        }
    }
}
