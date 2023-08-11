use crate::error::LoxError;
use crate::scan::span::{
    CodePoint,
    Span,
};
use crate::scan::token::Token;
use crate::scan::token::simple::SimpleTokenEnum;
use crate::scan::token::identifier::IdentifierToken;
use crate::parse::expr::Expression;
use crate::parse::expr::assign::AssignExpression;
use crate::parse::expr::binary::{
    BinaryExpression,
    BinaryExpressionEnum,
};
use crate::parse::expr::grouping::GroupingExpression;
use crate::parse::expr::literal::{
    LiteralExpression,
    LiteralExpressionEnum,
};
use crate::parse::expr::logical::{
    LogicalExpression,
    LogicalExpressionEnum,
};
use crate::parse::expr::unary::{
    UnaryExpression,
    UnaryExpressionEnum,
};
use crate::parse::expr::variable::VariableExpression;
use crate::parse::stmt::Statement;
use crate::parse::stmt::block::BlockStatement;
use crate::parse::stmt::expression::ExpressionStatement;
use crate::parse::stmt::r#for::ForStatement;
use crate::parse::stmt::ifelse::IfStatement;
use crate::parse::stmt::print::PrintStatement;
use crate::parse::stmt::var_declare::VarDeclareStatement;
use crate::parse::stmt::r#while::WhileStatement;
use crate::{
    assign_expression,
    binary_expression,
    grouping_expression,
    literal_expression,
    logical_expression,
    unary_expression,
    variable_expression,
    block_statement,
    expression_statement,
    for_statement,
    if_statement,
    print_statement,
    var_declare_statement,
    while_statement,
};

#[derive(Debug, PartialEq)]
pub enum Error {
    UnexpectedEnd(CodePoint),
    UnexpectedToken(Span),
    ExpectTokenMismatch(String, Span),
    ExpectTokenNotFound(String, CodePoint),
    ExpectIdentifier(Span),
}

impl LoxError for Error {
    fn print(&self, src_lines: &Vec<&str>) -> String {
        match self {
            Error::UnexpectedEnd(cp) => {
                let mut out = "Error: Unexpected end.\r\n".to_owned();
                out += cp.indication_string(&src_lines).as_ref();
                return out;
            }
            Error::UnexpectedToken(s) => {
                let mut out = format!("Error: Unexpected token: {}\r\n", s.token_string(&src_lines, 10));
                out += s.indication_string(&src_lines).as_ref();
                return out;
            }
            Error::ExpectTokenMismatch(et, s) => {
                let mut out = format!(
                    "Error: Expect token: {} but found: {}\r\n",
                    et,
                    s.token_string(&src_lines, 10)
                );
                out += s.indication_string(&src_lines).as_ref();
                return out;
            }
            Error::ExpectTokenNotFound(et, cp) => {
                let mut out = format!("Error: Expect token: {} but not found.\r\n", et);
                out += cp.indication_string(&src_lines).as_ref();
                return out;
            }
            Error::ExpectIdentifier(s) => {
                let mut out = "Error: Expect identifier.\r\n".to_owned();
                out += s.indication_string(&src_lines).as_ref();
                return out;
            }
        }
        
    }
}

pub type ParserOutput = (Vec<Statement>, Vec<Error>);

pub struct Parser<'tokens> {
    tokens: &'tokens Vec<Token>,
    current: usize,
}

impl<'tokens> Parser<'tokens> {
    pub fn new(tokens: &'tokens Vec<Token>) -> Parser<'tokens> {
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

        if let Token::Simple(st) = self.tokens[self.current] {
            if st.variant() == SimpleTokenEnum::Eof {
                return true;
            }
        }

        return false;
    }

    fn peek(&self) -> Option<&'tokens Token> {
        self.tokens.get(self.current)
    }

    fn peek_last(&self) -> &'tokens Token {
        &self.tokens[self.current - 1]
    }

    fn consume(&mut self, variant: SimpleTokenEnum) -> Result<&'tokens Token, Error> {
        if let Some(t) = self.peek() {
            if let Token::Simple(st) = t {
                if st.variant() == variant {
                    self.advance();
                    return Ok(t);
                }
                else if st.variant() == SimpleTokenEnum::Eof {
                    return Err(self.expect_token_not_found_error(variant.lexeme()));
                }
            }

            return Err(
                Error::ExpectTokenMismatch(
                    variant.lexeme().to_string(),
                    t.span(),
                )
            );
        }
        else {
            return Err(self.expect_token_not_found_error(variant.lexeme()))
        }
    }

    fn consume_identifier(&mut self) -> Result<&'tokens IdentifierToken, Error> {
        if let Some(t) = self.peek() {
            match t {
                Token::Identifier(ref it) => {
                    self.advance();
                    Ok(it)
                }
                _ => Err(Error::ExpectIdentifier(t.span()))
            }
        }
        else {
            Err(self.unexpected_end_error())
        }
    }

    pub fn synchronize(&mut self) {
        self.advance();
        while !self.is_end() {
            if let Token::Simple(st) = self.peek_last() {
                if st.variant() == SimpleTokenEnum::Semicolon {
                    return;
                }
            }

            if let Some(t) = self.peek() {
                if let Token::Simple(st) = t {
                    match st.variant() {
                        SimpleTokenEnum::Class |
                        SimpleTokenEnum::Fun |
                        SimpleTokenEnum::Var |
                        SimpleTokenEnum::For |
                        SimpleTokenEnum::If |
                        SimpleTokenEnum::While |
                        SimpleTokenEnum::Print |
                        SimpleTokenEnum::Return => {
                            return;
                        }
                        _ => { }
                    }
                }

                self.advance();
                continue;
            }
            else {
                return;
            }
        }
    }

    pub fn expression(&mut self) -> Result<Expression, Error> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expression, Error> {
        if let Ok(ident) = self.consume_identifier() {
            if self.consume(SimpleTokenEnum::Equal).is_ok() {
                let expr = self.assignment()?;
                let span = Span::new(ident.span().start, expr.span().end);
                return Ok(assign_expression!(ident.name(), expr, span));
            }
            else {
                self.fallback();
            }
        }
        return self.logical_or();
    }

    fn logical_or(&mut self) -> Result<Expression, Error> {
        let mut lhs = self.logical_and()?;
        while self.consume(SimpleTokenEnum::Or).is_ok() {
            let rhs = self.logical_and()?;
            let span = Span::new(lhs.span().start, rhs.span().end);
            lhs = logical_expression!(Or, lhs, rhs, span);
        }
        Ok(lhs)
    }

    fn logical_and(&mut self) -> Result<Expression, Error> {
        let mut lhs = self.equality()?;
        while self.consume(SimpleTokenEnum::And).is_ok() {
            let rhs = self.equality()?;
            let span = Span::new(lhs.span().start, rhs.span().end);
            lhs = logical_expression!(And, lhs, rhs, span);
        }
        Ok(lhs)
    }

    fn equality(&mut self) -> Result<Expression, Error> {
        let mut lhs = self.comparison()?;
        loop {
            if let Some(Token::Simple(st)) = self.peek() {
                match st.variant() {
                    SimpleTokenEnum::EqualEqual => {
                        self.advance();
                        let rhs = self.comparison()?;
                        let span = Span::new(lhs.span().start, rhs.span().end);
                        lhs = binary_expression!(Equal, lhs, rhs, span);
                        continue;
                    }
                    SimpleTokenEnum::BangEqual => {
                        self.advance();
                        let rhs = self.comparison()?;
                        let span = Span::new(lhs.span().start, rhs.span().end);
                        lhs = binary_expression!(NotEqual, lhs, rhs, span);
                        continue;
                    }
                    _ => break
                }
            }
            else {
                break;
            }
        }
        Ok(lhs)
    }

    fn comparison(&mut self) -> Result<Expression, Error> {
        let mut lhs = self.term()?;
        loop {
            if let Some(Token::Simple(st)) = self.peek() {
                match st.variant() {
                    SimpleTokenEnum::Less => {
                        self.advance();
                        let rhs = self.term()?;
                        let span = Span::new(lhs.span().start, rhs.span().end);
                        lhs = binary_expression!(Less, lhs, rhs, span);
                        continue;
                    }
                    SimpleTokenEnum::LessEqual => {
                        self.advance();
                        let rhs = self.term()?;
                        let span = Span::new(lhs.span().start, rhs.span().end);
                        lhs = binary_expression!(LessEqual, lhs, rhs, span);
                        continue;
                    }
                    SimpleTokenEnum::Greater => {
                        self.advance();
                        let rhs = self.term()?;
                        let span = Span::new(lhs.span().start, rhs.span().end);
                        lhs = binary_expression!(Greater, lhs, rhs, span);
                        continue;
                    }
                    SimpleTokenEnum::GreaterEqual => {
                        self.advance();
                        let rhs = self.term()?;
                        let span = Span::new(lhs.span().start, rhs.span().end);
                        lhs = binary_expression!(GreaterEqual, lhs, rhs, span);
                        continue;
                    }
                    _ => break
                }
            }
            else {
                break;
            }
        }
        Ok(lhs)
    }

    fn term(&mut self) -> Result<Expression, Error> {
        let mut lhs = self.factor()?;
        loop {
            if let Some(Token::Simple(st)) = self.peek() {
                match st.variant() {
                    SimpleTokenEnum::Plus => {
                        self.advance();
                        let rhs = self.factor()?;
                        let span = Span::new(lhs.span().start, rhs.span().end);
                        lhs = binary_expression!(Plus, lhs, rhs, span);
                        continue;
                    }
                    SimpleTokenEnum::Minus => {
                        self.advance();
                        let rhs = self.factor()?;
                        let span = Span::new(lhs.span().start, rhs.span().end);
                        lhs = binary_expression!(Minus, lhs, rhs, span);
                        continue;
                    }
                    _ => break
                }
            }
            else {
                break;
            }
        }
        Ok(lhs)
    }

    fn factor(&mut self) -> Result<Expression, Error> {
        let mut lhs = self.unary()?;
        loop {
            if let Some(Token::Simple(st)) = self.peek() {
                match st.variant() {
                    SimpleTokenEnum::Star => {
                        self.advance();
                        let rhs = self.unary()?;
                        let span = Span::new(lhs.span().start, rhs.span().end);
                        lhs = binary_expression!(Multiply, lhs, rhs, span);
                        continue;
                    }
                    SimpleTokenEnum::Slash => {
                        self.advance();
                        let rhs = self.unary()?;
                        let span = Span::new(lhs.span().start, rhs.span().end);
                        lhs = binary_expression!(Divide, lhs, rhs, span);
                        continue;
                    }
                    _ => break
                }
            }
            else {
                break;
            }
        }
        Ok(lhs)
    }

    fn unary(&mut self) -> Result<Expression, Error> {
        if let Some(t) = self.peek() {
            if let Token::Simple(st) = t {
                match st.variant() {
                    SimpleTokenEnum::Bang  => {
                        self.advance();
                        let rhs = self.unary()?;
                        let span = Span::new(t.span().start, rhs.span().end);
                        return Ok(unary_expression!(Not, rhs, span));
                    }
                    SimpleTokenEnum::Minus  => {
                        self.advance();
                        let rhs = self.unary()?;
                        let span = Span::new(t.span().start, rhs.span().end);
                        return Ok(unary_expression!(Negative, rhs, span));
                    }
                    _ => { }
                }
            }

            return self.primary();
        }
        else {
            return Err(self.unexpected_end_error());
        }
    }

    fn primary(&mut self) -> Result<Expression, Error> {
        if let Some(t) = self.peek() {
            match t {
                Token::Number(nt) => {
                    self.advance();
                    Ok(literal_expression!(Number, nt.clone(), nt.span()))
                }
                Token::String(st) => {
                    self.advance();
                    Ok(literal_expression!(String, st.clone(), st.span()))
                }
                Token::Identifier(it) => {
                    self.advance();
                    Ok(variable_expression!(it.name(), it.span()))
                }
                Token::Simple(st) => {
                    match st.variant() {
                        SimpleTokenEnum::True => {
                            self.advance();
                            Ok(literal_expression!(True, st.span()))
                        }
                        SimpleTokenEnum::False => {
                            self.advance();
                            Ok(literal_expression!(False, st.span()))
                        }
                        SimpleTokenEnum::Nil => {
                            self.advance();
                            Ok(literal_expression!(Nil, st.span()))
                        }
                        SimpleTokenEnum::LeftParen => {
                            self.advance();
                            let e = self.expression()?;
                            Ok(grouping_expression!(
                                e,
                                Span::new(
                                    st.span().start,
                                    self.consume(SimpleTokenEnum::RightParen)?.span().end,
                                )
                            ))
                        }
                        SimpleTokenEnum::Eof => {
                            Err(self.unexpected_end_error())
                        }
                        _ => {
                            Err(Error::UnexpectedToken(t.span()))
                        }
                    }
                }
            }
        }
        else {
            Err(self.unexpected_end_error())
        }
    }

    pub fn statement(&mut self) -> Result<Statement, Error> {
        if let Some(t) = self.peek() {
            if let Token::Simple(st) = t {
                match st.variant() {
                    SimpleTokenEnum::Var => {
                        self.advance();
                        self.var_declare_statement()
                    }
                    SimpleTokenEnum::If => {
                        self.advance();
                        self.ifelse()
                    }
                    SimpleTokenEnum::While => {
                        self.advance();
                        self.r#while()
                    }
                    SimpleTokenEnum::For => {
                        self.advance();
                        self.r#for()
                    }
                    SimpleTokenEnum::LeftBrace => {
                        self.advance();
                        self.block()
                    }
                    SimpleTokenEnum::Print => {
                        self.advance();
                        self.print_statement()
                    }
                    _ => {
                        self.expression_statement()
                    }
                }
            }
            else {
                self.expression_statement()
            }
        }
        else {
            Err(self.unexpected_end_error())
        }
    }

    fn not_declaration_statement(&mut self) -> Result<Statement, Error> {
        if let Some(t) = self.peek() {
            if let Token::Simple(st) = t {
                match st.variant() {
                    SimpleTokenEnum::If => {
                        self.advance();
                        self.ifelse()
                    }
                    SimpleTokenEnum::While => {
                        self.advance();
                        self.r#while()
                    }
                    SimpleTokenEnum::For => {
                        self.advance();
                        self.r#for()
                    }
                    SimpleTokenEnum::LeftBrace => {
                        self.advance();
                        self.block()
                    }
                    SimpleTokenEnum::Print => {
                        self.advance();
                        self.print_statement()
                    }
                    _ => {
                        self.expression_statement()
                    }
                }
            }
            else {
                self.expression_statement()
            }
        }
        else {
            Err(self.unexpected_end_error())
        }
    }

    fn ifelse(&mut self) -> Result<Statement, Error> {
        let cp_start = self.peek_last().span().start;
        self.consume(SimpleTokenEnum::LeftParen)?;
        let condition = self.expression()?;
        self.consume(SimpleTokenEnum::RightParen)?;
        let then_stmt = self.not_declaration_statement()?;
        let mut else_stmt = None;
        if self.consume(SimpleTokenEnum::Else).is_ok() {
            else_stmt = Some(self.not_declaration_statement()?);
        }
        let cp_end = self.peek_last().span().end;
        if let Some(else_stmt) = else_stmt {
            Ok(if_statement!(
                condition,
                then_stmt,
                else_stmt,
                Span::new(cp_start, cp_end)
            ))
        }
        else {
            Ok(if_statement!(
                condition,
                then_stmt,
                Span::new(cp_start, cp_end)
            ))
        }
    }

    fn r#while(&mut self) -> Result<Statement, Error> {
        let cp_start = self.peek_last().span().start;

        self.consume(SimpleTokenEnum::LeftParen)?;

        let condition = self.expression()?;

        self.consume(SimpleTokenEnum::RightParen)?;

        let body = self.not_declaration_statement()?;

        let cp_end = self.peek_last().span().end;

        Ok(while_statement!(
            condition,
            body,
            Span::new(cp_start, cp_end)
        ))
    }

    fn r#for(&mut self) -> Result<Statement, Error> {
        let cp_start = self.peek_last().span().start;

        self.consume(SimpleTokenEnum::LeftParen)?;

        let initializer = if self.consume(SimpleTokenEnum::Var).is_ok() {
            Some(self.var_declare_statement()?)
        }
        else if self.consume(SimpleTokenEnum::Semicolon).is_ok() {
            None
        }
        else {
            Some(self.expression_statement()?)
        };

        let condition = if self.consume(SimpleTokenEnum::Semicolon).is_ok() {
            None
        }
        else {
            let condition = self.expression()?;
            self.consume(SimpleTokenEnum::Semicolon)?;
            Some(condition)
        };

        let increment = if self.consume(SimpleTokenEnum::RightParen).is_ok() {
            None
        }
        else {
            let increment = self.expression()?;
            self.consume(SimpleTokenEnum::RightParen)?;
            Some(increment)
        };

        let body = self.not_declaration_statement()?;

        let cp_end = self.peek_last().span().end;

        return Ok(for_statement!(
            initializer,
            condition,
            increment,
            body,
            Span::new(cp_start, cp_end),
        ));
    }

    fn block(&mut self) -> Result<Statement, Error> {
        let cp_start = self.peek_last().span().start;
        let mut stmts = Vec::new();
        loop {
            if let Ok(t) = self.consume(SimpleTokenEnum::RightBrace) {
                return Ok(block_statement!(stmts, Span::new(cp_start, t.span().end)));
            }
            else if self.is_end() {
                return Err(self.expect_token_not_found_error(SimpleTokenEnum::RightBrace.lexeme()));
            }
            else {
                stmts.push(self.statement()?);
            }
        }
    }

    fn var_declare_statement(&mut self) -> Result<Statement, Error> {
        let cp_start = self.peek_last().span().start;

        let name = self.consume_identifier()?.name();

        let mut initializer = None;
        if self.consume(SimpleTokenEnum::Equal).is_ok() {
            initializer = Some(self.expression()?);
        }

        self.consume(SimpleTokenEnum::Semicolon)?;

        let cp_end = self.peek_last().span().end;

        return Ok(var_declare_statement!(
            name,
            initializer,
            Span::new(cp_start, cp_end)
        ));
    }

    fn print_statement(&mut self) -> Result<Statement, Error> {
        let cp_start = self.peek_last().span().start;
        let value = self.expression()?;
        self.consume(SimpleTokenEnum::Semicolon)?;
        let cp_end = self.peek_last().span().end;
        Ok(print_statement!(
            value,
            Span::new(cp_start, cp_end)
        ))
    }

    fn expression_statement(&mut self) -> Result<Statement, Error> {
        let expr = self.expression()?;
        let cp_start = expr.span().start;
        self.consume(SimpleTokenEnum::Semicolon)?;
        let cp_end = self.peek_last().span().end;
        Ok(expression_statement!(
            expr,
            Span::new(cp_start, cp_end)
        ))
    }

    fn expect_token_not_found_error(&self, token: &str) -> Error {
        if self.current == 0 {
            Error::ExpectTokenNotFound(token.to_string(), CodePoint::new(0, 0))
        }
        else {
            Error::ExpectTokenNotFound(token.to_string(), self.peek_last().span().end)
        }
    }

    fn unexpected_end_error(&self) -> Error {
        if self.current == 0 {
            Error::UnexpectedEnd(CodePoint::new(0, 0))
        }
        else {
            Error::UnexpectedEnd(self.peek_last().span().end)
        }
    }

    pub fn parse(tokens: &Vec<Token>) -> ParserOutput {
        let mut p = Parser::new(tokens);
        let mut stmts: Vec<Statement> = Vec::new();
        let mut errors: Vec<Error> = Vec::new();

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
    use crate::scan::span::{
        Span,
        CodePoint,
    };
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
            // Logical.
            ("x or y", "(or x y)"),
            ("x and y", "(and x y)"),
            ("x or y and z", "(or x (and y z))"),
            ("w and x or y and z", "(or (and w x) (and y z))"),
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
            Error::UnexpectedEnd(cp) => {
                assert_eq!(cp.line, 0);
                assert_eq!(cp.char, 0);
            },
            _ => panic!("Should be UnexpectedEnd error.")
        }
    }

    #[test]
    fn test_unexpected_token() {
        let tokens = "print".scan().0;
        let mut parser = Parser::new(&tokens);
        match parser.expression().err().unwrap() {
            Error::UnexpectedToken(s) => {
                assert_eq!(s.start.line, 0);
                assert_eq!(s.start.char, 0);
                assert_eq!(s.end.line, 0);
                assert_eq!(s.end.char, 5);
            },
            _ => panic!("Should be UnexpectedEnd error.")
        }
    }

    #[test]
    fn test_right_paren_mismatch() {
        let tokens = "(1 + 1}".scan().0;
        let mut parser = Parser::new(&tokens);
        match parser.expression().err().unwrap() {
            Error::ExpectTokenMismatch(ts, s) => {
                assert_eq!(ts, ")");
                assert_eq!(s.start.line, 0);
                assert_eq!(s.start.char, 6);
                assert_eq!(s.end.line, 0);
                assert_eq!(s.end.char, 7);
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
            Error::UnexpectedEnd(cp) => {
                assert_eq!(cp.line, 0);
                assert_eq!(cp.char, 0);
            },
            _ => panic!("Should be UnexpectedEnd error.")
        }
    }

    #[test]
    fn test_statement() {
        let tests: Vec<(&str, Result<&str, Error>)> = vec![
            // Variable declaration statement.
            (
                "var foo;",
                Ok("var foo;")
            ),
            (
                "var foo = true;",
                Ok("var foo = true;")
            ),
            (
                "var true;",
                Err(Error::ExpectIdentifier(Span::new(CodePoint::new(0, 4), CodePoint::new(0, 8))))
            ),
            (
                "var foo =",
                Err(Error::UnexpectedEnd(CodePoint::new(0, 9)))
            ),
            (
                "var foo = true",
                Err(Error::ExpectTokenNotFound(";".to_owned(), CodePoint::new(0, 14)))
            ),
            // Block statement.
            (
                "{var foo = true; foo = false;}",
                Ok("{var foo = true; (= foo false);}")
            ),
            (
                "{var foo}",
                Err(
                    Error::ExpectTokenMismatch(
                        ";".to_owned(),
                        Span::new(CodePoint::new(0, 8), CodePoint::new(0, 9)),
                    )
                ),
            ),
            (
                "{var foo;",
                Err(Error::ExpectTokenNotFound("}".to_owned(), CodePoint::new(0, 9)))
            ),
            // Ifelse.
            (
                "if (true) print \"hello\"; else print \"oops\";",
                Ok("if true print \"hello\"; else print \"oops\";")
            ),
            (
                "if true) print \"hello\"; else print \"oops\";",
                Err(
                    Error::ExpectTokenMismatch(
                        "(".to_owned(),
                        Span::new(CodePoint::new(0, 3), CodePoint::new(0, 7)),
                    )
                ),
            ),
            (
                "if (true print \"hello\"; else print \"oops\";",
                Err(
                    Error::ExpectTokenMismatch(
                        ")".to_owned(),
                        Span::new(CodePoint::new(0, 9), CodePoint::new(0, 14)),
                    )
                ),
            ),
            // Print statement.
            (
                "print \"hello\";",
                Ok("print \"hello\";")
            ),
            (
                "print",
                Err(Error::UnexpectedEnd(CodePoint::new(0, 5))),
            ),
            (
                "print fun",
                Err(Error::UnexpectedToken(Span::new(CodePoint::new(0, 6), CodePoint::new(0, 9)))),
            ),
            (
                "print \"hello\"",
                Err(Error::ExpectTokenNotFound(";".to_owned(), CodePoint::new(0, 13))),
            ),
            // Expression statement.
            (
                "true;",
                Ok("true;")
            ),
            (
                "",
                Err(Error::UnexpectedEnd(CodePoint::new(0, 0))),
            ),
            (
                "true",
                Err(Error::ExpectTokenNotFound(";".to_owned(), CodePoint::new(0, 4))),
            ),
            // While statement.
            (
                "while (foo) print 1;",
                Ok("while foo print 1;")
            ),
            (
                "while foo) print 1;",
                Err(
                    Error::ExpectTokenMismatch(
                        "(".to_owned(),
                        Span::new(CodePoint::new(0, 6), CodePoint::new(0, 9)),
                    )
                ),
            ),
            (
                "while (foo print 1;",
                Err(
                    Error::ExpectTokenMismatch(
                        ")".to_owned(),
                        Span::new(CodePoint::new(0, 11), CodePoint::new(0, 16)),
                    )
                ),
            ),
            // For statement.
            (
                "for (;;) print 1;",
                Ok("for (;;) print 1;")
            ),
            (
                "for (var i = 0; i < 10; i = i + 1) print 1;",
                Ok("for (var i = 0; (< i 10); (= i (+ i 1))) print 1;"),
            ),
        ];
        for (src, expect) in tests {
            let tokens = src.scan().0;
            let mut parser = Parser::new(&tokens);
            match expect {
                Ok(s) => assert_eq!(parser.statement().unwrap().print(), s),
                Err(e) => assert_eq!(parser.statement().unwrap_err(), e),
            }
        }
    }
}
