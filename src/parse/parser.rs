use std::collections::HashSet;
use crate::error::LoxError;
use crate::code::Code;
use crate::code::code_point::CodePoint;
use crate::code::code_span::CodeSpan;
use crate::scan::token::Token;
use crate::scan::token::simple::{
    SimpleToken,
    SimpleTokenEnum,
};
use crate::scan::token::identifier::IdentifierToken;
use crate::parse::expression::BoxedExpression;
use crate::parse::expression::assign::AssignExpression;
use crate::parse::expression::binary::{
    BinaryExpression,
    BinaryExpressionEnum,
};
use crate::parse::expression::call::CallExpression;
use crate::parse::expression::grouping::GroupingExpression;
use crate::parse::expression::literal::{
    LiteralExpression,
    LiteralExpressionEnum,
};
use crate::parse::expression::logical::{
    LogicalExpression,
    LogicalExpressionEnum,
};
use crate::parse::expression::unary::{
    UnaryExpression,
    UnaryExpressionEnum,
};
use crate::parse::expression::variable::VariableExpression;
use crate::parse::statement::BoxedStatement;
use crate::parse::statement::block::BlockStatement;
use crate::parse::statement::expression::ExpressionStatement;
use crate::parse::statement::r#for::ForStatement;
use crate::parse::statement::fun_declare::FunDeclareStatement;
use crate::parse::statement::ifelse::IfStatement;
use crate::parse::statement::print::PrintStatement;
use crate::parse::statement::r#return::ReturnStatement;
use crate::parse::statement::var_declare::VarDeclareStatement;
use crate::parse::statement::r#while::WhileStatement;
use crate::{
    assign_expression,
    binary_expression,
    call_expression,
    grouping_expression,
    literal_expression,
    logical_expression,
    unary_expression,
    variable_expression,
    block_statement,
    expression_statement,
    for_statement,
    fun_declare_statement,
    if_statement,
    print_statement,
    return_statement,
    var_declare_statement,
    while_statement,
};

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnexpectedEnd(CodePoint),
    UnexpectedToken(CodeSpan),
    ExpectTokenMismatch(String, CodeSpan),
    ExpectTokenNotFound(String, CodePoint),
    ExpectIdentifier(CodeSpan),
    DuplicatedFunctionParameter(CodeSpan),
}

impl LoxError for ParseError {
    fn print(&self, src_lines: &Vec<&str>) -> String {
        match self {
            ParseError::UnexpectedEnd(cp) => {
                let mut out = "Error: Unexpected end.\r\n".to_owned();
                out += cp.debug_string(&src_lines).as_ref();
                return out;
            }
            ParseError::UnexpectedToken(s) => {
                let mut out = format!("Error: Unexpected token: {}\r\n", s.code_string(&src_lines, 10));
                out += s.debug_string(&src_lines).as_ref();
                return out;
            }
            ParseError::ExpectTokenMismatch(et, s) => {
                let mut out = format!(
                    "Error: Expect token: {} but found: {}\r\n",
                    et,
                    s.code_string(&src_lines, 10)
                );
                out += s.debug_string(&src_lines).as_ref();
                return out;
            }
            ParseError::ExpectTokenNotFound(et, cp) => {
                let mut out = format!("Error: Expect token: {} but not found.\r\n", et);
                out += cp.debug_string(&src_lines).as_ref();
                return out;
            }
            ParseError::ExpectIdentifier(s) => {
                let mut out = "Error: Expect identifier.\r\n".to_owned();
                out += s.debug_string(&src_lines).as_ref();
                return out;
            }
            ParseError::DuplicatedFunctionParameter(s) => {
                let mut out = "Error: Duplicated function parameter.\r\n".to_owned();
                out += s.debug_string(&src_lines).as_ref();
                return out;
            }
        }
        
    }
}

pub type ParserOutput = (Vec<BoxedStatement>, Vec<ParseError>);

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

    fn peek_simple(&self, variant: SimpleTokenEnum) -> Option<&'tokens SimpleToken> {
        if let Some(Token::Simple(st)) = self.peek() {
            if st.variant() == variant {
                return Some(st);
            }
            else {
                return None;
            }
        }
        else {
            return None;
        }
    }

    fn peek_last(&self) -> &'tokens Token {
        &self.tokens[self.current - 1]
    }

    fn consume(&mut self, variant: SimpleTokenEnum) -> Result<&'tokens Token, ParseError> {
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
                ParseError::ExpectTokenMismatch(
                    variant.lexeme().to_string(),
                    t.code_span(),
                )
            );
        }
        else {
            return Err(self.expect_token_not_found_error(variant.lexeme()))
        }
    }

    fn consume_identifier(&mut self) -> Result<&'tokens IdentifierToken, ParseError> {
        if let Some(t) = self.peek() {
            match t {
                Token::Identifier(ref it) => {
                    self.advance();
                    Ok(it)
                }
                _ => Err(ParseError::ExpectIdentifier(t.code_span()))
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

    pub fn expression(&mut self) -> Result<BoxedExpression, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<BoxedExpression, ParseError> {
        if let Ok(ident) = self.consume_identifier() {
            if self.consume(SimpleTokenEnum::Equal).is_ok() {
                let expr = self.assignment()?;
                let span = CodeSpan::new(ident.code_span().start(), expr.code_span().end());
                return Ok(assign_expression!(ident.name(), expr, span));
            }
            else {
                self.fallback();
            }
        }
        return self.logical_or();
    }

    fn logical_or(&mut self) -> Result<BoxedExpression, ParseError> {
        let mut lhs = self.logical_and()?;
        while self.consume(SimpleTokenEnum::Or).is_ok() {
            let rhs = self.logical_and()?;
            let span = CodeSpan::new(lhs.code_span().start(), rhs.code_span().end());
            lhs = logical_expression!(Or, lhs, rhs, span);
        }
        Ok(lhs)
    }

    fn logical_and(&mut self) -> Result<BoxedExpression, ParseError> {
        let mut lhs = self.equality()?;
        while self.consume(SimpleTokenEnum::And).is_ok() {
            let rhs = self.equality()?;
            let span = CodeSpan::new(lhs.code_span().start(), rhs.code_span().end());
            lhs = logical_expression!(And, lhs, rhs, span);
        }
        Ok(lhs)
    }

    fn equality(&mut self) -> Result<BoxedExpression, ParseError> {
        let mut lhs = self.comparison()?;
        loop {
            if let Some(Token::Simple(st)) = self.peek() {
                match st.variant() {
                    SimpleTokenEnum::EqualEqual => {
                        self.advance();
                        let rhs = self.comparison()?;
                        let span = CodeSpan::new(lhs.code_span().start(), rhs.code_span().end());
                        lhs = binary_expression!(Equal, lhs, rhs, span);
                        continue;
                    }
                    SimpleTokenEnum::BangEqual => {
                        self.advance();
                        let rhs = self.comparison()?;
                        let span = CodeSpan::new(lhs.code_span().start(), rhs.code_span().end());
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

    fn comparison(&mut self) -> Result<BoxedExpression, ParseError> {
        let mut lhs = self.term()?;
        loop {
            if let Some(Token::Simple(st)) = self.peek() {
                match st.variant() {
                    SimpleTokenEnum::Less => {
                        self.advance();
                        let rhs = self.term()?;
                        let span = CodeSpan::new(lhs.code_span().start(), rhs.code_span().end());
                        lhs = binary_expression!(Less, lhs, rhs, span);
                        continue;
                    }
                    SimpleTokenEnum::LessEqual => {
                        self.advance();
                        let rhs = self.term()?;
                        let span = CodeSpan::new(lhs.code_span().start(), rhs.code_span().end());
                        lhs = binary_expression!(LessEqual, lhs, rhs, span);
                        continue;
                    }
                    SimpleTokenEnum::Greater => {
                        self.advance();
                        let rhs = self.term()?;
                        let span = CodeSpan::new(lhs.code_span().start(), rhs.code_span().end());
                        lhs = binary_expression!(Greater, lhs, rhs, span);
                        continue;
                    }
                    SimpleTokenEnum::GreaterEqual => {
                        self.advance();
                        let rhs = self.term()?;
                        let span = CodeSpan::new(lhs.code_span().start(), rhs.code_span().end());
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

    fn term(&mut self) -> Result<BoxedExpression, ParseError> {
        let mut lhs = self.factor()?;
        loop {
            if let Some(Token::Simple(st)) = self.peek() {
                match st.variant() {
                    SimpleTokenEnum::Plus => {
                        self.advance();
                        let rhs = self.factor()?;
                        let span = CodeSpan::new(lhs.code_span().start(), rhs.code_span().end());
                        lhs = binary_expression!(Plus, lhs, rhs, span);
                        continue;
                    }
                    SimpleTokenEnum::Minus => {
                        self.advance();
                        let rhs = self.factor()?;
                        let span = CodeSpan::new(lhs.code_span().start(), rhs.code_span().end());
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

    fn factor(&mut self) -> Result<BoxedExpression, ParseError> {
        let mut lhs = self.unary()?;
        loop {
            if let Some(Token::Simple(st)) = self.peek() {
                match st.variant() {
                    SimpleTokenEnum::Star => {
                        self.advance();
                        let rhs = self.unary()?;
                        let span = CodeSpan::new(lhs.code_span().start(), rhs.code_span().end());
                        lhs = binary_expression!(Multiply, lhs, rhs, span);
                        continue;
                    }
                    SimpleTokenEnum::Slash => {
                        self.advance();
                        let rhs = self.unary()?;
                        let span = CodeSpan::new(lhs.code_span().start(), rhs.code_span().end());
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

    fn unary(&mut self) -> Result<BoxedExpression, ParseError> {
        if let Some(t) = self.peek() {
            if let Token::Simple(st) = t {
                match st.variant() {
                    SimpleTokenEnum::Bang  => {
                        self.advance();
                        let rhs = self.unary()?;
                        let span = CodeSpan::new(t.code_span().start(), rhs.code_span().end());
                        return Ok(unary_expression!(Not, rhs, span));
                    }
                    SimpleTokenEnum::Minus  => {
                        self.advance();
                        let rhs = self.unary()?;
                        let span = CodeSpan::new(t.code_span().start(), rhs.code_span().end());
                        return Ok(unary_expression!(Negative, rhs, span));
                    }
                    _ => { }
                }
            }

            return self.call();
        }
        else {
            return Err(self.unexpected_end_error());
        }
    }

    fn call(&mut self) -> Result<BoxedExpression, ParseError> {
        let mut e = self.primary()?;
        loop {
            if self.peek_simple(SimpleTokenEnum::LeftParen).is_some() {
                self.advance();
                let mut arguments = Vec::<BoxedExpression>::new();

                if let Some(rp) = self.peek_simple(SimpleTokenEnum::RightParen) {
                    self.advance();
                    let span = CodeSpan::new(e.code_span().start(), rp.code_span().end());
                    e = call_expression!(e, arguments, span);
                    continue;
                }
                else {
                    loop {
                        arguments.push(self.expression()?);
                        if self.peek_simple(SimpleTokenEnum::Comma).is_some() {
                            self.advance();
                            continue;
                        }
                        else {
                            let rp = self.consume(SimpleTokenEnum::RightParen)?;
                            let span = CodeSpan::new(e.code_span().start(), rp.code_span().end());
                            e = call_expression!(e, arguments, span);
                            break;
                        }
                    }
                    continue;
                }
            }
            else {
                break;
            }
        }
        return Ok(e);
    }

    fn primary(&mut self) -> Result<BoxedExpression, ParseError> {
        if let Some(t) = self.peek() {
            match t {
                Token::Number(nt) => {
                    self.advance();
                    Ok(literal_expression!(Number, nt.clone(), nt.code_span()))
                }
                Token::String(st) => {
                    self.advance();
                    Ok(literal_expression!(String, st.clone(), st.code_span()))
                }
                Token::Identifier(it) => {
                    self.advance();
                    Ok(variable_expression!(it.name(), it.code_span()))
                }
                Token::Simple(st) => {
                    match st.variant() {
                        SimpleTokenEnum::True => {
                            self.advance();
                            Ok(literal_expression!(True, st.code_span()))
                        }
                        SimpleTokenEnum::False => {
                            self.advance();
                            Ok(literal_expression!(False, st.code_span()))
                        }
                        SimpleTokenEnum::Nil => {
                            self.advance();
                            Ok(literal_expression!(Nil, st.code_span()))
                        }
                        SimpleTokenEnum::LeftParen => {
                            self.advance();
                            let e = self.expression()?;
                            Ok(grouping_expression!(
                                e,
                                CodeSpan::new(
                                    st.code_span().start(),
                                    self.consume(SimpleTokenEnum::RightParen)?.code_span().end(),
                                )
                            ))
                        }
                        SimpleTokenEnum::Eof => {
                            Err(self.unexpected_end_error())
                        }
                        _ => {
                            Err(ParseError::UnexpectedToken(t.code_span()))
                        }
                    }
                }
            }
        }
        else {
            Err(self.unexpected_end_error())
        }
    }

    pub fn statement(&mut self) -> Result<BoxedStatement, ParseError> {
        if let Some(t) = self.peek() {
            if let Token::Simple(st) = t {
                match st.variant() {
                    SimpleTokenEnum::Var => {
                        self.advance();
                        self.var_declare_statement()
                    }
                    SimpleTokenEnum::Fun => {
                        self.advance();
                        self.fun_declare_statement()
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
                    SimpleTokenEnum::Return => {
                        self.advance();
                        self.return_statement()
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

    fn not_declaration_statement(&mut self) -> Result<BoxedStatement, ParseError> {
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
                    SimpleTokenEnum::Return => {
                        self.advance();
                        self.return_statement()
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

    fn var_declare_statement(&mut self) -> Result<BoxedStatement, ParseError> {
        let cp_start = self.peek_last().code_span().start();

        let name = self.consume_identifier()?.name();

        let mut initializer = None;
        if self.consume(SimpleTokenEnum::Equal).is_ok() {
            initializer = Some(self.expression()?);
        }

        self.consume(SimpleTokenEnum::Semicolon)?;

        let cp_end = self.peek_last().code_span().end();

        return Ok(var_declare_statement!(
            name,
            initializer,
            CodeSpan::new(cp_start, cp_end)
        ));
    }

    fn fun_declare_statement(&mut self) -> Result<BoxedStatement, ParseError> {
        let cp_start = self.peek_last().code_span().start();

        let name = self.consume_identifier()?;

        self.consume(SimpleTokenEnum::LeftParen)?;
        let mut parameters = Vec::new();
        if self.peek_simple(SimpleTokenEnum::RightParen).is_none() {
            let mut used = HashSet::new();
            loop {
                let p = self.consume_identifier()?;
                if used.get(p.name()).is_some() {
                    return Err(ParseError::DuplicatedFunctionParameter(p.code_span()));
                }
                used.insert(p.name());
                parameters.push(p.clone());
                if self.consume(SimpleTokenEnum::Comma).is_ok() {
                    continue;
                }
                else {
                    break;
                }
            }
        }
        self.consume(SimpleTokenEnum::RightParen)?;

        self.consume(SimpleTokenEnum::LeftBrace)?;
        let mut body = Vec::new();
        loop {
            if self.consume(SimpleTokenEnum::RightBrace).is_ok() {
                break;
            }
            else {
                body.push(self.statement()?);
            }
        }

        let cp_end = self.peek_last().code_span().end();

        return Ok(fun_declare_statement!(
            name.clone(),
            parameters,
            body,
            CodeSpan::new(cp_start, cp_end)
        ));
    }

    fn ifelse(&mut self) -> Result<BoxedStatement, ParseError> {
        let cp_start = self.peek_last().code_span().start();
        self.consume(SimpleTokenEnum::LeftParen)?;
        let condition = self.expression()?;
        self.consume(SimpleTokenEnum::RightParen)?;
        let then_stmt = self.not_declaration_statement()?;
        let mut else_stmt = None;
        if self.consume(SimpleTokenEnum::Else).is_ok() {
            else_stmt = Some(self.not_declaration_statement()?);
        }
        let cp_end = self.peek_last().code_span().end();
        if let Some(else_stmt) = else_stmt {
            Ok(if_statement!(
                condition,
                then_stmt,
                else_stmt,
                CodeSpan::new(cp_start, cp_end)
            ))
        }
        else {
            Ok(if_statement!(
                condition,
                then_stmt,
                CodeSpan::new(cp_start, cp_end)
            ))
        }
    }

    fn r#while(&mut self) -> Result<BoxedStatement, ParseError> {
        let cp_start = self.peek_last().code_span().start();

        self.consume(SimpleTokenEnum::LeftParen)?;

        let condition = self.expression()?;

        self.consume(SimpleTokenEnum::RightParen)?;

        let body = self.not_declaration_statement()?;

        let cp_end = self.peek_last().code_span().end();

        Ok(while_statement!(
            condition,
            body,
            CodeSpan::new(cp_start, cp_end)
        ))
    }

    fn r#for(&mut self) -> Result<BoxedStatement, ParseError> {
        let cp_start = self.peek_last().code_span().start();

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

        let cp_end = self.peek_last().code_span().end();

        return Ok(for_statement!(
            initializer,
            condition,
            increment,
            body,
            CodeSpan::new(cp_start, cp_end),
        ));
    }

    fn block(&mut self) -> Result<BoxedStatement, ParseError> {
        let cp_start = self.peek_last().code_span().start();
        let mut stmts = Vec::new();
        loop {
            if let Ok(t) = self.consume(SimpleTokenEnum::RightBrace) {
                return Ok(block_statement!(stmts, CodeSpan::new(cp_start, t.code_span().end())));
            }
            else if self.is_end() {
                return Err(self.expect_token_not_found_error(SimpleTokenEnum::RightBrace.lexeme()));
            }
            else {
                stmts.push(self.statement()?);
            }
        }
    }

    fn print_statement(&mut self) -> Result<BoxedStatement, ParseError> {
        let cp_start = self.peek_last().code_span().start();
        let value = self.expression()?;
        self.consume(SimpleTokenEnum::Semicolon)?;
        let cp_end = self.peek_last().code_span().end();
        Ok(print_statement!(
            value,
            CodeSpan::new(cp_start, cp_end)
        ))
    }

    fn return_statement(&mut self) -> Result<BoxedStatement, ParseError> {
        let cp_start = self.peek_last().code_span().start();
        let mut e = Option::None;
        if self.peek_simple(SimpleTokenEnum::Semicolon).is_none() {
            e = Option::Some(self.expression()?);
        }
        self.consume(SimpleTokenEnum::Semicolon)?;
        let cp_end = self.peek_last().code_span().end();
        Ok(return_statement!(
            e,
            CodeSpan::new(cp_start, cp_end)
        ))
    }

    fn expression_statement(&mut self) -> Result<BoxedStatement, ParseError> {
        let expr = self.expression()?;
        let cp_start = expr.code_span().start();
        self.consume(SimpleTokenEnum::Semicolon)?;
        let cp_end = self.peek_last().code_span().end();
        Ok(expression_statement!(
            expr,
            CodeSpan::new(cp_start, cp_end)
        ))
    }

    fn expect_token_not_found_error(&self, token: &str) -> ParseError {
        if self.current == 0 {
            ParseError::ExpectTokenNotFound(token.to_string(), CodePoint::new(0, 0))
        }
        else {
            ParseError::ExpectTokenNotFound(token.to_string(), self.peek_last().code_span().end())
        }
    }

    fn unexpected_end_error(&self) -> ParseError {
        if self.current == 0 {
            ParseError::UnexpectedEnd(CodePoint::new(0, 0))
        }
        else {
            ParseError::UnexpectedEnd(self.peek_last().code_span().end())
        }
    }

    pub fn parse(tokens: &Vec<Token>) -> ParserOutput {
        let mut p = Parser::new(tokens);
        let mut stmts: Vec<BoxedStatement> = Vec::new();
        let mut errors: Vec<ParseError> = Vec::new();

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
    use crate::code::code_point::CodePoint;
    use crate::code::code_span::CodeSpan;
    use crate::scan::token::Token;
    use crate::scan::Scan;
    use super::{
        Parser,
        ParseError,
    };

    #[test]
    fn test_assignment() {
        let tests: Vec<(&str, &str)> = vec![
            ("foo = true", "(= foo true)"),
            ("foo = bar = true", "(= foo (= bar true))"),
            ("foo = x or y", "(= foo (or x y))"),
        ];
        for (src, ast) in tests {
            let ts = src.scan().0;
            let mut p = Parser::new(&ts);
            assert_eq!(p.assignment().unwrap().print(), ast);
        }
    }

    #[test]
    fn test_logicala_or() {
        let tests: Vec<(&str, &str)> = vec![
            ("1 or 2", "(or 1 2)"),
            ("1 or 2 or 3", "(or (or 1 2) 3)"),
            ("1 or 2 and 3", "(or 1 (and 2 3))"),
            ("1 and 2 or 3", "(or (and 1 2) 3)"),
        ];
        for (src, ast) in tests {
            let ts = src.scan().0;
            let mut p = Parser::new(&ts);
            assert_eq!(p.logical_or().unwrap().print(), ast);
        }
    }

    #[test]
    fn test_logicala_and() {
        let tests: Vec<(&str, &str)> = vec![
            ("1 and 2", "(and 1 2)"),
            ("1 and 2 and 3", "(and (and 1 2) 3)"),
            ("1 == 2 and 3", "(and (== 1 2) 3)"),
            ("1 and 2 == 3", "(and 1 (== 2 3))"),
        ];
        for (src, ast) in tests {
            let ts = src.scan().0;
            let mut p = Parser::new(&ts);
            assert_eq!(p.logical_and().unwrap().print(), ast);
        }
    }

    #[test]
    fn test_equality() {
        let tests: Vec<(&str, &str)> = vec![
            ("1 == 2", "(== 1 2)"),
            ("1 != 2", "(!= 1 2)"),
            ("1 == 2 != 3", "(!= (== 1 2) 3)"),
            ("1 == 2 > 3", "(== 1 (> 2 3))"),
            ("1 < 2 != 3", "(!= (< 1 2) 3)"),
        ];
        for (src, ast) in tests {
            let ts = src.scan().0;
            let mut p = Parser::new(&ts);
            assert_eq!(p.equality().unwrap().print(), ast);
        }
    }

    #[test]
    fn test_comparison() {
        let tests: Vec<(&str, &str)> = vec![
            ("1 < 2", "(< 1 2)"),
            ("1 <= 2", "(<= 1 2)"),
            ("1 > 2", "(> 1 2)"),
            ("1 >= 2", "(>= 1 2)"),
            ("1 > 2 > 3", "(> (> 1 2) 3)"),
            ("1 + 2 > 3", "(> (+ 1 2) 3)"),
            ("1 > 2 + 3", "(> 1 (+ 2 3))"),
        ];
        for (src, ast) in tests {
            let ts = src.scan().0;
            let mut p = Parser::new(&ts);
            assert_eq!(p.comparison().unwrap().print(), ast);
        }
    }

    #[test]
    fn test_term() {
        let tests: Vec<(&str, &str)> = vec![
            ("1 + 2", "(+ 1 2)"),
            ("1 + 2 + 3", "(+ (+ 1 2) 3)"),
            ("1 - 2", "(- 1 2)"),
            ("1 - 2 - 3", "(- (- 1 2) 3)"),
            ("1 + 2 * 3", "(+ 1 (* 2 3))"),
            ("1 * 2 - 3", "(- (* 1 2) 3)"),
        ];
        for (src, ast) in tests {
            let ts = src.scan().0;
            let mut p = Parser::new(&ts);
            assert_eq!(p.term().unwrap().print(), ast);
        }
    }

    #[test]
    fn test_factor() {
        let tests: Vec<(&str, &str)> = vec![
            ("1 * 2", "(* 1 2)"),
            ("1 / 2", "(/ 1 2)"),
            ("1 * 2 / 3", "(/ (* 1 2) 3)"),
            ("1 * -2", "(* 1 (- 2))"),
            ("-2 * 3", "(* (- 2) 3)"),
        ];
        for (src, ast) in tests {
            let ts = src.scan().0;
            let mut p = Parser::new(&ts);
            assert_eq!(p.factor().unwrap().print(), ast);
        }
    }

    #[test]
    fn test_unary() {
        let tests: Vec<(&str, &str)> = vec![
            ("!1", "(! 1)"),
            ("!!1", "(! (! 1))"),
            ("-1", "(- 1)"),
            ("-!1", "(- (! 1))"),
        ];
        for (src, ast) in tests {
            let ts = src.scan().0;
            let mut p = Parser::new(&ts);
            assert_eq!(p.unary().unwrap().print(), ast);
        }
    }

    #[test]
    fn test_unary_unexpected_end_error() {
        let ts = "".scan().0;
        let mut p = Parser::new(&ts);
        assert_eq!(
            p.unary().unwrap_err(),
            ParseError::UnexpectedEnd(
                CodePoint::new(0, 0),
            )
        );
    }

    #[test]
    fn test_call() {
        let tests: Vec<(&str, &str)> = vec![
            ("hello()", "(call hello )"),
            ("hello()()", "(call (call hello ) )"),
            ("hello(name, \"123\")", "(call hello name \"123\")"),
            ("hello(name)(\"123\")", "(call (call hello name) \"123\")"),
        ];
        for (src, ast) in tests {
            let ts = src.scan().0;
            let mut p = Parser::new(&ts);
            assert_eq!(p.call().unwrap().print(), ast);
        }
    }

    #[test]
    fn test_call_unclosed_parentheses_error() {
        let tests: Vec<(&str, ParseError)> = vec![
            (
                "hello(",
                ParseError::UnexpectedEnd(
                    CodePoint::new(0, 6),
                )
            ),
            (
                "hello(}",
                ParseError::UnexpectedToken(
                    CodeSpan::new(
                        CodePoint::new(0, 6),
                        CodePoint::new(0, 7),
                    ),
                )
            ),
            (
                "hello(123",
                ParseError::ExpectTokenNotFound(
                    ")".to_owned(),
                    CodePoint::new(0, 9),
                )
            ),
            (
                "hello(123}",
                ParseError::ExpectTokenMismatch(
                    ")".to_owned(),
                    CodeSpan::new(
                        CodePoint::new(0, 9),
                        CodePoint::new(0, 10),
                    ),
                )
            )
        ];
        for (src, err) in tests {
            let ts = src.scan().0;
            let mut p = Parser::new(&ts);
            assert_eq!(p.call().unwrap_err(), err);
        }
    }

    #[test]
    fn test_primary() {
        let tests: Vec<(&str, &str)> = vec![
            // number.
            ("1.23", "1.23"),
            // string.
            ("\"hello\"", "\"hello\""),
            // identifier.
            ("hello", "hello"),
            // simple true.
            ("true", "true"),
            // simple false.
            ("false", "false"),
            // simple nil.
            ("nil", "nil"),
            // simple group.
            ("(1 + 1)", "(group (+ 1 1))"),
        ];
        for (src, ast) in tests {
            let ts = src.scan().0;
            let mut p = Parser::new(&ts);
            assert_eq!(p.primary().unwrap().print(), ast);
        }
    }

    #[test]
    fn test_primary_unclosed_parentheses_error() {
        let tests: Vec<(&str, ParseError)> = vec![
            (
                "(1 + 1",
                ParseError::ExpectTokenNotFound(
                    ")".to_owned(),
                    CodePoint::new(0, 6),
                )
            ),
            (
                "(1 + 2}",
                ParseError::ExpectTokenMismatch(
                    ")".to_owned(),
                    CodeSpan::new(
                        CodePoint::new(0, 6),
                        CodePoint::new(0, 7),
                    ),
                )
            )
        ];
        for (src, err) in tests {
            let ts = src.scan().0;
            let mut p = Parser::new(&ts);
            assert_eq!(p.primary().unwrap_err(), err);
        }
    }

    #[test]
    fn test_primary_unexpected_end_error() {
        let ts = "".scan().0;
        let mut p = Parser::new(&ts);
        assert_eq!(
            p.primary().unwrap_err(),
            ParseError::UnexpectedEnd(CodePoint::new(0, 0))
        );
        let ts: Vec<Token> = vec![];
        let mut p = Parser::new(&ts);
        assert_eq!(
            p.primary().unwrap_err(),
            ParseError::UnexpectedEnd(CodePoint::new(0, 0))
        );
    }

    #[test]
    fn test_primary_unexpected_token_error() {
        let ts = "!".scan().0;
        let mut p = Parser::new(&ts);
        assert_eq!(
            p.primary().unwrap_err(),
            ParseError::UnexpectedToken(
                CodeSpan::new(
                    CodePoint::new(0, 0),
                    CodePoint::new(0, 1),
                )
            )
        );
    }

    #[test]
    fn test_var_declare_statement() {
        let tests: Vec<(&str, &str)> = vec![
            ("var foo;", "var foo;"),
            ("var foo = true;", "var foo = true;"),
        ];

        for (src, ast) in tests {
            let ts = src.scan().0;
            let mut p = Parser::new(&ts);
            assert_eq!(p.statement().unwrap().print(), ast);
        }
    }

    #[test]
    fn test_var_declare_statement_expect_identifier_error() {
        let ts = "var true;".scan().0;
        let mut p = Parser::new(&ts);
        assert_eq!(
            p.statement().unwrap_err(),
            ParseError::ExpectIdentifier(
                CodeSpan::new(
                    CodePoint::new(0, 4),
                    CodePoint::new(0, 8),
                )
            ),
        );
    }

    #[test]
    fn test_var_declare_statement_missing_semicolon_error() {
        let tests: Vec<(&str, ParseError)> = vec![
            (
                "var foo",
                ParseError::ExpectTokenNotFound(
                    ";".to_string(),
                    CodePoint::new(0, 7),
                )
            ),
            (
                "var foo = true",
                ParseError::ExpectTokenNotFound(
                    ";".to_string(),
                    CodePoint::new(0, 14),
                )
            ),
        ];

        for (src, err) in tests {
            let ts = src.scan().0;
            let mut p = Parser::new(&ts);
            assert_eq!(p.statement().unwrap_err(), err);
        }
    }

    #[test]
    fn test_var_declare_statement_unexpect_end_error() {
        let ts = "var foo =".scan().0;
        let mut p = Parser::new(&ts);
        assert_eq!(
            p.statement().unwrap_err(),
            ParseError::UnexpectedEnd(
                CodePoint::new(0, 9),
            )
        );
    }

    #[test]
    fn test_fun_declare_statement() {
        let tests: Vec<(&str, &str)> = vec![
            (
                "fun foo() {}",
                "fun foo() {}"
            ),
            (
                "fun bar(a) {}",
                "fun bar(a) {}"
            ),
            (
                "
                fun hello() {
                    print \"hello\";
                }
                ",
                "fun hello() {print \"hello\";}",
            ),
            (
                "
                fun add(a, b) {
                    var c = a + b;
                    print c;
                }
                ",
                "fun add(a, b) {var c = (+ a b); print c;}",
            )
        ];

        for (src, ast) in tests {
            let ts = src.scan().0;
            let mut p = Parser::new(&ts);
            assert_eq!(p.statement().unwrap().print(), ast);
        }
    }

    #[test]
    fn test_fun_declare_statement_expect_identifier_error() {
        let ts = "fun ()".scan().0;
        let mut p = Parser::new(&ts);
        assert_eq!(
            p.statement().unwrap_err(),
            ParseError::ExpectIdentifier(
                CodeSpan::new(
                    CodePoint::new(0, 4),
                    CodePoint::new(0, 5),
                )
            ),
        );
    }

    #[test]
    fn test_fun_declare_statement_missing_left_paren_error() {
        let ts = "fun foo".scan().0;
        let mut p = Parser::new(&ts);
        assert_eq!(
            p.statement().unwrap_err(),
            ParseError::ExpectTokenNotFound(
                "(".to_owned(),
                CodePoint::new(0, 7),
            )
        );
    }

    #[test]
    fn test_fun_declare_statement_parameter_not_identifier_error() {
        let ts = "fun foo(fun)".scan().0;
        let mut p = Parser::new(&ts);
        assert_eq!(
            p.statement().unwrap_err(),
            ParseError::ExpectIdentifier(
                CodeSpan::new(
                    CodePoint::new(0, 8),
                    CodePoint::new(0, 11),
                )
            )
        );
    }

    #[test]
    fn test_fun_declare_statement_duplicated_parameter_error() {
        let ts = "fun foo(x, x)".scan().0;
        let mut p = Parser::new(&ts);
        assert_eq!(
            p.statement().unwrap_err(),
            ParseError::DuplicatedFunctionParameter(
                CodeSpan::new(
                    CodePoint::new(0, 11),
                    CodePoint::new(0, 12),
                )
            )
        );
    }

    #[test]
    fn test_fun_declare_statement_missing_right_paren_error() {
        let ts = "fun foo(x".scan().0;
        let mut p = Parser::new(&ts);
        assert_eq!(
            p.statement().unwrap_err(),
            ParseError::ExpectTokenNotFound(
                ")".to_owned(),
                CodePoint::new(0, 9),
            )
        );
    }

    #[test]
    fn test_fun_declare_statement_missing_left_brace_error() {
        let ts = "fun foo()".scan().0;
        let mut p = Parser::new(&ts);
        assert_eq!(
            p.statement().unwrap_err(),
            ParseError::ExpectTokenNotFound(
                "{".to_owned(),
                CodePoint::new(0, 9),
            )
        );
    }

    #[test]
    fn test_fun_declare_statement_statement_error() {
        let ts = "fun foo() { print \"hello\" }".scan().0;
        let mut p = Parser::new(&ts);
        assert_eq!(
            p.statement().unwrap_err(),
            ParseError::ExpectTokenMismatch(
                ";".to_owned(),
                CodeSpan::new(
                    CodePoint::new(0, 26),
                    CodePoint::new(0, 27),
                )
            )
        );
    }

    #[test]
    fn test_fun_declare_statement_missing_right_brace_error() {
        let ts = "fun foo() {".scan().0;
        let mut p = Parser::new(&ts);
        assert_eq!(
            p.statement().unwrap_err(),
            ParseError::UnexpectedEnd(
                CodePoint::new(0, 11),
            )
        );
    }

    #[test]
    fn test_ifelse_statement() {
        let tests: Vec<(&str, &str)> = vec![
            (
                "if (true) print \"hello\";",
                "if true print \"hello\";",
            ),
            (
                "if (foo) print \"hello\"; else print \"world\";",
                "if foo print \"hello\"; else print \"world\";",
            ),
        ];

        for (src, expect) in tests {
            let ts = src.scan().0;
            let mut p = Parser::new(&ts);
            assert_eq!(p.statement().unwrap().print(), expect);
        }
    }

    #[test]
    fn test_ifelse_statement_missing_left_paren_error() {
        let ts = "if true) print 1;".scan().0;
        let mut p = Parser::new(&ts);
        assert_eq!(
            p.statement().unwrap_err(),
            ParseError::ExpectTokenMismatch(
                "(".to_string(),
                CodeSpan::new(
                    CodePoint::new(0, 3),
                    CodePoint::new(0, 7),
                )
            )
        );
    }

    #[test]
    fn test_ifelse_statement_missing_right_paren_error() {
        let ts = "if (true print 1;".scan().0;
        let mut p = Parser::new(&ts);
        assert_eq!(
            p.statement().unwrap_err(),
            ParseError::ExpectTokenMismatch(
                ")".to_string(),
                CodeSpan::new(
                    CodePoint::new(0, 9),
                    CodePoint::new(0, 14),
                )
            )
        );
    }

    #[test]
    fn test_while_statement() {
        let ts = "while (foo) print 1;".scan().0;
        let mut p = Parser::new(&ts);
        assert_eq!(
            p.statement().unwrap().print(),
            "while foo print 1;"
        );
    }

    #[test]
    fn test_while_statement_missing_left_paren_error() {
        let ts = "while foo) print 1;".scan().0;
        let mut p = Parser::new(&ts);
        assert_eq!(
            p.statement().unwrap_err(),
            ParseError::ExpectTokenMismatch(
                "(".to_string(),
                CodeSpan::new(
                    CodePoint::new(0, 6),
                    CodePoint::new(0, 9),
                )
            )
        );
    }

    #[test]
    fn test_while_statement_missing_right_paren_error() {
        let ts = "while (foo print 1;".scan().0;
        let mut p = Parser::new(&ts);
        assert_eq!(
            p.statement().unwrap_err(),
            ParseError::ExpectTokenMismatch(
                ")".to_string(),
                CodeSpan::new(
                    CodePoint::new(0, 11),
                    CodePoint::new(0, 16),
                )
            )
        );
    }

    #[test]
    fn test_for_statement() {
        let tests: Vec<(&str, &str)> = vec![
            (
                "for (;;) print 1;",
                "for (;;) print 1;",
            ),
            (
                "for (var i = 0; i < 10; i = i + 1) print 1;",
                "for (var i = 0; (< i 10); (= i (+ i 1))) print 1;",
            ),
        ];
        for (src, expect) in tests {
            let tokens = src.scan().0;
            let mut parser = Parser::new(&tokens);
            assert_eq!(parser.statement().unwrap().print(), expect);
        }
    }

    #[test]
    fn test_for_statement_missing_left_paren_error() {
        let tokens = "for var i = 0; i < 10: i = i + 1) print i;".scan().0;
        let mut parser = Parser::new(&tokens);
        assert_eq!(
            parser.statement().unwrap_err(),
            ParseError::ExpectTokenMismatch(
                "(".to_string(),
                CodeSpan::new(
                    CodePoint::new(0, 4),
                    CodePoint::new(0, 7),
                )
            )
        );
    }

    #[test]
    fn test_for_statement_initializer_var_declare_statement_error() {
        let tokens = "for (var i = 0)".scan().0;
        let mut parser = Parser::new(&tokens);
        assert_eq!(
            parser.statement().unwrap_err(),
            ParseError::ExpectTokenMismatch(
                ";".to_string(),
                CodeSpan::new(
                    CodePoint::new(0, 14),
                    CodePoint::new(0, 15),
                )
            )
        );
    }

    #[test]
    fn test_for_statement_initializer_expression_statement_error() {
        let tokens = "for (i = 0)".scan().0;
        let mut parser = Parser::new(&tokens);
        assert_eq!(
            parser.statement().unwrap_err(),
            ParseError::ExpectTokenMismatch(
                ";".to_string(),
                CodeSpan::new(
                    CodePoint::new(0, 10),
                    CodePoint::new(0, 11),
                )
            )
        );
    }

    #[test]
    fn test_for_statement_condition_expression_error() {
        let tokens = "for (i = 0; =;)".scan().0;
        let mut parser = Parser::new(&tokens);
        assert_eq!(
            parser.statement().unwrap_err(),
            ParseError::UnexpectedToken(
                CodeSpan::new(
                    CodePoint::new(0, 12),
                    CodePoint::new(0, 13),
                )
            )
        );
    }

    #[test]
    fn test_for_statement_condition_missing_semicolon_error() {
        let tokens = "for (i = 0; i < 10)".scan().0;
        let mut parser = Parser::new(&tokens);
        assert_eq!(
            parser.statement().unwrap_err(),
            ParseError::ExpectTokenMismatch(
                ";".to_string(),
                CodeSpan::new(
                    CodePoint::new(0, 18),
                    CodePoint::new(0, 19),
                )
            )
        );
    }

    #[test]
    fn test_for_statement_increment_expression_error() {
        let tokens = "for (i = 0; i < 10; =)".scan().0;
        let mut parser = Parser::new(&tokens);
        assert_eq!(
            parser.statement().unwrap_err(),
            ParseError::UnexpectedToken(
                CodeSpan::new(
                    CodePoint::new(0, 20),
                    CodePoint::new(0, 21),
                )
            )
        );
    }

    #[test]
    fn test_for_statement_missing_right_paren_error() {
        let tokens = "for (i = 0; i < 10; i = i + 1".scan().0;
        let mut parser = Parser::new(&tokens);
        assert_eq!(
            parser.statement().unwrap_err(),
            ParseError::ExpectTokenNotFound(
                ")".to_string(),
                CodePoint::new(0, 29),
            )
        );
    }

    #[test]
    fn test_for_statement_body_statement_error() {
        let tokens = "for (i = 0; i < 10; i = i + 1)".scan().0;
        let mut parser = Parser::new(&tokens);
        assert_eq!(
            parser.statement().unwrap_err(),
            ParseError::UnexpectedEnd(
                CodePoint::new(0, 30)
            )
        );
    }

    #[test]
    fn test_block_statement() {
        let tokens = "{var foo = true; foo = false;}".scan().0;
        let mut parser = Parser::new(&tokens);
        assert_eq!(
            parser.statement().unwrap().print(),
            "{var foo = true; (= foo false);}"
        );
    }

    #[test]
    fn test_block_statement_statement_error() {
        let tokens = "{var foo}".scan().0;
        let mut parser = Parser::new(&tokens);
        assert_eq!(
            parser.statement().unwrap_err(),
            ParseError::ExpectTokenMismatch(
                ";".to_string(),
                CodeSpan::new(
                    CodePoint::new(0, 8),
                    CodePoint::new(0, 9),
                )
            )
        );
    }

    #[test]
    fn test_block_statement_missing_closing_brace_error() {
        let tokens = "{var foo;".scan().0;
        let mut parser = Parser::new(&tokens);
        assert_eq!(
            parser.statement().unwrap_err(),
            ParseError::ExpectTokenNotFound(
                "}".to_string(),
                CodePoint::new(0, 9),
            )
        );
    }

    #[test]
    fn test_print_statement() {
        let tokens = "print \"hello\";".scan().0;
        let mut parser = Parser::new(&tokens);
        assert_eq!(
            parser.statement().unwrap().print(),
            "print \"hello\";",
        );
    }

    #[test]
    fn test_print_statement_expression_error() {
        let tokens = "print ;".scan().0;
        let mut parser = Parser::new(&tokens);
        assert_eq!(
            parser.statement().unwrap_err(),
            ParseError::UnexpectedToken(
                CodeSpan::new(
                    CodePoint::new(0, 6),
                    CodePoint::new(0, 7),
                )
            )
        );
    }

    #[test]
    fn test_print_statement_missing_semicolon_error() {
        let tokens = "print foo".scan().0;
        let mut parser = Parser::new(&tokens);
        assert_eq!(
            parser.statement().unwrap_err(),
            ParseError::ExpectTokenNotFound(
                ";".to_string(),
                CodePoint::new(0, 9),
            )
        );
    }

    #[test]
    fn test_return_statement() {
        let tests: Vec<(&str, &str)> = vec![
            (
                "return;",
                "return;"
            ),
            (
                "return true;",
                "return true;"
            ),
            (
                "return 1 + 1;",
                "return (+ 1 1);"
            ),
            (
                "return foo(a, b);",
                "return (call foo a b);"
            ),
        ];
        for (src, expect) in tests {
            let tokens = src.scan().0;
            let mut parser = Parser::new(&tokens);
            assert_eq!(parser.statement().unwrap().print(), expect);
        }
    }

    #[test]
    fn test_return_statement_missing_semicolon_error() {
        let tests: Vec<(&str, ParseError)> = vec![
            (
                "return",
                ParseError::UnexpectedEnd(
                    CodePoint::new(0, 6),
                ),
            ),
            (
                "return true",
                ParseError::ExpectTokenNotFound(
                    ";".to_owned(),
                    CodePoint::new(0, 11),
                ),
            ),
        ];
        for (src, error) in tests {
            let tokens = src.scan().0;
            let mut parser = Parser::new(&tokens);
            assert_eq!(parser.statement().unwrap_err(), error);
        }
    }

    #[test]
    fn test_expression_statement() {
        let tokens = "true;".scan().0;
        let mut parser = Parser::new(&tokens);
        assert_eq!(
            parser.statement().unwrap().print(),
            "true;",
        );
    }

    #[test]
    fn test_expression_statement_expression_error() {
        let tokens = "".scan().0;
        let mut parser = Parser::new(&tokens);
        assert_eq!(
            parser.statement().unwrap_err(),
            ParseError::UnexpectedEnd(
                CodePoint::new(0, 0)
            )
        );
    }

    #[test]
    fn test_expression_statement_missing_semicolon_error() {
        let tokens = "true".scan().0;
        let mut parser = Parser::new(&tokens);
        assert_eq!(
            parser.statement().unwrap_err(),
            ParseError::ExpectTokenNotFound(
                ";".to_string(),
                CodePoint::new(0, 4),
            )
        );
    }
}
