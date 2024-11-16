use std::{
    rc::Rc,
    collections::{
        HashSet,
        HashMap,
    },
};
use crate::{
    code::{
        Code,
        code_point::CodePoint,
        code_span::CodeSpan,
    },
    parse::{
        expression::{
            Expression,
            assign::AssignExpressionNotResolved,
            binary::{
                BinaryExpression,
                BinaryExpressionEnum,
            },
            call::CallExpression,
            get::GetExpression,
            grouping::GroupingExpression,
            literal::{
                LiteralExpression,
                LiteralExpressionEnum,
            },
            logical::{
                LogicalExpression,
                LogicalExpressionEnum,
            },
            set::SetExpression,
            unary::{
                UnaryExpression,
                UnaryExpressionEnum,
            },
            variable::VariableExpressionNotResolved,
        },
        statement::{
            Statement,
            block::BlockStatement,
            r#break::BreakStatement,
            class_declare::{
                MethodDefinition,
                ClassDeclareStatement,
            },
            expression::ExpressionStatement,
            r#for::ForStatement,
            fun_declare::FunDeclareStatement,
            ifelse::IfStatement,
            print::PrintStatement,
            r#return::ReturnStatement,
            var_declare::VarDeclareStatement,
            r#while::WhileStatement,
        }
    },
    scan::token::{
        Token,
        identifier::IdentifierToken,
        simple::{
            SimpleToken,
            SimpleTokenEnum,
        }
    },
    error::LoxError,
    assign_expression_not_resolved,
    binary_expression,
    call_expression,
    get_expression,
    grouping_expression,
    literal_expression,
    logical_expression,
    set_expression,
    unary_expression,
    variable_expression_not_resolved,
    block_statement,
    break_statement,
    class_declare_statement,
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
pub enum ParserError {
    UnexpectedEnd(CodePoint),
    UnexpectedToken(CodeSpan),
    ExpectTokenMismatch(String, CodeSpan),
    ExpectTokenNotFound(String, CodePoint),
    ExpectIdentifier(CodeSpan),
    DuplicatedFunctionParameter(CodeSpan),
    ContextNotSupportBreak(CodeSpan),
    DuplicatedMethodDefinition(CodeSpan),
}

impl LoxError for ParserError {
    fn print(&self, src_lines: &Vec<&str>) -> String {
        match self {
            ParserError::UnexpectedEnd(cp) => {
                let mut out = "ParserError: Unexpected end.\r\n".to_owned();
                out += cp.debug_string(&src_lines).as_ref();
                return out;
            }
            ParserError::UnexpectedToken(s) => {
                let mut out = format!("ParserError: Unexpected token: {}\r\n", s.code_string(&src_lines, 10));
                out += s.debug_string(&src_lines).as_ref();
                return out;
            }
            ParserError::ExpectTokenMismatch(et, s) => {
                let mut out = format!(
                    "ParserError: Expect token: {} but found: {}\r\n",
                    et,
                    s.code_string(&src_lines, 10)
                );
                out += s.debug_string(&src_lines).as_ref();
                return out;
            }
            ParserError::ExpectTokenNotFound(et, cp) => {
                let mut out = format!("ParserError: Expect token: {} but not found.\r\n", et);
                out += cp.debug_string(&src_lines).as_ref();
                return out;
            }
            ParserError::ExpectIdentifier(s) => {
                let mut out = "ParserError: Expect identifier.\r\n".to_owned();
                out += s.debug_string(&src_lines).as_ref();
                return out;
            }
            ParserError::DuplicatedFunctionParameter(s) => {
                let mut out = "ParserError: Duplicated function parameter.\r\n".to_owned();
                out += s.debug_string(&src_lines).as_ref();
                return out;
            }
            ParserError::ContextNotSupportBreak(s) => {
                let mut out = "ParserError: Break statement is not supported in this context.\r\n".to_owned();
                out += s.debug_string(&src_lines).as_ref();
                return out;
            }
            ParserError::DuplicatedMethodDefinition(s) => {
                let mut out = "ParserError: Duplicated method definition.\r\n".to_owned();
                out += s.debug_string(&src_lines).as_ref();
                return out;
            }
        }
    }
}

pub type ParserOutput = (Vec<Statement>, Vec<ParserError>);

pub struct Parser<'tokens> {
    tokens: &'tokens Vec<Rc<Token>>,
    current: usize,
}

impl<'tokens> Parser<'tokens> {
    pub fn new(tokens: &'tokens Vec<Rc<Token>>) -> Parser<'tokens> {
        Parser {
            tokens,
            current: 0,
        }
    }

    fn advance(&mut self) {
        self.current += 1;
    }

    fn _fallback(&mut self) {
        self.current -= 1;
    }

    fn is_end(&self) -> bool {
        if self.current >= self.tokens.len() {
            return true;
        }

        if let Token::Simple(st) = self.tokens[self.current].as_ref() {
            if st.variant() == SimpleTokenEnum::Eof {
                return true;
            }
        }

        return false;
    }

    fn peek(&self) -> Option<&'tokens Token> {
        self.tokens.get(self.current).map(|t| t.as_ref())
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

    fn consume(&mut self, variant: SimpleTokenEnum) -> Result<&'tokens Token, ParserError> {
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
                ParserError::ExpectTokenMismatch(
                    variant.lexeme().to_string(),
                    t.code_span(),
                )
            );
        }
        else {
            return Err(self.expect_token_not_found_error(variant.lexeme()))
        }
    }

    fn consume_identifier(&mut self) -> Result<&'tokens Rc<IdentifierToken>, ParserError> {
        if let Some(t) = self.peek() {
            match t {
                Token::Identifier(ref it) => {
                    self.advance();
                    Ok(it)
                }
                _ => Err(ParserError::ExpectIdentifier(t.code_span()))
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

    pub fn expression(&mut self) -> Result<Expression, ParserError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expression, ParserError> {
        let lhs = self.logical_or()?;
        if let Some(get_expr) = lhs.downcast_ref::<GetExpression>() {
            if self.consume(SimpleTokenEnum::Equal).is_ok() {
                let rhs = self.assignment()?;
                let span = CodeSpan::new(get_expr.code_span().start(), rhs.code_span().end());
                return Ok(
                    set_expression!(
                        get_expr.object().clone(),
                        get_expr.name().clone(),
                        rhs,
                        span
                    )
                );
            }
        }
        else if let Some(var_expr) = lhs.downcast_ref::<VariableExpressionNotResolved>() {
            if self.consume(SimpleTokenEnum::Equal).is_ok() {
                let rhs = self.assignment()?;
                let span = CodeSpan::new(var_expr.code_span().start(), rhs.code_span().end());
                return Ok(assign_expression_not_resolved!(var_expr.from().clone(), rhs, span));
            }
        }
        return Ok(lhs);
    }

    fn logical_or(&mut self) -> Result<Expression, ParserError> {
        let mut lhs = self.logical_and()?;
        while self.consume(SimpleTokenEnum::Or).is_ok() {
            let rhs = self.logical_and()?;
            let span = CodeSpan::new(lhs.code_span().start(), rhs.code_span().end());
            lhs = logical_expression!(Or, lhs, rhs, span);
        }
        Ok(lhs)
    }

    fn logical_and(&mut self) -> Result<Expression, ParserError> {
        let mut lhs = self.equality()?;
        while self.consume(SimpleTokenEnum::And).is_ok() {
            let rhs = self.equality()?;
            let span = CodeSpan::new(lhs.code_span().start(), rhs.code_span().end());
            lhs = logical_expression!(And, lhs, rhs, span);
        }
        Ok(lhs)
    }

    fn equality(&mut self) -> Result<Expression, ParserError> {
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

    fn comparison(&mut self) -> Result<Expression, ParserError> {
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

    fn term(&mut self) -> Result<Expression, ParserError> {
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

    fn factor(&mut self) -> Result<Expression, ParserError> {
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

    fn unary(&mut self) -> Result<Expression, ParserError> {
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

    fn call(&mut self) -> Result<Expression, ParserError> {
        let mut e = self.primary()?;
        loop {
            if self.peek_simple(SimpleTokenEnum::LeftParen).is_some() {
                self.advance();
                let mut arguments = Vec::<Expression>::new();

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
            else if self.peek_simple(SimpleTokenEnum::Dot).is_some() {
                self.advance();
                let ident = self.consume_identifier()?;
                let span = CodeSpan::new(e.code_span().start(), ident.code_span().end());
                e = get_expression!(e, ident.clone(), span);
                continue;
            }
            else {
                break;
            }
        }
        return Ok(e);
    }

    fn primary(&mut self) -> Result<Expression, ParserError> {
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
                    Ok(variable_expression_not_resolved!(it.clone()))
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
                            Err(ParserError::UnexpectedToken(t.code_span()))
                        }
                    }
                }
            }
        }
        else {
            Err(self.unexpected_end_error())
        }
    }

    fn method_definition(&mut self) -> Result<MethodDefinition, ParserError> {
        let name = self.consume_identifier()?;
        let cp_start = name.code_span().start();

        self.consume(SimpleTokenEnum::LeftParen)?;
        let mut parameters = Vec::new();
        if self.peek_simple(SimpleTokenEnum::RightParen).is_none() {
            let mut used = HashSet::new();
            used.insert("this");
            loop {
                let p = self.consume_identifier()?;
                if !used.insert(p.name()) {
                    return Err(ParserError::DuplicatedFunctionParameter(p.code_span()));
                }
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
                body.push(self.statement(false)?);
            }
        }

        let cp_end = self.peek_last().code_span().end();

        return Ok(
            MethodDefinition::new(
                name.clone(),
                parameters,
                body,
                CodeSpan::new(cp_start, cp_end),
            )
        );
    }

    pub fn statement(&mut self, can_break: bool) -> Result<Statement, ParserError> {
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
                    SimpleTokenEnum::Class => {
                        self.advance();
                        self.class_declare_statement()
                    }
                    SimpleTokenEnum::If => {
                        self.advance();
                        self.ifelse(can_break)
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
                        self.block(can_break)
                    }
                    SimpleTokenEnum::Print => {
                        self.advance();
                        self.print_statement()
                    }
                    SimpleTokenEnum::Return => {
                        self.advance();
                        self.return_statement()
                    }
                    SimpleTokenEnum::Break => {
                        if can_break {
                            self.advance();
                            self.break_statement()
                        }
                        else {
                            Err(ParserError::ContextNotSupportBreak(t.code_span()))
                        }
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

    fn not_declaration_statement(&mut self, can_break: bool) -> Result<Statement, ParserError> {
        if let Some(t) = self.peek() {
            if let Token::Simple(st) = t {
                match st.variant() {
                    SimpleTokenEnum::If => {
                        self.advance();
                        self.ifelse(can_break)
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
                        self.block(can_break)
                    }
                    SimpleTokenEnum::Print => {
                        self.advance();
                        self.print_statement()
                    }
                    SimpleTokenEnum::Return => {
                        self.advance();
                        self.return_statement()
                    }
                    SimpleTokenEnum::Break => {
                        if can_break {
                            self.advance();
                            self.break_statement()
                        }
                        else {
                            Err(ParserError::ContextNotSupportBreak(t.code_span()))
                        }
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

    fn var_declare_statement(&mut self) -> Result<Statement, ParserError> {
        let cp_start = self.peek_last().code_span().start();

        let identifier = self.consume_identifier()?;

        let mut initializer = None;
        if self.consume(SimpleTokenEnum::Equal).is_ok() {
            initializer = Some(self.expression()?);
        }

        self.consume(SimpleTokenEnum::Semicolon)?;

        let cp_end = self.peek_last().code_span().end();

        return Ok(
            var_declare_statement!(
                identifier.clone(),
                initializer,
                CodeSpan::new(cp_start, cp_end)
            )
        );
    }

    fn fun_declare_statement(&mut self) -> Result<Statement, ParserError> {
        let cp_start = self.peek_last().code_span().start();

        let name = self.consume_identifier()?;

        self.consume(SimpleTokenEnum::LeftParen)?;
        let mut parameters = Vec::new();
        if self.peek_simple(SimpleTokenEnum::RightParen).is_none() {
            let mut used = HashSet::new();
            loop {
                let p = self.consume_identifier()?;
                if used.get(p.name()).is_some() {
                    return Err(ParserError::DuplicatedFunctionParameter(p.code_span()));
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
                body.push(self.statement(false)?);
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

    fn class_declare_statement(&mut self) -> Result<Statement, ParserError> {
        let cp_start = self.peek_last().code_span().start();

        let name = self.consume_identifier()?;

        self.consume(SimpleTokenEnum::LeftBrace)?;
        let mut method_definitions = HashMap::new();
        loop {
            if self.peek_simple(SimpleTokenEnum::RightBrace).is_some() {
                break;
            }
            let method_def = self.method_definition()?;
            if method_definitions.contains_key(method_def.name().name()) {
                return Err(
                    ParserError::DuplicatedMethodDefinition(method_def.code_span())
                );
            }
            else {
                method_definitions.insert(
                    method_def.name().name().to_owned(),
                    Rc::new(method_def),
                );
            }
        }
        let cp_end = self.consume(SimpleTokenEnum::RightBrace)?.code_span().end();

        return Ok(class_declare_statement!(
            name.clone(),
            Rc::new(method_definitions),
            CodeSpan::new(cp_start, cp_end)
        ));
    }

    fn ifelse(&mut self, can_break: bool) -> Result<Statement, ParserError> {
        let cp_start = self.peek_last().code_span().start();
        self.consume(SimpleTokenEnum::LeftParen)?;
        let condition = self.expression()?;
        self.consume(SimpleTokenEnum::RightParen)?;
        let then_stmt = self.not_declaration_statement(can_break)?;
        let mut else_stmt = None;
        if self.consume(SimpleTokenEnum::Else).is_ok() {
            else_stmt = Some(self.not_declaration_statement(can_break)?);
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

    fn r#while(&mut self) -> Result<Statement, ParserError> {
        let cp_start = self.peek_last().code_span().start();

        self.consume(SimpleTokenEnum::LeftParen)?;

        let condition = self.expression()?;

        self.consume(SimpleTokenEnum::RightParen)?;

        let body = self.not_declaration_statement(true)?;

        let cp_end = self.peek_last().code_span().end();

        Ok(while_statement!(
            condition,
            body,
            CodeSpan::new(cp_start, cp_end)
        ))
    }

    fn r#for(&mut self) -> Result<Statement, ParserError> {
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

        self.consume(SimpleTokenEnum::LeftBrace)?;
        let body = self.block(true)?;

        let cp_end = self.peek_last().code_span().end();

        return Ok(for_statement!(
            initializer,
            condition,
            increment,
            body,
            CodeSpan::new(cp_start, cp_end),
        ));
    }

    fn block(&mut self, can_break: bool) -> Result<Statement, ParserError> {
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
                stmts.push(self.statement(can_break)?);
            }
        }
    }

    fn print_statement(&mut self) -> Result<Statement, ParserError> {
        let cp_start = self.peek_last().code_span().start();
        let value = self.expression()?;
        self.consume(SimpleTokenEnum::Semicolon)?;
        let cp_end = self.peek_last().code_span().end();
        Ok(print_statement!(
            value,
            CodeSpan::new(cp_start, cp_end)
        ))
    }

    fn return_statement(&mut self) -> Result<Statement, ParserError> {
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

    fn break_statement(&mut self) -> Result<Statement, ParserError> {
        let cs = self.peek_last().code_span();
        self.consume(SimpleTokenEnum::Semicolon)?;
        Ok(break_statement!(
            cs
        ))
    }

    fn expression_statement(&mut self) -> Result<Statement, ParserError> {
        let expr = self.expression()?;
        let cp_start = expr.code_span().start();
        self.consume(SimpleTokenEnum::Semicolon)?;
        let cp_end = self.peek_last().code_span().end();
        Ok(expression_statement!(
            expr,
            CodeSpan::new(cp_start, cp_end)
        ))
    }

    fn expect_token_not_found_error(&self, token: &str) -> ParserError {
        if self.current == 0 {
            ParserError::ExpectTokenNotFound(token.to_string(), CodePoint::new(0, 0))
        }
        else {
            ParserError::ExpectTokenNotFound(token.to_string(), self.peek_last().code_span().end())
        }
    }

    fn unexpected_end_error(&self) -> ParserError {
        if self.current == 0 {
            ParserError::UnexpectedEnd(CodePoint::new(0, 0))
        }
        else {
            ParserError::UnexpectedEnd(self.peek_last().code_span().end())
        }
    }

    pub fn parse(tokens: &Vec<Rc<Token>>) -> ParserOutput {
        let mut p = Parser::new(tokens);
        let mut stmts: Vec<Statement> = Vec::new();
        let mut errors: Vec<ParserError> = Vec::new();

        while !p.is_end() {
            match p.statement(false) {
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
    use std::rc::Rc;
    use crate::{
        code::{
            code_point::CodePoint,
            code_span::{
                new_code_span,
                CodeSpan,
            },
        },
        parse::{
            Parse,
            expression::{
                assign::AssignExpression,
                get::GetExpression,
                set::SetExpression,
            },
            statement::class_declare::ClassDeclareStatement,
            parser::{
                Parser,
                ParserError,
            }
        },
        scan::{
            token::Token,
            Scan,
        },
        print::Print,
        utils::test_utils::{
            TestContext,
            parse_expression,
            parse_expression_unknown,
            parse_statement,
            try_parse_statement,
        }
    };

    #[test]
    fn test_set_expression() {
        let tests: Vec<(&str, &str)> = vec![
            (
                "foo.bar = true",
                "(.bar= foo true)"
            ),
            (
                "foo.bar.foo = true",
                "(.foo= (. foo bar) true)"
            )
        ];
        for (src, ast) in tests {
            let expr = parse_expression::<SetExpression>(src);
            assert_eq!(expr.print(), ast);
        }
    }

    #[test]
    fn test_assign_expression() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo;");
        ctx.execute_src("var bar;");
        let tests: Vec<(&str, &str)> = vec![
            (
                "foo = true",
                "(= foo true)"
            ),
            (
                "foo = bar = true",
                "(= foo (= bar true))"
            ),
            (
                "foo = true or false",
                "(= foo (or true false))"
            )
        ];
        for (src, ast) in tests {
            let expr = ctx.resolve_expression::<AssignExpression>(
                parse_expression_unknown(src).as_ref()
            )
                .unwrap();
            assert_eq!(expr.print(), ast);
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
            ParserError::UnexpectedEnd(
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
        let tests: Vec<(&str, ParserError)> = vec![
            (
                "hello(",
                ParserError::UnexpectedEnd(
                    CodePoint::new(0, 6),
                )
            ),
            (
                "hello(}",
                ParserError::UnexpectedToken(
                    CodeSpan::new(
                        CodePoint::new(0, 6),
                        CodePoint::new(0, 7),
                    ),
                )
            ),
            (
                "hello(123",
                ParserError::ExpectTokenNotFound(
                    ")".to_owned(),
                    CodePoint::new(0, 9),
                )
            ),
            (
                "hello(123}",
                ParserError::ExpectTokenMismatch(
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
    fn test_get_expression() {
        let tests: Vec<(&str, &str)> = vec![
            (
                "foo.bar",
                "(. foo bar)"
            ),
            (
                "foo.bar.foo",
                "(. (. foo bar) foo)"
            ),
            (
                "foo.bar().foo",
                "(. (call (. foo bar) ) foo)"
            )
        ];
        for (src, ast) in tests {
            let expr = parse_expression::<GetExpression>(src);
            assert_eq!(expr.print(), ast);
        }
    }

    #[test]
    fn test_get_expression_error() {
        assert_eq!(
            try_parse_statement("foo.true").unwrap_err(),
            ParserError::ExpectIdentifier(
                new_code_span(0, 4, 0, 8)
            ),
        );
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
        let tests: Vec<(&str, ParserError)> = vec![
            (
                "(1 + 1",
                ParserError::ExpectTokenNotFound(
                    ")".to_owned(),
                    CodePoint::new(0, 6),
                )
            ),
            (
                "(1 + 2}",
                ParserError::ExpectTokenMismatch(
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
            ParserError::UnexpectedEnd(CodePoint::new(0, 0))
        );
        let ts: Vec<Rc<Token>> = vec![];
        let mut p = Parser::new(&ts);
        assert_eq!(
            p.primary().unwrap_err(),
            ParserError::UnexpectedEnd(CodePoint::new(0, 0))
        );
    }

    #[test]
    fn test_primary_unexpected_token_error() {
        let ts = "!".scan().0;
        let mut p = Parser::new(&ts);
        assert_eq!(
            p.primary().unwrap_err(),
            ParserError::UnexpectedToken(
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
            assert_eq!(p.statement(false).unwrap().print(), ast);
        }
    }

    #[test]
    fn test_var_declare_statement_expect_identifier_error() {
        let ts = "var true;".scan().0;
        let mut p = Parser::new(&ts);
        assert_eq!(
            p.statement(false).unwrap_err(),
            ParserError::ExpectIdentifier(
                CodeSpan::new(
                    CodePoint::new(0, 4),
                    CodePoint::new(0, 8),
                )
            ),
        );
    }

    #[test]
    fn test_var_declare_statement_missing_semicolon_error() {
        let tests: Vec<(&str, ParserError)> = vec![
            (
                "var foo",
                ParserError::ExpectTokenNotFound(
                    ";".to_string(),
                    CodePoint::new(0, 7),
                )
            ),
            (
                "var foo = true",
                ParserError::ExpectTokenNotFound(
                    ";".to_string(),
                    CodePoint::new(0, 14),
                )
            ),
        ];

        for (src, err) in tests {
            let ts = src.scan().0;
            let mut p = Parser::new(&ts);
            assert_eq!(p.statement(false).unwrap_err(), err);
        }
    }

    #[test]
    fn test_var_declare_statement_unexpect_end_error() {
        let ts = "var foo =".scan().0;
        let mut p = Parser::new(&ts);
        assert_eq!(
            p.statement(false).unwrap_err(),
            ParserError::UnexpectedEnd(
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
            assert_eq!(p.statement(false).unwrap().print(), ast);
        }
    }

    #[test]
    fn test_fun_declare_statement_expect_identifier_error() {
        let ts = "fun ()".scan().0;
        let mut p = Parser::new(&ts);
        assert_eq!(
            p.statement(false).unwrap_err(),
            ParserError::ExpectIdentifier(
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
            p.statement(false).unwrap_err(),
            ParserError::ExpectTokenNotFound(
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
            p.statement(false).unwrap_err(),
            ParserError::ExpectIdentifier(
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
            p.statement(false).unwrap_err(),
            ParserError::DuplicatedFunctionParameter(
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
            p.statement(false).unwrap_err(),
            ParserError::ExpectTokenNotFound(
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
            p.statement(false).unwrap_err(),
            ParserError::ExpectTokenNotFound(
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
            p.statement(false).unwrap_err(),
            ParserError::ExpectTokenMismatch(
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
            p.statement(false).unwrap_err(),
            ParserError::UnexpectedEnd(
                CodePoint::new(0, 11),
            )
        );
    }

    #[test]
    fn test_class_declare_statement() {
        let stmt = parse_statement::<ClassDeclareStatement>(
            "
            class Foo {
                bar() {
                    print \"hello\";
                }

                foo(a, b) {
                    return a + b;
                }
            }
            "
        );
        assert_eq!(
            stmt.print(),
            "class Foo {bar() {print \"hello\";} foo(a, b) {return (+ a b);}}"
        );
    }

    #[test]
    fn test_class_declare_statement_expect_identifier_error() {
        assert_eq!(
            try_parse_statement(
                "
                class true {
                    foo() {
                        print \"hello\";
                    }
                }
                "
            )
                .unwrap_err(),
            ParserError::ExpectIdentifier(
                new_code_span(1, 6, 1, 10)
            )
        );
    }

    #[test]
    fn test_class_declare_statement_left_brace_token_not_found_error() {
        assert_eq!(
            try_parse_statement(
                "
                class Foo
                "
            )
                .unwrap_err(),
            ParserError::ExpectTokenNotFound(
                "{".to_owned(),
                CodePoint::new(1, 9),
            )
        );
    }

    #[test]
    fn test_class_declare_statement_duplicated_method_definition() {
        assert_eq!(
            try_parse_statement(
                "
                class Foo {
                    foo() {}
                    foo() {}
                }
                "
            )
                .unwrap_err(),
            ParserError::DuplicatedMethodDefinition(
                new_code_span(3, 0, 3, 8)
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
            assert_eq!(p.statement(false).unwrap().print(), expect);
        }
    }

    #[test]
    fn test_ifelse_statement_missing_left_paren_error() {
        let ts = "if true) print 1;".scan().0;
        let mut p = Parser::new(&ts);
        assert_eq!(
            p.statement(false).unwrap_err(),
            ParserError::ExpectTokenMismatch(
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
            p.statement(false).unwrap_err(),
            ParserError::ExpectTokenMismatch(
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
            p.statement(false).unwrap().print(),
            "while foo print 1;"
        );
    }

    #[test]
    fn test_while_statement_missing_left_paren_error() {
        let ts = "while foo) print 1;".scan().0;
        let mut p = Parser::new(&ts);
        assert_eq!(
            p.statement(false).unwrap_err(),
            ParserError::ExpectTokenMismatch(
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
            p.statement(false).unwrap_err(),
            ParserError::ExpectTokenMismatch(
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
                "for (;;) { print 1; }",
                "for (;;) {print 1;}",
            ),
            (
                "for (var i = 0; i < 10; i = i + 1) { print 1; }",
                "for (var i = 0; (< i 10); (= i (+ i 1))) {print 1;}",
            ),
        ];
        for (src, expect) in tests {
            let tokens = src.scan().0;
            let mut parser = Parser::new(&tokens);
            assert_eq!(parser.statement(false).unwrap().print(), expect);
        }
    }

    #[test]
    fn test_for_statement_missing_left_paren_error() {
        let tokens = "for var i = 0; i < 10: i = i + 1) print i;".scan().0;
        let mut parser = Parser::new(&tokens);
        assert_eq!(
            parser.statement(false).unwrap_err(),
            ParserError::ExpectTokenMismatch(
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
            parser.statement(false).unwrap_err(),
            ParserError::ExpectTokenMismatch(
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
            parser.statement(false).unwrap_err(),
            ParserError::ExpectTokenMismatch(
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
            parser.statement(false).unwrap_err(),
            ParserError::UnexpectedToken(
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
            parser.statement(false).unwrap_err(),
            ParserError::ExpectTokenMismatch(
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
            parser.statement(false).unwrap_err(),
            ParserError::UnexpectedToken(
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
            parser.statement(false).unwrap_err(),
            ParserError::ExpectTokenNotFound(
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
            parser.statement(false).unwrap_err(),
            ParserError::ExpectTokenNotFound(
                "{".to_owned(),
                CodePoint::new(0, 30)
            )
        );
    }

    #[test]
    fn test_block_statement() {
        let tokens = "{var foo = true; foo = false;}".scan().0;
        let mut parser = Parser::new(&tokens);
        assert_eq!(
            parser.statement(false).unwrap().print(),
            "{var foo = true; (= foo false);}"
        );
    }

    #[test]
    fn test_block_statement_statement_error() {
        let tokens = "{var foo}".scan().0;
        let mut parser = Parser::new(&tokens);
        assert_eq!(
            parser.statement(false).unwrap_err(),
            ParserError::ExpectTokenMismatch(
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
            parser.statement(false).unwrap_err(),
            ParserError::ExpectTokenNotFound(
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
            parser.statement(false).unwrap().print(),
            "print \"hello\";",
        );
    }

    #[test]
    fn test_print_statement_expression_error() {
        let tokens = "print ;".scan().0;
        let mut parser = Parser::new(&tokens);
        assert_eq!(
            parser.statement(false).unwrap_err(),
            ParserError::UnexpectedToken(
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
            parser.statement(false).unwrap_err(),
            ParserError::ExpectTokenNotFound(
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
            assert_eq!(parser.statement(false).unwrap().print(), expect);
        }
    }

    #[test]
    fn test_return_statement_missing_semicolon_error() {
        let tests: Vec<(&str, ParserError)> = vec![
            (
                "return",
                ParserError::UnexpectedEnd(
                    CodePoint::new(0, 6),
                ),
            ),
            (
                "return true",
                ParserError::ExpectTokenNotFound(
                    ";".to_owned(),
                    CodePoint::new(0, 11),
                ),
            ),
        ];
        for (src, error) in tests {
            let tokens = src.scan().0;
            let mut parser = Parser::new(&tokens);
            assert_eq!(parser.statement(false).unwrap_err(), error);
        }
    }

    #[test]
    fn test_break_statement_while_break() {
        let tokens =
            "
            while (true) {
                break;
            }
            "
            .scan().0;
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        assert_eq!(
            stmts[0].print(),
            "while true {break;}"
        );
    }

    #[test]
    fn test_break_statement_for_break() {
        let tokens =
            "
            for (;;) {
                break;
            }
            "
            .scan().0;
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        assert_eq!(
            stmts[0].print(),
            "for (;;) {break;}"
        );
    }

    #[test]
    fn test_break_statement_context_not_support_break_error() {
        let tokens =
            "
            break;
            "
            .scan().0;
        let (_, errors) = &tokens.parse();
        assert_eq!(
            errors[0],
            ParserError::ContextNotSupportBreak(
                CodeSpan::new(
                    CodePoint::new(1, 0),
                    CodePoint::new(1, 5),
                )
            )
        );
    }

    #[test]
    fn test_break_statement_missing_semicolon_error() {
        let tokens =
            "
            while (true) {
                break
            }
            "
            .scan().0;
        let (_, errors) = &tokens.parse();
        assert_eq!(
            errors[0],
            ParserError::ExpectTokenMismatch(
                ";".to_owned(),
                CodeSpan::new(
                    CodePoint::new(3, 0),
                    CodePoint::new(3, 1),
                )
            )
        );
    }

    #[test]
    fn test_expression_statement() {
        let tokens = "true;".scan().0;
        let mut parser = Parser::new(&tokens);
        assert_eq!(
            parser.statement(false).unwrap().print(),
            "true;",
        );
    }

    #[test]
    fn test_expression_statement_expression_error() {
        let tokens = "".scan().0;
        let mut parser = Parser::new(&tokens);
        assert_eq!(
            parser.statement(false).unwrap_err(),
            ParserError::UnexpectedEnd(
                CodePoint::new(0, 0)
            )
        );
    }

    #[test]
    fn test_expression_statement_missing_semicolon_error() {
        let tokens = "true".scan().0;
        let mut parser = Parser::new(&tokens);
        assert_eq!(
            parser.statement(false).unwrap_err(),
            ParserError::ExpectTokenNotFound(
                ";".to_string(),
                CodePoint::new(0, 4),
            )
        );
    }
}
