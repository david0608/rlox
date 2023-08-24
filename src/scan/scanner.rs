use std::str::FromStr;
use crate::error::LoxError;
use crate::code::code_point::CodePoint;
use crate::code::code_span::CodeSpan;
use super::token::Token;
use super::token::simple::SimpleTokenEnum;

pub enum Error {
    UnexpectedCharacter(CodePoint),
    ExpectCharacterNotFound(String, CodePoint),
}

impl LoxError for Error {
    fn print(&self, src_lines: &Vec<&str>) -> String {
        match self {
            Error::UnexpectedCharacter(cp) => {
                let mut out = format!("Error: Unexpected character: {}\r\n", cp.str(&src_lines));
                out += cp.debug_string(&src_lines).as_ref();
                return out;
            }
            Error::ExpectCharacterNotFound(ec, cp) => {
                let mut out = format!("Error: Expect character: {} but not found.\r\n", ec);
                out += cp.debug_string(&src_lines).as_ref();
                return out;
            }
        }
    }
}

pub type ScannerOutput = (Vec<Token>, Vec<Error>);

pub struct Scanner<'src> {
    src: &'src str,
    line: usize,
    char: usize,
    start: usize,
    current: usize,
    tokens: Vec<Token>,
    errors: Vec<Error>,
}

impl<'src> Scanner<'src> {
    pub fn new(src: &str) -> Scanner {
        Scanner {
            src,
            line: 0,
            char: 0,
            start: 0,
            current: 0,
            tokens: Vec::new(),
            errors: Vec::new(),
        }
    }

    fn advance(&mut self) {
        self.char += 1;
        self.current += 1;
    }

    fn newline(&mut self) {
        self.line += 1;
        self.char = 0;
    }

    fn is_end(&self) -> bool {
        self.current >= self.src.len()
    }

    fn _is_next_end(&self) -> bool {
        self.current + 1 >= self.src.len()
    }

    fn peek(&self) -> Option<&'src str> {
        self.src.get(self.current..(self.current + 1))
    }

    fn peek_next(&self) -> Option<&'src str> {
        self.src.get((self.current + 1)..(self.current + 2))
    }

    fn peek_check<F>(&self, checker: F) -> bool
        where
        F: Fn(&str) -> bool
    {
        self.peek()
            .map(checker)
            .unwrap_or(false)
    }

    fn peek_next_check<F>(&self, checker: F) -> bool
        where
        F: Fn(&str) -> bool
    {
        self.peek_next()
            .map(checker)
            .unwrap_or(false)
    }

    fn peek_check_equal(&self, c: &str) -> bool {
        self.peek_check(|pc| pc == c)
    }

    fn _peek_next_check_equal(&self, c: &str) -> bool {
        self.peek_next_check(|pc| pc == c)
    }

    fn get_code_span(&self) -> CodeSpan {
        CodeSpan::new(
            CodePoint::new(self.line, self.char - (self.current - self.start)),
            CodePoint::new(self.line, self.char),
        )
    }

    fn init(&mut self) {
        let mut newline = false;

        loop {
            if let Some(c) = self.peek() {
                if c == " "
                    || c == "\r"
                    || c == "\t"
                {
                    self.advance();
                    if newline {
                        self.char = 0;
                    }
                    continue;
                }
                else if c == "\n" {
                    self.advance();
                    self.newline();
                    newline = true;
                    continue;
                }
                else {
                    break;
                }
            }
            else {
                break;
            }
        }

        self.start = self.current;
    }

    fn simple_token(&mut self, variant: SimpleTokenEnum) {
        self.tokens.push(Token::new_simple(variant, self.get_code_span()));
    }

    fn slash_token_or_comment(&mut self) {
        self.advance();
        if self.peek_check_equal("/") {
            loop {
                self.advance();
                if !self.is_end() && !self.peek_check_equal("\n") {
                    continue;
                }
                else {
                    break;
                }
            }
        }
        else {
            self.simple_token(SimpleTokenEnum::Slash);
        }
    }

    fn string_token(&mut self) {
        let start = CodePoint::new(self.line, self.char);
        self.advance();
        loop {
            if let Some(c) = self.peek() {
                if c == "\"" {
                    self.advance();
                    self.tokens.push(
                        Token::new_string(
                            &self.src[(self.start + 1)..(self.current - 1)],
                            &self.src[self.start..self.current],
                            CodeSpan::new(start, CodePoint::new(self.line, self.char)),
                        )
                    );
                    return;
                }
                else if c == "\n" {
                    self.advance();
                    self.newline();
                    continue;
                }
                else {
                    self.advance();
                    continue;
                }
            }
            else {
                self.expected_character_not_found("\"");
                return;
            }
        }
    }

    fn number_token(&mut self) {
        self.advance();
        while self.peek_check(is_digit) {
            self.advance();
        }

        if !self.peek_check_equal(".") {
            let nstr = &self.src[self.start..self.current];
            self.tokens.push(
                Token::new_number(
                    f64::from_str(nstr).unwrap(),
                    nstr,
                    self.get_code_span(),
                )
            );
            return;
        }

        if self.peek_next_check(is_digit) {
            self.advance();
            while self.peek_check(is_digit) {
                self.advance();
            }
        }

        let nstr = &self.src[self.start..self.current];
        self.tokens.push(
            Token::new_number(
                f64::from_str(nstr).unwrap(),
                nstr,
                self.get_code_span(),
            )
        );
    }

    fn ident_or_keyword_token(&mut self) {
        self.advance();
        while self.peek_check(is_alphanumeric) {
            self.advance();
        }

        let lexeme = &self.src[self.start..self.current];
        match lexeme {
            "if" => self.simple_token(SimpleTokenEnum::If),
            "else" => self.simple_token(SimpleTokenEnum::Else),
            "for" => self.simple_token(SimpleTokenEnum::For),
            "while" => self.simple_token(SimpleTokenEnum::While),
            "var" => self.simple_token(SimpleTokenEnum::Var),
            "fun" => self.simple_token(SimpleTokenEnum::Fun),
            "return" => self.simple_token(SimpleTokenEnum::Return),
            "class" => self.simple_token(SimpleTokenEnum::Class),
            "super" => self.simple_token(SimpleTokenEnum::Super),
            "this" => self.simple_token(SimpleTokenEnum::This),
            "print" => self.simple_token(SimpleTokenEnum::Print),
            "and" => self.simple_token(SimpleTokenEnum::And),
            "or" => self.simple_token(SimpleTokenEnum::Or),
            "true" => self.simple_token(SimpleTokenEnum::True),
            "false" => self.simple_token(SimpleTokenEnum::False),
            "nil" => self.simple_token(SimpleTokenEnum::Nil),
            _ => {
                self.tokens.push(
                    Token::new_identifier(
                        lexeme,
                        self.get_code_span(),
                    )
                );
            }
        }
    }

    fn unexpected_character(&mut self) {
        self.errors.push(Error::UnexpectedCharacter(CodePoint::new(self.line, self.char)));
    }

    fn expected_character_not_found(&mut self, ec: &str) {
        self.errors.push(Error::ExpectCharacterNotFound(ec.to_string(), CodePoint::new(self.line, self.char)));
    }

    pub fn scan(src: &str) -> ScannerOutput {
        let mut s = Scanner::new(src);

        loop {
            s.init();

            if let Some(c) = s.peek() {
                match c {
                    "(" => {
                        s.advance();
                        s.simple_token(SimpleTokenEnum::LeftParen);
                    }
                    ")" => {
                        s.advance();
                        s.simple_token(SimpleTokenEnum::RightParen);
                    }
                    "{" => {
                        s.advance();
                        s.simple_token(SimpleTokenEnum::LeftBrace);
                    }
                    "}" => {
                        s.advance();
                        s.simple_token(SimpleTokenEnum::RightBrace);
                    }
                    "," => {
                        s.advance();
                        s.simple_token(SimpleTokenEnum::Comma);
                    }
                    "." => {
                        s.advance();
                        s.simple_token(SimpleTokenEnum::Dot);
                    }
                    "-" => {
                        s.advance();
                        s.simple_token(SimpleTokenEnum::Minus);
                    }
                    "+" => {
                        s.advance();
                        s.simple_token(SimpleTokenEnum::Plus);
                    }
                    "*" => {
                        s.advance();
                        s.simple_token(SimpleTokenEnum::Star);
                    }
                    ";" => {
                        s.advance();
                        s.simple_token(SimpleTokenEnum::Semicolon);
                    }
                    "!" => {
                        s.advance();
                        if s.peek_check_equal("=") {
                            s.advance();
                            s.simple_token(SimpleTokenEnum::BangEqual);
                        }
                        else {
                            s.simple_token(SimpleTokenEnum::Bang);
                        }
                    }
                    "=" => {
                        s.advance();
                        if s.peek_check_equal("=") {
                            s.advance();
                            s.simple_token(SimpleTokenEnum::EqualEqual);
                        }
                        else {
                            s.simple_token(SimpleTokenEnum::Equal);
                        }
                    }
                    ">" => {
                        s.advance();
                        if s.peek_check_equal("=") {
                            s.advance();
                            s.simple_token(SimpleTokenEnum::GreaterEqual);
                        }
                        else {
                            s.simple_token(SimpleTokenEnum::Greater);
                        }
                    }
                    "<" => {
                        s.advance();
                        if s.peek_check_equal("=") {
                            s.advance();
                            s.simple_token(SimpleTokenEnum::LessEqual);
                        }
                        else {
                            s.simple_token(SimpleTokenEnum::Less);
                        }
                    }
                    "/" => s.slash_token_or_comment(),
                    "\"" => s.string_token(),
                    _ => {
                        if is_digit(c) {
                            s.number_token();
                        }
                        else if is_alpha(c) {
                            s.ident_or_keyword_token();
                        }
                        else {
                            s.unexpected_character();
                            s.advance();
                        }
                    }
                }
                continue;
            }

            break;
        }

        s.simple_token(SimpleTokenEnum::Eof);

        return (s.tokens, s.errors);
    }
}

fn is_digit(c: &str) -> bool {
    if let Ok(c) = char::from_str(c) {
        c.is_ascii_digit()
    }
    else {
        false
    }
}

fn is_alpha(c: &str) -> bool {
    if let Ok(c) = char::from_str(c) {
        c.is_ascii_alphabetic() || c == '_'
    }
    else {
        false
    }
}

fn is_alphanumeric(c: &str) -> bool {
    if let Ok(c) = char::from_str(c) {
        c.is_ascii_alphanumeric() || c == '_'
    }
    else {
        false
    }
}

#[cfg(test)]
mod tests {
    use crate::error::LoxError;
    use crate::code::Code;
    use crate::code::code_point::CodePoint;
    use crate::code::code_span::CodeSpan;
    use crate::scan::token::Token;
    use crate::scan::token::simple::SimpleTokenEnum;
    use super::{
        Scanner,
        Error,
        is_digit,
        is_alpha,
        is_alphanumeric,
    };

    #[test]
    fn test_scanner_new() {
        let s = Scanner::new("123");
        assert_eq!(s.src, "123");
        assert_eq!(s.line, 0);
        assert_eq!(s.char, 0);
        assert_eq!(s.start, 0);
        assert_eq!(s.current, 0);
        assert_eq!(s.tokens.len(), 0);
        assert_eq!(s.errors.len(), 0);
    }

    #[test]
    fn test_scanner_advance() {
        let mut s = Scanner::new("123");
        assert_eq!(s.char, 0);
        assert_eq!(s.current, 0);
        s.advance();
        assert_eq!(s.char, 1);
        assert_eq!(s.current, 1);
    }

    #[test]
    fn test_scanner_newline() {
        let mut s = Scanner::new("123");
        s.advance();
        assert_eq!(s.line, 0);
        assert_eq!(s.char, 1);
        s.newline();
        assert_eq!(s.line, 1);
        assert_eq!(s.char, 0);
    }

    #[test]
    fn test_scaner_is_end() {
        let mut s = Scanner::new("123");
        assert!(!s.is_end());
        s.advance();
        assert!(!s.is_end());
        s.advance();
        assert!(!s.is_end());
        s.advance();
        assert!(s.is_end());
    }

    #[test]
    fn test_scaner_is_next_end() {
        let mut s = Scanner::new("123");
        assert!(!s._is_next_end());
        s.advance();
        assert!(!s._is_next_end());
        s.advance();
        assert!(s._is_next_end());
    }

    #[test]
    fn test_scanner_peek() {
        let mut s = Scanner::new("123");
        assert!(s.peek().is_some());
        assert_eq!(s.peek().unwrap(), "1");
        s.advance();
        assert!(s.peek().is_some());
        assert_eq!(s.peek().unwrap(), "2");
        s.advance();
        assert!(s.peek().is_some());
        assert_eq!(s.peek().unwrap(), "3");
        s.advance();
        assert!(s.peek().is_none());
    }

    #[test]
    fn test_scanner_peek_next() {
        let mut s = Scanner::new("123");
        assert!(s.peek_next().is_some());
        assert_eq!(s.peek_next().unwrap(), "2");
        s.advance();
        assert!(s.peek_next().is_some());
        assert_eq!(s.peek_next().unwrap(), "3");
        s.advance();
        assert!(s.peek_next().is_none());
    }

    #[test]
    fn test_scanner_peek_check() {
        let checker: fn(&str) -> bool = |c| c == "1";
        let mut s = Scanner::new("123");
        assert!(s.peek_check(checker));
        s.advance();
        assert!(!s.peek_check(checker));
        s.advance();
        assert!(!s.peek_check(checker));
        s.advance();
        assert!(!s.peek_check(checker));
    }

    #[test]
    fn test_scanner_peek_next_check() {
        let checker: fn(&str) -> bool = |c| c == "3";
        let mut s = Scanner::new("123");
        assert!(!s.peek_next_check(checker));
        s.advance();
        assert!(s.peek_next_check(checker));
        s.advance();
        assert!(!s.peek_next_check(checker));
    }

    #[test]
    fn test_scanner_peek_check_equal() {
        let mut s = Scanner::new("123");
        assert!(s.peek_check_equal("1"));
        assert!(!s.peek_check_equal("2"));
        s.advance();
        assert!(s.peek_check_equal("2"));
        s.advance();
        assert!(s.peek_check_equal("3"));
        s.advance();
        assert!(!s.peek_check_equal(""));
    }

    #[test]
    fn test_scanner_peek_next_check_equal() {
        let mut s = Scanner::new("123");
        assert!(s._peek_next_check_equal("2"));
        assert!(!s._peek_next_check_equal("1"));
        s.advance();
        assert!(s._peek_next_check_equal("3"));
        s.advance();
        assert!(!s._peek_next_check_equal("3"));
    }

    #[test]
    fn test_scanner_init() {
        let mut s = Scanner::new("123");
        s.init();
        assert_eq!(s.line, 0);
        assert_eq!(s.char, 0);
        assert_eq!(s.start, 0);
        assert_eq!(s.current, 0);

        let mut s = Scanner::new("  \r\n  \r\n  123");
        s.init();
        assert_eq!(s.line, 2);
        assert_eq!(s.char, 0);
        assert_eq!(s.start, 10);
        assert_eq!(s.current, 10);

        let mut s = Scanner::new("\r\n \t123\n\r");
        s.init();
        assert_eq!(s.line, 1);
        assert_eq!(s.char, 0);
        assert_eq!(s.start, 4);
        assert_eq!(s.current, 4);
        s.init();
        assert_eq!(s.line, 1);
        assert_eq!(s.char, 0);
        assert_eq!(s.start, 4);
        assert_eq!(s.current, 4);
        s.advance();
        s.advance();
        s.advance();
        s.init();
        assert_eq!(s.line, 2);
        assert_eq!(s.char, 0);
        assert_eq!(s.start, 9);
        assert_eq!(s.current, 9);
    }

    #[test]
    fn test_simple_token() {
        let mut s = Scanner::new("(123)");
        s.advance();
        s.simple_token(SimpleTokenEnum::LeftParen);
        assert_eq!(s.tokens.len(), 1);
        assert_eq!(s.errors.len(), 0);
        let t = &s.tokens[0];
        assert_eq!(t.code_span(), CodeSpan::new(CodePoint::new(0, 0), CodePoint::new(0, 1)));
        match t {
            Token::Simple(st) => {
                assert_eq!(st.variant(), SimpleTokenEnum::LeftParen);
            }
            _ => panic!("Should be LeftParenToken.")
        }
    }

    #[test]
    fn test_slash_token_or_comment() {
        let mut s = Scanner::new("2/1//2/1\r\n2/1");
        s.advance();
        s.init();
        s.slash_token_or_comment();
        assert_eq!(s.tokens.len(), 1);
        assert_eq!(s.errors.len(), 0);
        let t = &s.tokens[0];
        assert_eq!(t.code_span(), CodeSpan::new(CodePoint::new(0, 1), CodePoint::new(0, 2)));
        match t {
            Token::Simple(st) => {
                assert_eq!(st.variant(), SimpleTokenEnum::Slash);
            }
            _ => panic!("Should be SlashToken.")
        }
        s.advance();
        s.init();
        s.slash_token_or_comment();
        assert_eq!(s.current, 9);
        assert_eq!(s.tokens.len(), 1);
        assert_eq!(s.errors.len(), 0);
    }

    #[test]
    fn test_string_token() {
        let mut s = Scanner::new("\"test\"\n\"test");
        s.string_token();
        assert_eq!(s.tokens.len(), 1);
        assert_eq!(s.errors.len(), 0);
        let t = &s.tokens[0];
        assert_eq!(t.code_span(), CodeSpan::new(CodePoint::new(0, 0), CodePoint::new(0, 6)));
        match t {
            Token::String(st) => {
                assert_eq!(st.literal(), "test");
            }
            _ => panic!("Should be StringToken.")
        }
        s.init();
        s.string_token();
        assert_eq!(s.current, 12);
        assert_eq!(s.tokens.len(), 1);
        assert_eq!(s.errors.len(), 1);
        match s.errors[0] {
            Error::ExpectCharacterNotFound(ref ec, cp) => {
                assert_eq!(ec, "\"");
                assert_eq!(cp, CodePoint::new(1, 5));
            }
            _ => panic!("Should be UnclosedString error.")
        }
    }

    #[test]
    fn test_number_token() {
        let mut s = Scanner::new("123. 10.01");
        s.number_token();
        assert_eq!(s.tokens.len(), 1);
        assert_eq!(s.errors.len(), 0);
        let t = &s.tokens[0];
        assert_eq!(t.code_span(), CodeSpan::new(CodePoint::new(0, 0), CodePoint::new(0, 3)));
        match t {
            Token::Number(nt) => {
                assert_eq!(nt.literal(), 123.0);
            }
            _ => panic!("Should be NumberToken.")
        }
        s.advance();
        s.init();
        s.number_token();
        assert_eq!(s.tokens.len(), 2);
        assert_eq!(s.errors.len(), 0);
        let t = &s.tokens[1];
        assert_eq!(t.code_span(), CodeSpan::new(CodePoint::new(0, 5), CodePoint::new(0, 10)));
        match t {
            Token::Number(nt) => {
                assert_eq!(nt.literal(), 10.01);
            }
            _ => panic!("Should be NumberToken.")
        }
    }

    #[test]
    fn test_ident_or_keyword_token() {
        let mut s = Scanner::new("if\r\nelse");
        s.ident_or_keyword_token();
        s.init();
        s.ident_or_keyword_token();
        assert_eq!(s.tokens.len(), 2);
        assert_eq!(s.errors.len(), 0);
        let t = &s.tokens[0];
        assert_eq!(t.code_span(), CodeSpan::new(CodePoint::new(0, 0), CodePoint::new(0, 2)));
        match t {
            Token::Simple(st) => {
                assert_eq!(st.variant(), SimpleTokenEnum::If);
            }
            _ => panic!("Should be IfToken.")
        }
        let t = &s.tokens[1];
        assert_eq!(t.code_span(), CodeSpan::new(CodePoint::new(1, 0), CodePoint::new(1, 4)));
        match t {
            Token::Simple(st) => {
                assert_eq!(st.variant(), SimpleTokenEnum::Else);
            }
            _ => panic!("Should be ElseToken.")
        }
    }

    #[test]
    fn test_scan() {
        let (tokens, errors) = Scanner::scan(
            concat!(
                "fun hello() {\r\n",
                "    print \"Hello world!\"\r\n",
                "}\r\n",
            )
        );
        assert_eq!(tokens.len(), 9);
        assert_eq!(errors.len(), 0);
        let t = &tokens[0];
        assert_eq!(t.code_span(), CodeSpan::new(CodePoint::new(0, 0), CodePoint::new(0, 3)));
        match t {
            Token::Simple(st) => {
                assert_eq!(st.variant(), SimpleTokenEnum::Fun);
            }
            _ => panic!("Should be Fun.")
        }
        let t = &tokens[1];
        assert_eq!(t.code_span(), CodeSpan::new(CodePoint::new(0, 4), CodePoint::new(0, 9)));
        match t {
            Token::Identifier(it) => {
                assert_eq!(it.name(), "hello");
            }
            _ => panic!("Should be Ident.")
        }
        let t = &tokens[2];
        assert_eq!(t.code_span(), CodeSpan::new(CodePoint::new(0, 9), CodePoint::new(0, 10)));
        match t {
            Token::Simple(st) => {
                assert_eq!(st.variant(), SimpleTokenEnum::LeftParen);
            }
            _ => panic!("Should be LeftParen.")
        }
        let t = &tokens[3];
        assert_eq!(t.code_span(), CodeSpan::new(CodePoint::new(0, 10), CodePoint::new(0, 11)));
        match t {
            Token::Simple(st) => {
                assert_eq!(st.variant(), SimpleTokenEnum::RightParen);
            }
            _ => panic!("Should be RightParen.")
        }
        let t = &tokens[4];
        assert_eq!(t.code_span(), CodeSpan::new(CodePoint::new(0, 12), CodePoint::new(0, 13)));
        match t {
            Token::Simple(st) => {
                assert_eq!(st.variant(), SimpleTokenEnum::LeftBrace);
            }
            _ => panic!("Should be LeftBrace.")
        }
        let t = &tokens[5];
        assert_eq!(t.code_span(), CodeSpan::new(CodePoint::new(1, 0), CodePoint::new(1, 5)));
        match t {
            Token::Simple(st) => {
                assert_eq!(st.variant(), SimpleTokenEnum::Print);
            }
            _ => panic!("Should be Print.")
        }
        let t = &tokens[6];
        assert_eq!(t.code_span(), CodeSpan::new(CodePoint::new(1, 6), CodePoint::new(1, 20)));
        match t {
            Token::String(st) => {
                assert_eq!(st.literal(), "Hello world!");
            }
            _ => panic!("Should be String.")
        }
        let t = &tokens[7];
        assert_eq!(t.code_span(), CodeSpan::new(CodePoint::new(2, 0), CodePoint::new(2, 1)));
        match t {
            Token::Simple(st) => {
                assert_eq!(st.variant(), SimpleTokenEnum::RightBrace);
            }
            _ => panic!("Should be RightBrace.")
        }
        let t = &tokens[8];
        assert_eq!(t.code_span(), CodeSpan::new(CodePoint::new(3, 0), CodePoint::new(3, 0)));
        match t {
            Token::Simple(st) => {
                assert_eq!(st.variant(), SimpleTokenEnum::Eof);
            }
            _ => panic!("Should be Eof.")
        }
    }

    #[test]
    fn test_is_digit() {
        assert!(is_digit("1"));
        assert!(is_digit("2"));
        assert!(is_digit("3"));
        assert!(is_digit("4"));
        assert!(is_digit("5"));
        assert!(is_digit("6"));
        assert!(is_digit("7"));
        assert!(is_digit("8"));
        assert!(is_digit("9"));
        assert!(is_digit("0"));
        assert!(!is_digit("a"));
        assert!(!is_digit("00"));
    }

    #[test]
    fn test_is_alpha() {
        assert!(is_alpha("a"));
        assert!(is_alpha("z"));
        assert!(is_alpha("A"));
        assert!(is_alpha("Z"));
        assert!(is_alpha("_"));
        assert!(!is_alpha("0"));
        assert!(!is_alpha("9"));
        assert!(!is_alpha("@"));
        assert!(!is_alpha("00"));
    }

    #[test]
    fn test_is_alphanumeric() {
        assert!(is_alphanumeric("0"));
        assert!(is_alphanumeric("1"));
        assert!(is_alphanumeric("9"));
        assert!(is_alphanumeric("a"));
        assert!(is_alphanumeric("z"));
        assert!(is_alphanumeric("A"));
        assert!(is_alphanumeric("Z"));
        assert!(is_alphanumeric("_"));
        assert!(!is_alphanumeric("@"));
        assert!(!is_alphanumeric("00"));
    }

    #[test]
    fn test_unexpected_character_error_print() {
        let src = "@\r\n";
        let src_lines: Vec<&str> = src.lines().collect();
        let (tokens, errors) = Scanner::scan(src);

        assert_eq!(tokens.len(), 1);
        assert_eq!(
            tokens[0].code_span(),
            CodeSpan::new(
                CodePoint::new(1, 0),
                CodePoint::new(1, 0),
            ),
        );
        match tokens[0] {
            Token::Simple(st) => {
                assert_eq!(st.variant(), SimpleTokenEnum::Eof);
            }
            _ => panic!("Should be Eof.")
        }

        assert_eq!(errors.len(), 1);
        assert_eq!(
            errors[0].print(&src_lines),
            concat!(
                "Error: Unexpected character: @\r\n",
                "1: @\r\n",
                "   ^\r\n",
            ),
        );
    }

    #[test]
    fn test_expect_character_not_found_error_print() {
        let src = concat!(
            "\"hello\r\n",
            "    world!\r\n",
        );
        let src_lines: Vec<&str> = src.lines().collect();
        let (tokens, errors) = Scanner::scan(src);

        assert_eq!(tokens.len(), 1);
        assert_eq!(
            tokens[0].code_span(),
            CodeSpan::new(
                CodePoint::new(2, 0),
                CodePoint::new(2, 0),
            ),
        );
        match tokens[0] {
            Token::Simple(st) => {
                assert_eq!(st.variant(), SimpleTokenEnum::Eof);
            }
            _ => panic!("Should be Eof.")
        }

        assert_eq!(errors.len(), 1);
        assert_eq!(
            errors[0].print(&src_lines),
            concat!(
                "Error: Expect character: \" but not found.\r\n",
                "3: \r\n",
                "   ^\r\n",
            ),
        );
    }
}
