use std::fmt;
use std::str::FromStr;
use crate::token::*;
use crate::token::Token;

enum Error<'a> {
    UnexpectedCharacter(usize, &'a str),
    UnclosedString(usize),
}

impl fmt::Display for Error<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::UnexpectedCharacter(l, c) => write!(f, "line {}: Unexpected character {}.", l, c),
            Error::UnclosedString(l) => write!(f, "line {}: Unclosed string.", l),
        }
    }
}

pub struct Scanner<'a> {
    src: &'a str,
    line: usize,
    start: usize,
    current: usize,
    tokens: Vec<TokenEnum<'a>>,
    errors: Vec<Error<'a>>,
}

impl Scanner<'_> {
    pub fn new(src: &str) -> Scanner {
        Scanner {
            src,
            line: 1,
            start: 0,
            current: 0,
            tokens: Vec::new(),
            errors: Vec::new(),
        }
    }

    fn advance(&mut self) {
        self.current += 1;
    }

    fn retreat(&mut self) {
        self.current -= 1;
    }

    fn newline(&mut self) {
        self.line += 1;
    }

    fn is_end(&self) -> bool {
        self.current >= self.src.len()
    }

    fn is_next_end(&self) -> bool {
        self.current + 1 >= self.src.len()
    }

    fn peek(&self) -> Option<&str> {
        if self.is_end() {
            None
        }
        else {
            Some(&self.src[self.current..self.current+1])
        }
    }

    fn peek_next(&self) -> Option<&str> {
        if self.is_next_end() {
            None
        }
        else {
            Some(&self.src[self.current+1..self.current+2])
        }
    }

    fn peek_check<F>(&self, checker: F) -> bool
        where
        F: Fn(&str) -> bool
    {
        if let Some(pc) = self.peek() {
            checker(pc)
        }
        else {
            false
        }
    }

    fn peek_next_check<F>(&self, checker: F) -> bool
        where
        F: Fn(&str) -> bool
    {
        if let Some(pc) = self.peek_next() {
            checker(pc)
        }
        else {
            false
        }
    }

    fn peek_check_equal(&self, c: &str) -> bool {
        self.peek_check(|pc| pc == c)
    }

    fn peek_next_check_equal(&self, c: &str) -> bool {
        self.peek_next_check(|pc| pc == c)
    }

    fn init(&mut self) {
        loop {
            if let Some(c) = self.peek() {
                if (
                    c == " "
                    || c == "\r"
                    || c == "\t"
                ) {
                    self.advance();
                    continue;
                }
                else if c == "\n" {
                    self.advance();
                    self.newline();
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

    fn simple_token<T: SimpleToken>(&mut self) {
        self.tokens.push(T::new_enum(self.line));
    }

    fn single_char_token<T: SimpleToken>(&mut self) {
        self.advance();
        self.simple_token::<T>();
    }

    fn single_or_double_char_token<S, D>(&mut self, c: &str)
        where
        S: SimpleToken,
        D: SimpleToken
    {
        self.advance();
        if self.peek_check_equal(c) {
            self.advance();
            self.simple_token::<D>();
        }
        else {
            self.simple_token::<S>();
        }
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
            self.simple_token::<SlashToken>();
        }
    }

    fn string_token(&mut self) {
        let line = self.line;
        loop {
            self.advance();
            if let Some(c) = self.peek() {
                if c == "\"" {
                    self.advance();
                    self.tokens.push(
                        StringToken::new_enum(
                            &self.src[self.start..self.current],
                            line,
                        )
                    );
                    break;
                }
                else if c == "\n" {
                    self.newline();
                    continue;
                }
                else {
                    continue;
                }
            }
            else {
                self.errors.push(Error::UnclosedString(line));
                break;
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
                NumberToken::new_enum(
                    nstr,
                    f64::from_str(nstr).unwrap(),
                    self.line,
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
            NumberToken::new_enum(
                nstr,
                f64::from_str(nstr).unwrap(),
                self.line,
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
            "if" => self.simple_token::<IfToken>(),
            "else" => self.simple_token::<ElseToken>(),
            "for" => self.simple_token::<ForToken>(),
            "while" => self.simple_token::<WhileToken>(),
            "var" => self.simple_token::<VarToken>(),
            "fun" => self.simple_token::<FunToken>(),
            "return" => self.simple_token::<ReturnToken>(),
            "class" => self.simple_token::<ClassToken>(),
            "super" => self.simple_token::<SuperToken>(),
            "this" => self.simple_token::<ThisToken>(),
            "print" => self.simple_token::<PrintToken>(),
            "and" => self.simple_token::<AndToken>(),
            "or" => self.simple_token::<OrToken>(),
            "true" => self.simple_token::<TrueToken>(),
            "false" => self.simple_token::<FalseToken>(),
            "nil" => self.simple_token::<NilToken>(),
            _ => self.tokens.push(IdentToken::new_enum(lexeme, self.line)),
        }
    }

    pub fn scan(src: &str) -> Scanner {
        let mut s = Scanner::new(src);

        loop {
            s.init();

            if let Some(c) = s.peek() {
                match c {
                    "(" => s.single_char_token::<LeftParenToken>(),
                    ")" => s.single_char_token::<RightParenToken>(),
                    "{" => s.single_char_token::<LeftBraceToken>(),
                    "}" => s.single_char_token::<RightBraceToken>(),
                    "," => s.single_char_token::<CommaToken>(),
                    "." => s.single_char_token::<DotToken>(),
                    "-" => s.single_char_token::<MinusToken>(),
                    "+" => s.single_char_token::<PlusToken>(),
                    "*" => s.single_char_token::<StarToken>(),
                    ";" => s.single_char_token::<SemicolonToken>(),
                    "!" => s.single_or_double_char_token::<BangToken, BangEqualToken>("="),
                    "=" => s.single_or_double_char_token::<EqualToken, EqualEqualToken>("="),
                    ">" => s.single_or_double_char_token::<GreaterToken, GreaterEqualToken>("="),
                    "<" => s.single_or_double_char_token::<LessToken, LessEqualToken>("="),
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
                            unreachable!();
                        }
                    }
                }
                continue;
            }

            break;
        }

        s.simple_token::<EofToken>();

        return s;
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
    use super::*;

    #[test]
    fn test_scanner_new() {
        let s = Scanner::new("123");
        assert_eq!(s.src, "123");
        assert_eq!(s.line, 1);
        assert_eq!(s.start, 0);
        assert_eq!(s.current, 0);
        assert_eq!(s.tokens.len(), 0);
        assert_eq!(s.errors.len(), 0);
    }

    #[test]
    fn test_scanner_advance() {
        let mut s = Scanner::new("123");
        assert_eq!(s.current, 0);
        s.advance();
        assert_eq!(s.current, 1);
    }

    #[test]
    fn test_scanner_retreat() {
        let mut s = Scanner::new("123");
        assert_eq!(s.current, 0);
        s.advance();
        assert_eq!(s.current, 1);
        s.retreat();
        assert_eq!(s.current, 0);
    }

    #[test]
    fn test_scanner_newline() {
        let mut s = Scanner::new("123");
        assert_eq!(s.line, 1);
        s.newline();
        assert_eq!(s.line, 2);
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
        assert!(!s.is_next_end());
        s.advance();
        assert!(!s.is_next_end());
        s.advance();
        assert!(s.is_next_end());
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
        assert!(s.peek_next_check_equal("2"));
        assert!(!s.peek_next_check_equal("1"));
        s.advance();
        assert!(s.peek_next_check_equal("3"));
        s.advance();
        assert!(!s.peek_next_check_equal("3"));
    }

    #[test]
    fn test_scanner_init() {
        let mut s = Scanner::new("123");
        s.init();
        assert_eq!(s.line, 1);
        assert_eq!(s.start, 0);
        assert_eq!(s.current, 0);

        let mut s = Scanner::new("  \n  \n  123");
        s.init();
        assert_eq!(s.line, 3);
        assert_eq!(s.start, 8);
        assert_eq!(s.current, 8);

        let mut s = Scanner::new("\n\r \t123\n\r");
        s.init();
        assert_eq!(s.line, 2);
        assert_eq!(s.start, 4);
        assert_eq!(s.current, 4);
        s.init();
        assert_eq!(s.line, 2);
        assert_eq!(s.start, 4);
        assert_eq!(s.current, 4);
        s.advance();
        s.advance();
        s.advance();
        s.init();
        assert_eq!(s.line, 3);
        assert_eq!(s.start, 9);
        assert_eq!(s.current, 9);
    }

    #[test]
    fn test_simple_token() {
        let mut s = Scanner::new("(123)");
        s.simple_token::<LeftParenToken>();
        assert_eq!(s.start, 0);
        assert_eq!(s.current, 0);
        assert_eq!(s.tokens.len(), 1);
        assert_eq!(s.errors.len(), 0);
        if let TokenEnum::LeftParenToken(t) = &s.tokens[0] {
            assert_eq!(t.line(), 1);
            assert_eq!(t.lexeme(), "(");
        }
        else {
            panic!("Should be LeftParenToken.");
        }
    }

    #[test]
    fn test_single_char_token() {
        let mut s = Scanner::new("(123)");
        s.single_char_token::<LeftParenToken>();
        assert_eq!(s.current, 1);
        assert_eq!(s.tokens.len(), 1);
        assert_eq!(s.errors.len(), 0);
        if let TokenEnum::LeftParenToken(t) = &s.tokens[0] {
            assert_eq!(t.line(), 1);
            assert_eq!(t.lexeme(), "(");
        }
        else {
            panic!("Should be LeftParenToken.")
        }
    }

    #[test]
    fn test_single_or_double_char_token() {
        let mut s = Scanner::new("=!=");
        s.single_or_double_char_token::<EqualToken, BangEqualToken>("=");
        assert_eq!(s.current, 1);
        assert_eq!(s.tokens.len(), 1);
        assert_eq!(s.errors.len(), 0);
        if let TokenEnum::EqualToken(t) = &s.tokens[0] {
            assert_eq!(t.line(), 1);
            assert_eq!(t.lexeme(), "=");
        }
        else {
            panic!("Should be EqualToken.");
        }
        s.single_or_double_char_token::<EqualToken, BangEqualToken>("=");
        assert_eq!(s.current, 3);
        assert_eq!(s.tokens.len(), 2);
        assert_eq!(s.errors.len(), 0);
        if let TokenEnum::BangEqualToken(t) = &s.tokens[1] {
            assert_eq!(t.line(), 1);
            assert_eq!(t.lexeme(), "!=");
        }
        else {
            panic!("Should be EqualToken.");
        }
    }

    #[test]
    fn test_slash_token_or_comment() {
        let mut s = Scanner::new("2/1//2/1\n\r2/1");
        s.advance();
        s.init();
        s.slash_token_or_comment();
        assert_eq!(s.current, 2);
        assert_eq!(s.tokens.len(), 1);
        assert_eq!(s.errors.len(), 0);
        if let TokenEnum::SlashToken(t) = &s.tokens[0] {
            assert_eq!(t.line(), 1);
            assert_eq!(t.lexeme(), "/");
        }
        else {
            panic!("Should be SlashToken.");
        }
        s.advance();
        s.init();
        s.slash_token_or_comment();
        assert_eq!(s.current, 8);
        assert_eq!(s.tokens.len(), 1);
        assert_eq!(s.errors.len(), 0);
    }

    #[test]
    fn test_string_token() {
        let mut s = Scanner::new("\"test\" \"test");
        s.string_token();
        assert_eq!(s.current, 6);
        assert_eq!(s.tokens.len(), 1);
        assert_eq!(s.errors.len(), 0);
        if let TokenEnum::StringToken(t) = &s.tokens[0] {
            assert_eq!(t.line(), 1);
            assert_eq!(t.lexeme(), "\"test\"");
            assert_eq!(t.literal(), "test");
        }
        else {
            panic!("Should be StringToken.");
        }
        s.init();
        s.string_token();
        assert_eq!(s.current, 12);
        assert_eq!(s.tokens.len(), 1);
        assert_eq!(s.errors.len(), 1);
        if let Error::UnclosedString(l) = s.errors[0] {
            assert_eq!(l, 1);
        }
        else {
            panic!("Should be UnclosedString error.");
        }
    }

    #[test]
    fn test_number_token() {
        let mut s = Scanner::new("123. 10.01");
        s.number_token();
        assert_eq!(s.current, 3);
        assert_eq!(s.tokens.len(), 1);
        assert_eq!(s.errors.len(), 0);
        if let TokenEnum::NumberToken(t) = &s.tokens[0] {
            assert_eq!(t.line(), 1);
            assert_eq!(t.lexeme(), "123");
            assert_eq!(t.literal(), 123.0);
        }
        else {
            panic!("Should be NumberToken.");
        }
        s.advance();
        s.init();
        s.number_token();
        assert_eq!(s.current, 10);
        assert_eq!(s.tokens.len(), 2);
        assert_eq!(s.errors.len(), 0);
        if let TokenEnum::NumberToken(t) = &s.tokens[1] {
            assert_eq!(t.line(), 1);
            assert_eq!(t.lexeme(), "10.01");
            assert_eq!(t.literal(), 10.01);
        }
        else {
            panic!("Should be NumberToken.");
        }
    }

    #[test]
    fn test_ident_or_keyword_token() {
        let mut s = Scanner::new("if\n\relse");
        s.ident_or_keyword_token();
        s.init();
        s.ident_or_keyword_token();
        assert_eq!(s.current, 8);
        assert_eq!(s.tokens.len(), 2);
        assert_eq!(s.errors.len(), 0);
        if let TokenEnum::IfToken(t) = &s.tokens[0] {
            assert_eq!(t.line(), 1);
            assert_eq!(t.lexeme(), "if");
        }
        else {
            panic!("Should be IfToken.");
        }
        if let TokenEnum::ElseToken(t) = &s.tokens[1] {
            assert_eq!(t.line(), 2);
            assert_eq!(t.lexeme(), "else");
        }
        else {
            panic!("Should be ElseToken.");
        }
    }

    #[test]
    fn test_scan() {
        let s = Scanner::scan(
            "fun hello() {
                print \"Hello world!\"
            }"
        );
        assert_eq!(s.tokens.len(), 9);
        assert_eq!(s.errors.len(), 0);
        if let TokenEnum::FunToken(t) = &s.tokens[0] {
            assert_eq!(t.line(), 1);
        }
        else {
            panic!("Should be FunToken");
        }
        if let TokenEnum::IdentToken(t) = &s.tokens[1] {
            assert_eq!(t.line(), 1);
        }
        else {
            panic!("Should be IdentToken");
        }
        if let TokenEnum::LeftParenToken(t) = &s.tokens[2] {
            assert_eq!(t.line(), 1);
        }
        else {
            panic!("Should be LeftParenToken");
        }
        if let TokenEnum::RightParenToken(t) = &s.tokens[3] {
            assert_eq!(t.line(), 1);
        }
        else {
            panic!("Should be RightParenToken");
        }
        if let TokenEnum::LeftBraceToken(t) = &s.tokens[4] {
            assert_eq!(t.line(), 1);
        }
        else {
            panic!("Should be LeftBraceToken");
        }
        if let TokenEnum::PrintToken(t) = &s.tokens[5] {
            assert_eq!(t.line(), 2);
        }
        else {
            panic!("Should be PrintToken");
        }
        if let TokenEnum::StringToken(t) = &s.tokens[6] {
            assert_eq!(t.line(), 2);
        }
        else {
            panic!("Should be StringToken");
        }
        if let TokenEnum::RightBraceToken(t) = &s.tokens[7] {
            assert_eq!(t.line(), 3);
        }
        else {
            panic!("Should be RightBraceToken");
        }
        if let TokenEnum::EofToken(t) = &s.tokens[8] {
            assert_eq!(t.line(), 3);
        }
        else {
            panic!("Should be EofToken");
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
}
