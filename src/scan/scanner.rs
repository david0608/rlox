use std::str::FromStr;
use crate::scan::token::{
    Token,
    SimpleToken,
};

pub enum Error<'src> {
    UnexpectedCharacter(usize, &'src str),
    UnclosedString(usize),
}

impl std::fmt::Display for Error<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::UnexpectedCharacter(l, c) => write!(f, "line {}: Unexpected character {}.", l, c),
            Error::UnclosedString(l) => write!(f, "line {}: Unclosed string.", l),
        }
    }
}

pub type ScannerOutput<'src> = (Vec<Token<'src>>, Vec<Error<'src>>);

pub struct Scanner<'src> {
    src: &'src str,
    line: usize,
    start: usize,
    current: usize,
    tokens: Vec<Token<'src>>,
    errors: Vec<Error<'src>>,
}

impl<'src> Scanner<'src> {
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

    fn peek(&self) -> Option<&'src str> {
        if self.is_end() {
            None
        }
        else {
            Some(&self.src[self.current..self.current+1])
        }
    }

    fn peek_next(&self) -> Option<&'src str> {
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
                if c == " "
                    || c == "\r"
                    || c == "\t"
                {
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

    fn simple_token(&mut self, e: SimpleToken) {
        self.tokens.push(Token::new_simple(e, self.line));
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
            self.simple_token(SimpleToken::Slash);
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
                        Token::new_string(
                            &self.src[self.start..self.current],
                            &self.src[self.start+1..self.current-1],
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
                Token::new_number(
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
            Token::new_number(
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
            "if" => self.simple_token(SimpleToken::If),
            "else" => self.simple_token(SimpleToken::Else),
            "for" => self.simple_token(SimpleToken::For),
            "while" => self.simple_token(SimpleToken::While),
            "var" => self.simple_token(SimpleToken::Var),
            "fun" => self.simple_token(SimpleToken::Fun),
            "return" => self.simple_token(SimpleToken::Return),
            "class" => self.simple_token(SimpleToken::Class),
            "super" => self.simple_token(SimpleToken::Super),
            "this" => self.simple_token(SimpleToken::This),
            "print" => self.simple_token(SimpleToken::Print),
            "and" => self.simple_token(SimpleToken::And),
            "or" => self.simple_token(SimpleToken::Or),
            "true" => self.simple_token(SimpleToken::True),
            "false" => self.simple_token(SimpleToken::False),
            "nil" => self.simple_token(SimpleToken::Nil),
            _ => self.tokens.push(Token::new_ident(lexeme, self.line)),
        }
    }

    fn unexpected_character(&mut self) {
        self.errors.push(Error::UnexpectedCharacter(self.line, self.peek().unwrap()));
        self.advance();
    }

    pub fn scan(src: &str) -> ScannerOutput {
        let mut s = Scanner::new(src);

        loop {
            s.init();

            if let Some(c) = s.peek() {
                match c {
                    "(" => {
                        s.advance();
                        s.simple_token(SimpleToken::LeftParen);
                    }
                    ")" => {
                        s.advance();
                        s.simple_token(SimpleToken::RightParen);
                    }
                    "{" => {
                        s.advance();
                        s.simple_token(SimpleToken::LeftBrace);
                    }
                    "}" => {
                        s.advance();
                        s.simple_token(SimpleToken::RightBrace);
                    }
                    "," => {
                        s.advance();
                        s.simple_token(SimpleToken::Comma);
                    }
                    "." => {
                        s.advance();
                        s.simple_token(SimpleToken::Dot);
                    }
                    "-" => {
                        s.advance();
                        s.simple_token(SimpleToken::Minus);
                    }
                    "+" => {
                        s.advance();
                        s.simple_token(SimpleToken::Plus);
                    }
                    "*" => {
                        s.advance();
                        s.simple_token(SimpleToken::Star);
                    }
                    ";" => {
                        s.advance();
                        s.simple_token(SimpleToken::Semicolon);
                    }
                    "!" => {
                        s.advance();
                        if s.peek_check_equal("=") {
                            s.advance();
                            s.simple_token(SimpleToken::BangEqual);
                        }
                        else {
                            s.simple_token(SimpleToken::Bang);
                        }
                    }
                    "=" => {
                        s.advance();
                        if s.peek_check_equal("=") {
                            s.advance();
                            s.simple_token(SimpleToken::EqualEqual);
                        }
                        else {
                            s.simple_token(SimpleToken::Equal);
                        }
                    }
                    ">" => {
                        s.advance();
                        if s.peek_check_equal("=") {
                            s.advance();
                            s.simple_token(SimpleToken::GreaterEqual);
                        }
                        else {
                            s.simple_token(SimpleToken::Greater);
                        }
                    }
                    "<" => {
                        s.advance();
                        if s.peek_check_equal("=") {
                            s.advance();
                            s.simple_token(SimpleToken::LessEqual);
                        }
                        else {
                            s.simple_token(SimpleToken::Less);
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
                        }
                    }
                }
                continue;
            }

            break;
        }

        s.simple_token(SimpleToken::Eof);

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
    use crate::scan::token::{
        TokenType,
        SimpleToken,
    };
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
        s.simple_token(SimpleToken::LeftParen);
        assert_eq!(s.start, 0);
        assert_eq!(s.current, 0);
        assert_eq!(s.tokens.len(), 1);
        assert_eq!(s.errors.len(), 0);
        let t = &s.tokens[0];
        assert_eq!(t.line(), 1);
        assert_eq!(t.lexeme(), "(");
        match t.token_type() {
            TokenType::Simple(SimpleToken::LeftParen) => { }
            _ => panic!("Should be LeftParenToken.")
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
        let t = &s.tokens[0];
        assert_eq!(t.line(), 1);
        assert_eq!(t.lexeme(), "/");
        match t.token_type() {
            TokenType::Simple(SimpleToken::Slash) => { }
            _ => panic!("Should be SlashToken.")
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
        let t = &s.tokens[0];
        assert_eq!(t.line(), 1);
        assert_eq!(t.lexeme(), "\"test\"");
        if let TokenType::String(st) = t.token_type() {
            assert_eq!(st.literal(), "test");
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
        let t = &s.tokens[0];
        assert_eq!(t.line(), 1);
        assert_eq!(t.lexeme(), "123");
        if let TokenType::Number(nt) = t.token_type() {
            assert_eq!(nt.literal(), 123.0);
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
        let t = &s.tokens[1];
        assert_eq!(t.line(), 1);
        assert_eq!(t.lexeme(), "10.01");
        if let TokenType::Number(nt) = t.token_type() {
            assert_eq!(nt.literal(), 10.01);
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
        let t = &s.tokens[0];
        assert_eq!(t.line(), 1);
        assert_eq!(t.lexeme(), "if");
        match t.token_type() {
            TokenType::Simple(SimpleToken::If) => { }
            _ => panic!("Should be IfToken.")
        }
        let t = &s.tokens[1];
        assert_eq!(t.line(), 2);
        assert_eq!(t.lexeme(), "else");
        match t.token_type() {
            TokenType::Simple(SimpleToken::Else) => { }
            _ => panic!("Should be ElseToken.")
        }
    }

    #[test]
    fn test_scan() {
        let (tokens, errors) = Scanner::scan(
            "fun hello() {
                print \"Hello world!\"
            }"
        );
        assert_eq!(tokens.len(), 9);
        assert_eq!(errors.len(), 0);
        let t = &tokens[0];
        assert_eq!(t.line(), 1);
        match t.token_type() {
            TokenType::Simple(SimpleToken::Fun) => { }
            _ => panic!("Should be Fun.")
        }
        let t = &tokens[1];
        assert_eq!(t.line(), 1);
        match t.token_type() {
            TokenType::Ident(_) => { }
            _ => panic!("Should be Ident.")
        }
        let t = &tokens[2];
        assert_eq!(t.line(), 1);
        match t.token_type() {
            TokenType::Simple(SimpleToken::LeftParen) => { }
            _ => panic!("Should be LeftParen.")
        }
        let t = &tokens[3];
        assert_eq!(t.line(), 1);
        match t.token_type() {
            TokenType::Simple(SimpleToken::RightParen) => { }
            _ => panic!("Should be RightParen.")
        }
        let t = &tokens[4];
        assert_eq!(t.line(), 1);
        match t.token_type() {
            TokenType::Simple(SimpleToken::LeftBrace) => { }
            _ => panic!("Should be LeftBrace.")
        }
        let t = &tokens[5];
        assert_eq!(t.line(), 2);
        match t.token_type() {
            TokenType::Simple(SimpleToken::Print) => { }
            _ => panic!("Should be Print.")
        }
        let t = &tokens[6];
        assert_eq!(t.line(), 2);
        match t.token_type() {
            TokenType::String(_) => { }
            _ => panic!("Should be String.")
        }
        let t = &tokens[7];
        assert_eq!(t.line(), 3);
        match t.token_type() {
            TokenType::Simple(SimpleToken::RightBrace) => { }
            _ => panic!("Should be RightBrace.")
        }
        let t = &tokens[8];
        assert_eq!(t.line(), 3);
        match t.token_type() {
            TokenType::Simple(SimpleToken::Eof) => { }
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
}
