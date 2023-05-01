use std::str::FromStr;

enum TokenType {
    // Single character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Leterals.
    Identifier,
    String,
    Number(f64),
    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Eof,
}

struct Token {
    r#type: TokenType,
    lexeme: String,
    line: usize,
}

struct Error {
    message: &'static str,
    line: usize,
}

impl Error {
    fn unclosed_string(line: usize) -> Error {
        Error {
            message: "Unclosed string.",
            line,
        }
    }
}

pub struct Scanner<'a> {
    src: &'a str,
    line: usize,
    start: usize,
    current: usize,
    tokens: Vec<Token>,
    errors: Vec<Error>,
}

impl Scanner<'_> {
    fn new(source: &str) -> Scanner {
        Scanner {
            src: source,
            line: 1,
            start: 0,
            current: 0,
            tokens: Vec::new(),
            errors: Vec::new(),
        }
    }

    fn scan(&mut self) {
        loop {
            self.init();

            if let Some(c) = self.peek() {
                match c {
                    "(" => self.single_char_token_nocheck(TokenType::LeftParen),
                    ")" => self.single_char_token_nocheck(TokenType::RightParen),
                    "{" => self.single_char_token_nocheck(TokenType::LeftBrace),
                    "}" => self.single_char_token_nocheck(TokenType::RightBrace),
                    "," => self.single_char_token_nocheck(TokenType::Comma),
                    "." => self.single_char_token_nocheck(TokenType::Dot),
                    "-" => self.single_char_token_nocheck(TokenType::Minus),
                    "+" => self.single_char_token_nocheck(TokenType::Plus),
                    ";" => self.single_char_token_nocheck(TokenType::Semicolon),
                    "*" => self.single_char_token_nocheck(TokenType::Star),
                    "/" => self.comment_or_slash(),
                    "!" =>
                        self.single_or_double_char_token_nocheck(
                            "=",
                            TokenType::Bang,
                            TokenType::BangEqual
                        ),
                    "=" =>
                        self.single_or_double_char_token_nocheck(
                            "=",
                            TokenType::Equal,
                            TokenType::EqualEqual
                        ),
                    ">" =>
                        self.single_or_double_char_token_nocheck(
                            "=",
                            TokenType::Greater,
                            TokenType::GreaterEqual
                        ),
                    "<" =>
                        self.single_or_double_char_token_nocheck(
                            "=",
                            TokenType::Less,
                            TokenType::LessEqual
                        ),
                    "\"" => self.string_token(),
                    _ => {
                        if is_digit(c) {
                            self.number();
                        } else {
                            unreachable!()
                        }
                    }
                };

            } else {
                break;
            }
        }
    }

    fn single_char_token_nocheck(&mut self, r#type: TokenType) {
        self.tokens.push(
            Token {
                r#type,
                lexeme: self.src[self.start..self.current+1].to_string(),
                line: self.line,
            }
        );
        self.advance();
    }

    fn single_or_double_char_token_nocheck(
        &mut self,
        followed_c: &str,
        sctype: TokenType,
        dctype: TokenType,
    ) {
        self.advance();
        if self.peek_check_equal(followed_c) {
            self.tokens.push(
                Token {
                    r#type: dctype,
                    lexeme: self.src[self.start..self.current+1].to_string(),
                    line: self.line,
                }
            );
            self.advance();
        } else {
            self.tokens.push(
                Token {
                    r#type: sctype,
                    lexeme: self.src[self.start..self.current].to_string(),
                    line: self.line,
                }
            );
        }
    }

    fn string_token(&mut self) {
        let line = self.line;

        loop {
            self.advance();

            if let Some(c) = self.peek() {
                if c == "\"" {
                    self.tokens.push(
                        Token {
                            r#type: TokenType::String,
                            lexeme: self.src[self.start+1..self.current].to_string(),
                            line,
                        }
                    );
                    self.advance();
                    return;

                } else if c == "\n" {
                    self.newline();
                    continue;
                }

            } else {
                self.errors.push(Error::unclosed_string(line));
                return;
            }
        }
    }

    fn comment_or_slash(&mut self) {
        self.advance();

        if self.peek_check_equal("/") {
            loop {
                self.advance();

                if self.peek_check(|c: &str| -> bool { c != "\n" }) {
                    continue;
                }

                return;
            }
        } else {
            self.tokens.push(
                Token {
                    r#type: TokenType::Slash,
                    lexeme: self.src[self.start..self.current].to_string(),
                    line: self.line,
                }
            );
        }
    }

    fn number(&mut self) {
        loop {
            self.advance();
            if !self.peek_check(is_digit) {
                break;
            }
        }

        if !self.peek_check(|c: &str| -> bool { c == "." }) {
            let number_str = &self.src[self.start..self.current];
            self.tokens.push(
                Token {
                    r#type: TokenType::Number(f64::from_str(number_str).unwrap()),
                    lexeme: number_str.to_string(),
                    line: self.line,
                }
            );
            return;
        }

        self.advance();
        if self.peek_check(is_digit) {
            loop {
                self.advance();
                if !self.peek_check(is_digit) {
                    break;
                }
            }
        } else {
            self.retreat();
        }

        let number_str = &self.src[self.start..self.current];
        self.tokens.push(
            Token {
                r#type: TokenType::Number(f64::from_str(number_str).unwrap()),
                lexeme: number_str.to_string(),
                line: self.line,
            }
        );
    }

    fn is_end(&self) -> bool {
        self.start + self.current >= self.src.len()
    }

    fn peek(&self) -> Option<&str> {
        if self.is_end() {
            None
        } else {
            let idx = self.start + self.current;
            Some(&self.src[idx..idx+1])
        }
    }

    fn peek_check_equal(&self, c: &str) -> bool {
        if self.is_end() {
            false
        } else {
            let idx = self.start + self.current;
            &self.src[idx..idx+1] == c
        }
    }

    fn peek_check(&self, checker: fn(&str) -> bool) -> bool {
        if self.is_end() {
            false
        } else {
            let idx = self.start + self.current;
            checker(&self.src[idx..idx+1])
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

    fn init(&mut self) {
        loop {
            if let Some(c) = self.peek() {
                if c == " " {
                    self.advance();
                    continue;

                } else if c == "\n" {
                    self.advance();
                    self.newline();
                    continue;

                }

            } else {
                break;
            }
        }

        self.start = self.current;
    }
}

fn is_digit(c: &str) -> bool {
    if let Ok(c) = char::from_str(c) {
        return c.is_ascii_digit();
    } else {
        return false;
    }
}
