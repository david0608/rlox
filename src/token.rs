pub enum SimpleToken {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Star,
    Semicolon,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Slash,
    If,
    Else,
    For,
    While,
    Var,
    Fun,
    Return,
    Class,
    Super,
    This,
    Print,
    And,
    Or,
    True,
    False,
    Nil,
    Eof,
}

impl SimpleToken {
    fn lexeme(&self) -> &str {
        match self {
            Self::LeftParen => "(",
            Self::RightParen => ")",
            Self::LeftBrace => "{",
            Self::RightBrace => "}",
            Self::Comma => ",",
            Self::Dot => ".",
            Self::Minus => "-",
            Self::Plus => "+",
            Self::Star => "*",
            Self::Semicolon => ";",
            Self::Bang => "!",
            Self::BangEqual => "!=",
            Self::Equal => "=",
            Self::EqualEqual => "==",
            Self::Greater => ">",
            Self::GreaterEqual => ">=",
            Self::Less => "<",
            Self::LessEqual => "<=",
            Self::Slash => "/",
            Self::If => "if",
            Self::Else => "else",
            Self::For => "for",
            Self::While => "while",
            Self::Var => "var",
            Self::Fun => "fun",
            Self::Return => "return",
            Self::Class => "class",
            Self::Super => "super",
            Self::This => "this",
            Self::Print => "print",
            Self::And => "and",
            Self::Or => "or",
            Self::True => "true",
            Self::False => "false",
            Self::Nil => "nil",
            Self::Eof => "eof",
        }
    }
}

pub struct NumberToken<'a> {
    lexeme: &'a str,
    literal: f64,
}

impl NumberToken<'_> {
    pub fn literal(&self) -> f64 {
        self.literal
    }
}

pub struct StringToken<'a> {
    lexeme: &'a str,
    literal: &'a str,
}

impl StringToken<'_> {
    pub fn literal(&self) -> &str {
        self.literal
    }
}

pub struct IdentToken<'a> {
    lexeme: &'a str,
}

pub enum TokenType<'a> {
    Simple(SimpleToken),
    Number(NumberToken<'a>),
    String(StringToken<'a>),
    Ident(IdentToken<'a>),
}

pub struct Token<'a> {
    token_type: TokenType<'a>,
    line: usize,
}

impl<'a> Token<'a> {
    pub fn token_type(&self) -> &TokenType {
        &self.token_type
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn lexeme(&self) -> &str {
        match &self.token_type {
            TokenType::Simple(t) => t.lexeme(),
            TokenType::Number(t) => t.lexeme,
            TokenType::String(t) => t.lexeme,
            TokenType::Ident(t) => t.lexeme,
        }
    }

    pub fn new_simple(e: SimpleToken, line: usize) -> Token<'a> {
        Token {
            token_type: TokenType::Simple(e),
            line,
        }
    }

    pub fn new_number(lexeme: &str, literal: f64, line: usize) -> Token {
        Token {
            token_type: TokenType::Number(
                NumberToken {
                    lexeme,
                    literal,
                }
            ),
            line,
        }
    }

    pub fn new_string(lexeme: &'a str, literal: &'a str, line: usize) -> Token<'a> {
        Token {
            token_type: TokenType::String(
                StringToken {
                    lexeme,
                    literal,
                }
            ),
            line,
        }
    }

    pub fn new_ident(lexeme: &'a str, line: usize) -> Token<'a> {
        Token {
            token_type: TokenType::Ident(
                IdentToken {
                    lexeme,
                }
            ),
            line
        }
    }
}
