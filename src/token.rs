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

pub const LEFT_PAREN_LEXEME: &str = "(";
pub const RIGHT_PAREN_LEXEME: &str = ")";
pub const LEFT_BRACE_LEXEME: &str = "{";
pub const RIGHT_BRACE_LEXEME: &str = "}";
pub const COMMA_LEXEME: &str = ",";
pub const DOT_LEXEME: &str = ".";
pub const MINUS_LEXEME: &str = "-";
pub const PLUS_LEXEME: &str = "+";
pub const STAR_LEXEME: &str = "*";
pub const SEMICOLON_LEXEME: &str = ";";
pub const BANG_LEXEME: &str = "!";
pub const BANGEQUAL_LEXEME: &str = "!=";
pub const EQUAL_LEXEME: &str = "=";
pub const EQUALEQUAL_LEXEME: &str = "==";
pub const GREATER_LEXEME: &str = ">";
pub const GREATEREQUAL_LEXEME: &str = ">=";
pub const LESS_LEXEME: &str = "<";
pub const LESSEQUAL_LEXEME: &str = "<=";
pub const SLASH_LEXEME: &str = "/";
pub const IF_LEXEME: &str = "if";
pub const ELSE_LEXEME: &str = "else";
pub const FOR_LEXEME: &str = "for";
pub const WHILE_LEXEME: &str = "while";
pub const VAR_LEXEME: &str = "var";
pub const FUN_LEXEME: &str = "fun";
pub const RETURN_LEXEME: &str = "return";
pub const CLASS_LEXEME: &str = "class";
pub const SUPER_LEXEME: &str = "super";
pub const THIS_LEXEME: &str = "this";
pub const PRINT_LEXEME: &str = "print";
pub const AND_LEXEME: &str = "and";
pub const OR_LEXEME: &str = "or";
pub const TRUE_LEXEME: &str = "true";
pub const FALSE_LEXEME: &str = "false";
pub const NIL_LEXEME: &str = "nil";
pub const EOF_LEXEME: &str = "eof";

impl SimpleToken {
    fn lexeme(&self) -> &str {
        match self {
            Self::LeftParen => LEFT_PAREN_LEXEME,
            Self::RightParen => RIGHT_PAREN_LEXEME,
            Self::LeftBrace => LEFT_BRACE_LEXEME,
            Self::RightBrace => RIGHT_BRACE_LEXEME,
            Self::Comma => COMMA_LEXEME,
            Self::Dot => DOT_LEXEME,
            Self::Minus => MINUS_LEXEME,
            Self::Plus => PLUS_LEXEME,
            Self::Star => STAR_LEXEME,
            Self::Semicolon => SEMICOLON_LEXEME,
            Self::Bang => BANG_LEXEME,
            Self::BangEqual => BANGEQUAL_LEXEME,
            Self::Equal => EQUAL_LEXEME,
            Self::EqualEqual => EQUALEQUAL_LEXEME,
            Self::Greater => GREATER_LEXEME,
            Self::GreaterEqual => GREATEREQUAL_LEXEME,
            Self::Less => LESS_LEXEME,
            Self::LessEqual => LESSEQUAL_LEXEME,
            Self::Slash => SLASH_LEXEME,
            Self::If => IF_LEXEME,
            Self::Else => ELSE_LEXEME,
            Self::For => FOR_LEXEME,
            Self::While => WHILE_LEXEME,
            Self::Var => VAR_LEXEME,
            Self::Fun => FUN_LEXEME,
            Self::Return => RETURN_LEXEME,
            Self::Class => CLASS_LEXEME,
            Self::Super => SUPER_LEXEME,
            Self::This => THIS_LEXEME,
            Self::Print => PRINT_LEXEME,
            Self::And => AND_LEXEME,
            Self::Or => OR_LEXEME,
            Self::True => TRUE_LEXEME,
            Self::False => FALSE_LEXEME,
            Self::Nil => NIL_LEXEME,
            Self::Eof => EOF_LEXEME,
        }
    }
}

#[derive(Clone, Copy)]
pub struct NumberToken<'a> {
    lexeme: &'a str,
    literal: f64,
}

impl<'a, 'b> NumberToken<'b> {
    pub fn new(lexeme:&'b str, literal: f64) -> NumberToken<'b> {
        NumberToken {
            lexeme,
            literal
        }
    }

    pub fn lexeme(&'a self) -> &'b str {
        self.lexeme
    }

    pub fn literal(&self) -> f64 {
        self.literal
    }
}

#[derive(Clone, Copy)]
pub struct StringToken<'a> {
    lexeme: &'a str,
    literal: &'a str,
}

impl<'a, 'b> StringToken<'b> {
    pub fn new(lexeme: &'b str, literal: &'b str) -> StringToken<'b> {
        StringToken {
            lexeme,
            literal
        }
    }

    pub fn lexeme(&'a self) -> &'b str {
        self.lexeme
    }

    pub fn literal(&'a self) -> &'b str {
        self.literal
    }
}

pub struct IdentToken<'a> {
    lexeme: &'a str,
}

impl<'a, 'b> IdentToken<'b> {
    pub fn new(lexeme: &'b str) -> IdentToken<'b> {
        IdentToken {
            lexeme
        }
    }

    pub fn lexeme(&'a self) -> &'b str {
        self.lexeme
    }
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

impl<'a, 'b> Token<'b> {
    pub fn token_type(&'a self) -> &'a TokenType<'b> {
        &self.token_type
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn lexeme(&'a self) -> &'b str
        where
        'a: 'b
    {
        match &self.token_type {
            TokenType::Simple(t) => t.lexeme(),
            TokenType::Number(t) => t.lexeme(),
            TokenType::String(t) => t.lexeme(),
            TokenType::Ident(t) => t.lexeme(),
        }
    }

    pub fn new_simple(e: SimpleToken, line: usize) -> Token<'static> {
        Token {
            token_type: TokenType::Simple(e),
            line,
        }
    }

    pub fn new_number(lexeme: &'b str, literal: f64, line: usize) -> Token<'b> {
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

    pub fn new_string(lexeme: &'b str, literal: &'b str, line: usize) -> Token<'b> {
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

    pub fn new_ident(lexeme: &'b str, line: usize) -> Token<'b> {
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
