use crate::code::{
    Code,
    code_span::CodeSpan,
};

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
pub const BREAK_LEXEME: &str = "break";
pub const CLASS_LEXEME: &str = "class";
pub const SUPER_LEXEME: &str = "super";
pub const PRINT_LEXEME: &str = "print";
pub const AND_LEXEME: &str = "and";
pub const OR_LEXEME: &str = "or";
pub const TRUE_LEXEME: &str = "true";
pub const FALSE_LEXEME: &str = "false";
pub const NIL_LEXEME: &str = "nil";
pub const EOF_LEXEME: &str = "eof";

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SimpleTokenEnum {
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
    Break,
    Class,
    Super,
    Print,
    And,
    Or,
    True,
    False,
    Nil,
    Eof,
}

impl SimpleTokenEnum {
    pub fn lexeme(&self) -> &'static str {
        match self {
            SimpleTokenEnum::LeftParen => LEFT_PAREN_LEXEME,
            SimpleTokenEnum::RightParen => RIGHT_PAREN_LEXEME,
            SimpleTokenEnum::LeftBrace => LEFT_BRACE_LEXEME,
            SimpleTokenEnum::RightBrace => RIGHT_BRACE_LEXEME,
            SimpleTokenEnum::Comma => COMMA_LEXEME,
            SimpleTokenEnum::Dot => DOT_LEXEME,
            SimpleTokenEnum::Minus => MINUS_LEXEME,
            SimpleTokenEnum::Plus => PLUS_LEXEME,
            SimpleTokenEnum::Star => STAR_LEXEME,
            SimpleTokenEnum::Semicolon => SEMICOLON_LEXEME,
            SimpleTokenEnum::Bang => BANG_LEXEME,
            SimpleTokenEnum::BangEqual => BANGEQUAL_LEXEME,
            SimpleTokenEnum::Equal => EQUAL_LEXEME,
            SimpleTokenEnum::EqualEqual => EQUALEQUAL_LEXEME,
            SimpleTokenEnum::Greater => GREATER_LEXEME,
            SimpleTokenEnum::GreaterEqual => GREATEREQUAL_LEXEME,
            SimpleTokenEnum::Less => LESS_LEXEME,
            SimpleTokenEnum::LessEqual => LESSEQUAL_LEXEME,
            SimpleTokenEnum::Slash => SLASH_LEXEME,
            SimpleTokenEnum::If => IF_LEXEME,
            SimpleTokenEnum::Else => ELSE_LEXEME,
            SimpleTokenEnum::For => FOR_LEXEME,
            SimpleTokenEnum::While => WHILE_LEXEME,
            SimpleTokenEnum::Var => VAR_LEXEME,
            SimpleTokenEnum::Fun => FUN_LEXEME,
            SimpleTokenEnum::Return => RETURN_LEXEME,
            SimpleTokenEnum::Break => BREAK_LEXEME,
            SimpleTokenEnum::Class => CLASS_LEXEME,
            SimpleTokenEnum::Super => SUPER_LEXEME,
            SimpleTokenEnum::Print => PRINT_LEXEME,
            SimpleTokenEnum::And => AND_LEXEME,
            SimpleTokenEnum::Or => OR_LEXEME,
            SimpleTokenEnum::True => TRUE_LEXEME,
            SimpleTokenEnum::False => FALSE_LEXEME,
            SimpleTokenEnum::Nil => NIL_LEXEME,
            SimpleTokenEnum::Eof => EOF_LEXEME,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SimpleToken {
    variant: SimpleTokenEnum,
    code_span: CodeSpan,
}

impl SimpleToken {
    pub fn new(variant: SimpleTokenEnum, code_span: CodeSpan) -> SimpleToken {
        SimpleToken {
            variant,
            code_span,
        }
    }

    pub fn variant(&self) -> SimpleTokenEnum {
        self.variant
    }

    pub fn lexeme(&self) -> &'static str {
        self.variant.lexeme()
    }
}

impl Code for SimpleToken {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}
