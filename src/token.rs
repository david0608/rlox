use rlox_macro::*;

pub enum TokenEnum<'a> {
    LeftParenToken(LeftParenToken),
    RightParenToken(RightParenToken),
    LeftBraceToken(LeftBraceToken),
    RightBraceToken(RightBraceToken),
    CommaToken(CommaToken),
    DotToken(DotToken),
    MinusToken(MinusToken),
    PlusToken(PlusToken),
    SemicolonToken(SemicolonToken),
    SlashToken(SlashToken),
    StarToken(StarToken),
    BangToken(BangToken),
    BangEqualToken(BangEqualToken),
    EqualToken(EqualToken),
    EqualEqualToken(EqualEqualToken),
    GreaterToken(GreaterToken),
    GreaterEqualToken(GreaterEqualToken),
    LessToken(LessToken),
    LessEqualToken(LessEqualToken),
    IdentToken(IdentToken<'a>),
    StringToken(StringToken<'a>),
    NumberToken(NumberToken<'a>),
    AndToken(AndToken),
    ClassToken(ClassToken),
    ElseToken(ElseToken),
    FalseToken(FalseToken),
    FunToken(FunToken),
    ForToken(ForToken),
    IfToken(IfToken),
    NilToken(NilToken),
    OrToken(OrToken),
    PrintToken(PrintToken),
    ReturnToken(ReturnToken),
    SuperToken(SuperToken),
    ThisToken(ThisToken),
    TrueToken(TrueToken),
    VarToken(VarToken),
    WhileToken(WhileToken),
    EofToken(EofToken),
}

pub trait Token {
    fn lexeme(&self) -> &str;

    fn line(&self) -> usize;
}

pub trait SimpleToken {
    fn new(line: usize) -> Self;

    fn new_enum(line: usize) -> TokenEnum<'static>;
}

declare_simple_token!(LeftParenToken, "(");
declare_simple_token!(RightParenToken, ")");
declare_simple_token!(LeftBraceToken, "{");
declare_simple_token!(RightBraceToken, "}");
declare_simple_token!(CommaToken, ",");
declare_simple_token!(DotToken, ".");
declare_simple_token!(MinusToken, "-");
declare_simple_token!(PlusToken, "+");
declare_simple_token!(StarToken, "*");
declare_simple_token!(SemicolonToken, ";");
declare_simple_token!(BangToken, "!");
declare_simple_token!(BangEqualToken, "!=");
declare_simple_token!(EqualToken, "=");
declare_simple_token!(EqualEqualToken, "==");
declare_simple_token!(GreaterToken, ">");
declare_simple_token!(GreaterEqualToken, ">=");
declare_simple_token!(LessToken, "<");
declare_simple_token!(LessEqualToken, "<=");
declare_simple_token!(SlashToken, "/");
declare_simple_token!(IfToken, "if");
declare_simple_token!(ElseToken, "else");
declare_simple_token!(ForToken, "for");
declare_simple_token!(WhileToken, "while");
declare_simple_token!(VarToken, "var");
declare_simple_token!(FunToken, "fun");
declare_simple_token!(ReturnToken, "return");
declare_simple_token!(ClassToken, "class");
declare_simple_token!(SuperToken, "super");
declare_simple_token!(ThisToken, "this");
declare_simple_token!(PrintToken, "print");
declare_simple_token!(AndToken, "and");
declare_simple_token!(OrToken, "or");
declare_simple_token!(TrueToken, "true");
declare_simple_token!(FalseToken, "false");
declare_simple_token!(NilToken, "nil");
declare_simple_token!(EofToken, "eof");

pub struct IdentToken<'a> {
    lexeme: &'a str,
    line: usize,
}

impl IdentToken<'_> {
    pub fn new(lexeme: &str, line: usize) -> IdentToken {
        IdentToken {
            lexeme,
            line,
        }
    }

    pub fn new_enum(lexeme: &str, line: usize) -> TokenEnum {
        TokenEnum::IdentToken(Self::new(lexeme, line))
    }
}

impl Token for IdentToken<'_> {
    fn lexeme(&self) -> &str {
        self.lexeme
    }

    fn line(&self) -> usize {
        self.line
    }
}

pub struct StringToken<'a> {
    lexeme: &'a str,
    line: usize,
}

impl StringToken<'_> {
    pub fn new(lexeme: &str, line: usize) -> StringToken {
        StringToken {
            lexeme,
            line,
        }
    }

    pub fn new_enum(lexeme: &str, line: usize) -> TokenEnum {
        TokenEnum::StringToken(Self::new(lexeme, line))
    }

    pub fn literal(&self) -> &str {
        &self.lexeme[1..self.lexeme.len()-1]
    }
}

impl Token for StringToken<'_> {
    fn lexeme(&self) -> &str {
        self.lexeme
    }

    fn line(&self) -> usize {
        self.line
    }
}

pub struct NumberToken<'a> {
    lexeme: &'a str,
    literal: f64,
    line: usize,
}

impl NumberToken<'_> {
    pub fn new(lexeme: &str, literal: f64, line: usize) -> NumberToken {
        NumberToken {
            lexeme,
            literal,
            line,
        }
    }

    pub fn new_enum(lexeme: &str, literal: f64, line: usize) -> TokenEnum {
        TokenEnum::NumberToken(Self::new(lexeme, literal, line))
    }

    pub fn literal(&self) -> f64 {
        self.literal
    }
}

impl Token for NumberToken<'_> {
    fn lexeme(&self) -> &str {
        self.lexeme
    }

    fn line(&self) -> usize {
        self.line
    }
}
