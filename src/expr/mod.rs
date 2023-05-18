use crate::visit::Accept;
use crate::token::Token;

mod print;
use print::Print;

struct UnaryExpr<'a> {
    operator: Token<'a>,
    rhs: Box<dyn Expr + 'a>,
}

impl<'a> UnaryExpr<'a> {
    fn new<T>(operator: Token<'a>, rhs: T) -> UnaryExpr<'a>
        where
        T: Expr + 'a
    {
        UnaryExpr {
            operator,
            rhs: Box::new(rhs),
        }
    }
}

struct BinaryExpr<'a> {
    operator: Token<'a>,
    lhs: Box<dyn Expr + 'a>,
    rhs: Box<dyn Expr + 'a>,
}

impl<'a> BinaryExpr<'a> {
    fn new<L, R>(operator: Token<'a>, lhs: L, rhs: R) -> BinaryExpr<'a>
        where
        L: Expr + 'a,
        R: Expr + 'a
    {
        BinaryExpr {
            operator,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
}

struct LiteralExpr<'a> {
    value: Token<'a>,
}

impl LiteralExpr<'_> {
    fn new(value: Token) -> LiteralExpr {
        LiteralExpr {
            value,
        }
    }
}

struct GroupingExpr<'a> {
    expr: Box<dyn Expr + 'a>,
}

impl<'a> GroupingExpr<'a> {
    fn new<T>(expr: T) -> GroupingExpr<'a>
        where
        T: Expr + 'a
    {
        GroupingExpr {
            expr: Box::new(expr),
        }
    }
}

trait Expr:
    Accept<Print, String>
{
    fn print(&self) -> String {
        self.accept(Print)
    }
}

impl<T> Expr for T
    where
    T: Accept<Print, String>
{ }
