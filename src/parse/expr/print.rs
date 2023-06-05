use visit::{
    Visit,
    Accept,
};
use crate::scan::{
    TRUE_LEXEME,
    FALSE_LEXEME,
    NIL_LEXEME,
};
use super::unary::UnaryExpression;
use super::binary::BinaryExpression;
use super::literal::LiteralExpression;
use super::grouping::GroupingExpression;

struct Print;

pub trait Printable
    where
    Self: for<'a> Accept<'a, Print, String>
{
    fn print(&self) -> String {
        self.accept(Print)
    }
}

impl<T> Printable for T
    where
    T: for<'a> Accept<'a, Print, String>
{ }

impl Visit<'_, UnaryExpression<'_>, String> for Print {
    fn visit(e: &UnaryExpression<'_>) -> String {
        match e {
            UnaryExpression::Negative(e) => format!("(- {})", e.print()),
            UnaryExpression::Not(e) => format!("(! {})", e.print()),
        }
    }
}

impl Visit<'_, BinaryExpression<'_, '_>, String> for Print {
    fn visit(e: &BinaryExpression<'_, '_>) -> String {
        match e {
            BinaryExpression::Equal(l, r) => format!("(== {} {})", l.print(), r.print()),
            BinaryExpression::NotEqual(l, r) => format!("(!= {} {})", l.print(), r.print()),
            BinaryExpression::Less(l, r) => format!("(< {} {})", l.print(), r.print()),
            BinaryExpression::LessEqual(l, r) => format!("(<= {} {})", l.print(), r.print()),
            BinaryExpression::Greater(l, r) => format!("(> {} {})", l.print(), r.print()),
            BinaryExpression::GreaterEqual(l, r) => format!("(>= {} {})", l.print(), r.print()),
            BinaryExpression::Plus(l, r) => format!("(+ {} {})", l.print(), r.print()),
            BinaryExpression::Minus(l, r) => format!("(- {} {})", l.print(), r.print()),
            BinaryExpression::Multiply(l, r) => format!("(* {} {})", l.print(), r.print()),
            BinaryExpression::Divide(l, r) => format!("(/ {} {})", l.print(), r.print()),
        }
    }
}

impl Visit<'_, LiteralExpression<'_>, String> for Print {
    fn visit(e: &LiteralExpression<'_>) -> String {
        match e {
            LiteralExpression::True => TRUE_LEXEME.to_string(),
            LiteralExpression::False => FALSE_LEXEME.to_string(),
            LiteralExpression::Nil => NIL_LEXEME.to_string(),
            LiteralExpression::Number(t) => t.lexeme().to_string(),
            LiteralExpression::String(t) => t.lexeme().to_string(),
        }
    }
}

impl Visit<'_, GroupingExpression<'_>, String> for Print {
    fn visit(e: &GroupingExpression) -> String {
        format!("(group {})", e.0.print())
    }
}

#[cfg(test)]
mod tests {
    use crate::scan::Scannable;
    use crate::parse::parser::Parser;

    #[test]
    fn test_print() {
        let src = "
            -123;
            !123;
            1 == 2;
            1 != 2;
            1 < 2;
            1 <= 2;
            1 > 2;
            1 >= 2;
            1 + 2;
            1 - 2;
            1 * 2;
            1 / 2;
            true;
            false;
            nil;
            123.456;
            \"hello\";
            (123);
            (1 + 2) * 3;
        ";
        let results = [
            "(- 123)",
            "(! 123)",
            "(== 1 2)",
            "(!= 1 2)",
            "(< 1 2)",
            "(<= 1 2)",
            "(> 1 2)",
            "(>= 1 2)",
            "(+ 1 2)",
            "(- 1 2)",
            "(* 1 2)",
            "(/ 1 2)",
            "true",
            "false",
            "nil",
            "123.456",
            "\"hello\"",
            "(group 123)",
            "(* (group (+ 1 2)) 3)",
        ];
        let tokens = &src.scan().0;
        let mut parser = Parser::new(&tokens);
        for result in results {
            assert_eq!(parser.expression().unwrap().print(), result);
            parser.synchronize();
        }
    }
}
