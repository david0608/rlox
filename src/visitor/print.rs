use crate::scan::token::{
    TRUE_LEXEME,
    FALSE_LEXEME,
    NIL_LEXEME,
    VAR_LEXEME,
    PRINT_LEXEME,
};
use crate::parse::expr::{
    UnaryExpression,
    BinaryExpression,
    LiteralExpression,
    GroupingExpression,
    VariableExpression,
    AssignExpression,
};
use crate::parse::stmt::{
    VarDeclareStatement,
    BlockStatement,
    ExpressionStatement,
    PrintStatement,
};
use super::{
    Visit,
    Accept,
};

pub struct Print;

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

impl Visit<'_, VariableExpression<'_>, String> for Print {
    fn visit(e: &VariableExpression) -> String {
        e.0.lexeme().to_string()
    }
}

impl Visit<'_, AssignExpression<'_>, String> for Print {
    fn visit(e: &AssignExpression) -> String {
        format!("(= {} {})", e.name.lexeme(), e.value.print())
    }
}

impl Visit<'_, VarDeclareStatement<'_>, String> for Print {
    fn visit(s: &VarDeclareStatement) -> String {
        if let Some(i) = s.initializer.as_ref() {
            format!("{} {} = {};", VAR_LEXEME, s.name.lexeme(), i.print())
        }
        else {
            format!("{} {};", VAR_LEXEME, s.name.lexeme())
        }
    }
}

impl Visit<'_, BlockStatement<'_>, String> for Print {
    fn visit(s: &BlockStatement) -> String {
        let strs = s.0.iter().map(|s| s.print()).collect::<Vec<String>>();
        format!("{{{}}}", strs.join(" "))
    }
}

impl Visit<'_, ExpressionStatement<'_>, String> for Print {
    fn visit(s: &ExpressionStatement) -> String {
        format!("{};", s.0.print())
    }
}

impl Visit<'_, PrintStatement<'_>, String> for Print {
    fn visit(s: &PrintStatement) -> String {
        format!("{} {};", PRINT_LEXEME, s.0.print())
    }
}

#[cfg(test)]
mod tests {
    use crate::visitor::Scannable;
    use crate::parse::Parser;

    #[test]
    fn test_print_expression() {
        let tests: Vec<(&str, &str)> = vec![
            // Unary.
            ("-123", "(- 123)"),
            ("!123", "(! 123)"),
            // Binary.
            ("1 == 2", "(== 1 2)"),
            ("1 != 2", "(!= 1 2)"),
            ("1 < 2", "(< 1 2)"),
            ("1 <= 2", "(<= 1 2)"),
            ("1 > 2", "(> 1 2)"),
            ("1 >= 2", "(>= 1 2)"),
            ("1 + 2", "(+ 1 2)"),
            ("1 - 2", "(- 1 2)"),
            ("1 * 2", "(* 1 2)"),
            ("1 / 2", "(/ 1 2)"),
            // Literal.
            ("true", "true"),
            ("false", "false"),
            ("nil", "nil"),
            ("123.456", "123.456"),
            ("\"hello\"", "\"hello\""),
            // Grouping.
            ("(123)", "(group 123)"),
            ("(1 + 2) * 3", "(* (group (+ 1 2)) 3)"),
            // Variable.
            ("foo", "foo"),
            // Assignment.
            ("foo = true", "(= foo true)"),
        ];
        for (src, expect) in tests {
            let tokens = src.scan().0;
            let expression = Parser::new(&tokens).expression().unwrap();
            assert_eq!(expression.print(), expect);
        }
    }

    #[test]
    fn test_print_statement() {
        let tests: Vec<(&str, &str)> = vec![
            // Var declare.
            ("var foo;", "var foo;"),
            ("var foo = true;", "var foo = true;"),
            ("var foo = 1 + 1;", "var foo = (+ 1 1);"),
            // Block.
            ("{var foo; var bar = true;}", "{var foo; var bar = true;}"),
            ("{var a; {a = true;}}", "{var a; {(= a true);}}"),
            // Expression.
            ("true;", "true;"),
            ("1 + 1;", "(+ 1 1);"),
            // Print.
            ("print foo;", "print foo;"),
            ("print 1 + 1;", "print (+ 1 1);"),
        ];
        for (src, expect) in tests {
            let tokens = src.scan().0;
            let statement = Parser::new(&tokens).statement().unwrap();
            assert_eq!(statement.print(), expect);
        }
    }
}
