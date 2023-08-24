use crate::scan::token::simple::{
    TRUE_LEXEME,
    FALSE_LEXEME,
    NIL_LEXEME,
    VAR_LEXEME,
    PRINT_LEXEME,
};
use crate::parse::expression::assign::AssignExpression;
use crate::parse::expression::binary::{
    BinaryExpression,
    BinaryExpressionEnum,
};
use crate::parse::expression::call::CallExpression;
use crate::parse::expression::grouping::GroupingExpression;
use crate::parse::expression::literal::{
    LiteralExpression,
    LiteralExpressionEnum,
};
use crate::parse::expression::logical::{
    LogicalExpression,
    LogicalExpressionEnum,
};
use crate::parse::expression::unary::{
    UnaryExpression,
    UnaryExpressionEnum,
};
use crate::parse::expression::variable::VariableExpression;
use crate::parse::statement::block::BlockStatement;
use crate::parse::statement::expression::ExpressionStatement;
use crate::parse::statement::r#for::ForStatement;
use crate::parse::statement::ifelse::IfStatement;
use crate::parse::statement::print::PrintStatement;
use crate::parse::statement::var_declare::VarDeclareStatement;
use crate::parse::statement::r#while::WhileStatement;
use super::{
    Visit,
    Accept,
};

pub struct Print;

pub trait Printable
    where
    Self: for<'this> Accept<'this, Print, String>
{
    fn print(&self) -> String {
        self.accept(Print)
    }
}

impl<T> Printable for T
    where
    T: for<'this> Accept<'this, Print, String>
{ }

impl Visit<'_, UnaryExpression, String> for Print {
    fn visit(e: &UnaryExpression) -> String {
        match e.variant() {
            UnaryExpressionEnum::Negative => format!("(- {})", e.rhs().print()),
            UnaryExpressionEnum::Not => format!("(! {})", e.rhs().print()),
        }
    }
}

impl Visit<'_, BinaryExpression, String> for Print {
    fn visit(e: &BinaryExpression) -> String {
        match e.variant() {
            BinaryExpressionEnum::Equal => format!("(== {} {})", e.lhs().print(), e.rhs().print()),
            BinaryExpressionEnum::NotEqual => format!("(!= {} {})", e.lhs().print(), e.rhs().print()),
            BinaryExpressionEnum::Less => format!("(< {} {})", e.lhs().print(), e.rhs().print()),
            BinaryExpressionEnum::LessEqual => format!("(<= {} {})", e.lhs().print(), e.rhs().print()),
            BinaryExpressionEnum::Greater => format!("(> {} {})", e.lhs().print(), e.rhs().print()),
            BinaryExpressionEnum::GreaterEqual => format!("(>= {} {})", e.lhs().print(), e.rhs().print()),
            BinaryExpressionEnum::Plus => format!("(+ {} {})", e.lhs().print(), e.rhs().print()),
            BinaryExpressionEnum::Minus => format!("(- {} {})", e.lhs().print(), e.rhs().print()),
            BinaryExpressionEnum::Multiply => format!("(* {} {})", e.lhs().print(), e.rhs().print()),
            BinaryExpressionEnum::Divide => format!("(/ {} {})", e.lhs().print(), e.rhs().print()),
        }
    }
}

impl Visit<'_, LiteralExpression, String> for Print {
    fn visit(e: &LiteralExpression) -> String {
        match e.variant() {
            LiteralExpressionEnum::True => TRUE_LEXEME.to_string(),
            LiteralExpressionEnum::False => FALSE_LEXEME.to_string(),
            LiteralExpressionEnum::Nil => NIL_LEXEME.to_string(),
            LiteralExpressionEnum::Number(t) => t.lexeme().to_string(),
            LiteralExpressionEnum::String(t) => t.lexeme().to_string(),
        }
    }
}

impl Visit<'_, GroupingExpression, String> for Print {
    fn visit(e: &GroupingExpression) -> String {
        format!("(group {})", e.expression().print())
    }
}

impl Visit<'_, VariableExpression, String> for Print {
    fn visit(e: &VariableExpression) -> String {
        e.name().to_string()
    }
}

impl Visit<'_, AssignExpression, String> for Print {
    fn visit(e: &AssignExpression) -> String {
        format!("(= {} {})", e.name(), e.value().print())
    }
}

impl Visit<'_, LogicalExpression, String> for Print {
    fn visit(e: &LogicalExpression) -> String {
        match e.variant() {
            LogicalExpressionEnum::And => {
                format!("(and {} {})", e.lhs().print(), e.rhs().print())
            }
            LogicalExpressionEnum::Or => {
                format!("(or {} {})", e.lhs().print(), e.rhs().print())
            }
        }
    }
}

impl Visit<'_, CallExpression, String> for Print {
    fn visit(e: &CallExpression) -> String {
        format!(
            "(call {} {})",
            e.callee().print(),
            e.arguments().iter().map(|s| s.print()).collect::<Vec<String>>().join(" "),
        )
    }
}

impl Visit<'_, BlockStatement, String> for Print {
    fn visit(s: &BlockStatement) -> String {
        let strs = s.statements().iter().map(|s| s.print()).collect::<Vec<String>>();
        format!("{{{}}}", strs.join(" "))
    }
}

impl Visit<'_, ExpressionStatement, String> for Print {
    fn visit(s: &ExpressionStatement) -> String {
        format!("{};", s.expression().print())
    }
}

impl Visit<'_, ForStatement, String> for Print {
    fn visit(s: &ForStatement) -> String {
        let mut inparen;

        if let Some(initializer) = s.initializer() {
            inparen = format!("{}", initializer.print());
        }
        else {
            inparen = ";".to_owned();
        }

        if let Some(condition) = s.condition() {
            inparen = format!("{} {};", inparen, condition.print());
        }
        else {
            inparen = format!("{};", inparen);
        }

        if let Some(increment) = s.increment() {
            inparen = format!("{} {}", inparen, increment.print());
        }

        return format!("for ({}) {}", inparen, s.body().print());
    }
}

impl Visit<'_, IfStatement, String> for Print {
    fn visit(s: &IfStatement) -> String {
        if let Some(else_statement) = s.else_statement() {
            format!(
                "if {} {} else {}",
                s.condition().print(),
                s.then_statement().print(),
                else_statement.print()
            )
        }
        else {
            format!(
                "if {} {}",
                s.condition().print(),
                s.then_statement().print()
            )
        }
    }
}

impl Visit<'_, PrintStatement, String> for Print {
    fn visit(s: &PrintStatement) -> String {
        format!("{} {};", PRINT_LEXEME, s.value().print())
    }
}

impl Visit<'_, VarDeclareStatement, String> for Print {
    fn visit(s: &VarDeclareStatement) -> String {
        if let Some(i) = s.initializer() {
            format!("{} {} = {};", VAR_LEXEME, s.name(), i.print())
        }
        else {
            format!("{} {};", VAR_LEXEME, s.name())
        }
    }
}

impl Visit<'_, WhileStatement, String> for Print {
    fn visit(s: &WhileStatement) -> String {
        if let Some(condition) = s.condition() {
            format!("while {} {}", condition.print(), s.body().print())
        }
        else {
            format!("while true {}", s.body().print())
        }
    }
}

#[macro_export]
macro_rules! impl_debug_for_printable {
    ( $target:ty ) => {
        impl std::fmt::Debug for $target {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.print())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::visitor::scan::Scannable;
    use crate::parse::parser::Parser;

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
            // Logical.
            ("x and y", "(and x y)"),
            ("x or y", "(or x y)"),
            ("x or y and z", "(or x (and y z))"),
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
            // Ifelse.
            ("if (true) print \"hello\";", "if true print \"hello\";"),
            ("if (1 + 1 == 2) { print 1; } else { print 2; }", "if (== (+ 1 1) 2) {print 1;} else {print 2;}"),
            // Expression.
            ("true;", "true;"),
            ("1 + 1;", "(+ 1 1);"),
            // Print.
            ("print foo;", "print foo;"),
            ("print 1 + 1;", "print (+ 1 1);"),
            // While.
            ("while (foo) print \"hello\";", "while foo print \"hello\";"),
            ("while (foo == true) print \"hello\";", "while (== foo true) print \"hello\";"),
        ];
        for (src, expect) in tests {
            let tokens = src.scan().0;
            let statement = Parser::new(&tokens).statement().unwrap();
            assert_eq!(statement.print(), expect);
        }
    }
}
