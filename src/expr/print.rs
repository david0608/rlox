use crate::token::{
    TRUE_LEXEME,
    FALSE_LEXEME,
    NIL_LEXEME,
};
use super::{
    UnaryExpression,
    BinaryExpression,
    LiteralExpression,
    GroupingExpression,
};
use crate::visit::Visit;

pub struct Print;

impl Visit<UnaryExpression<'_>, String> for Print {
    fn visit(e: &UnaryExpression<'_>) -> String {
        match e {
            UnaryExpression::Negative(e) => format!("(- {})", e.print()),
            UnaryExpression::Not(e) => format!("(! {})", e.print()),
        }
    }
}

impl Visit<BinaryExpression<'_, '_>, String> for Print {
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

impl Visit<LiteralExpression<'_>, String> for Print {
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

impl Visit<GroupingExpression<'_>, String> for Print {
    fn visit(e: &GroupingExpression) -> String {
        format!("(group {})", e.0.print())
    }
}

#[cfg(test)]
mod tests {
    use crate::token::{
        NumberToken,
        StringToken,
    };
    use crate::expr::{
        Expr,
        UnaryExpression,
        BinaryExpression,
        LiteralExpression,
        GroupingExpression,
    };
    use crate::{
        unary_expression,
        binary_expression,
        literal_expression,
        grouping_expression,
    };

    #[test]
    fn test_print_unary() {
        let expr = unary_expression!(
            Negative,
            literal_expression!(Number, "123", 123.0)
        );
        assert_eq!(expr.print(), "(- 123)");
    }

    #[test]
    fn test_print_binary() {
        let expr = binary_expression!(
            Plus,
            literal_expression!(Number, "1", 1.0),
            literal_expression!(Number, "2", 2.0)
        );
        assert_eq!(expr.print(), "(+ 1 2)");
    }

    #[test]
    fn test_print_literal() {
        let expr = literal_expression!(String, "\"hello\"", "hello");
        assert_eq!(expr.print(), "\"hello\"");
    }

    #[test]
    fn test_print_grouping() {
        let expr = grouping_expression!(
            literal_expression!(Number, "123", 123.0)
        );
        assert_eq!(expr.print(), "(group 123)");
    }

    #[test]
    fn test_print_ast() {
        let ast = binary_expression!(
            Multiply,
            grouping_expression!(
                binary_expression!(
                    Plus,
                    literal_expression!(Number, "1", 1.0),
                    literal_expression!(Number, "2", 2.0)
                )
            ),
            literal_expression!(Number, "3", 3.0)
        );
        assert_eq!(ast.print(), "(* (group (+ 1 2)) 3)");
    }
}
