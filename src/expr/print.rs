use crate::visit::Visit;
use super::{
    UnaryExpr,
    BinaryExpr,
    LiteralExpr,
    GroupingExpr,
};

pub struct Print;

impl Visit<UnaryExpr<'_>, String> for Print {
    fn visit(e: &UnaryExpr<'_>) -> String {
        format!(
            "({} {})",
            e.operator.lexeme().to_string(),
            e.rhs.print()
        )
    }
}

impl Visit<BinaryExpr<'_>, String> for Print {
    fn visit(e: &BinaryExpr<'_>) -> String {
        format!(
            "({} {} {})",
            e.operator.lexeme().to_string(),
            e.lhs.print(),
            e.rhs.print(),
        )
    }
}

impl Visit<LiteralExpr<'_>, String> for Print {
    fn visit(e: &LiteralExpr<'_>) -> String {
        e.value.lexeme().to_string()
    }
}

impl Visit<GroupingExpr<'_>, String> for Print {
    fn visit(e: &GroupingExpr) -> String {
        format!("(group {})", e.expr.print())
    }
}

#[cfg(test)]
mod tests {
    use crate::token::{
        Token,
        SimpleToken,
    };
    use crate::expr::{
        Expr,
        UnaryExpr,
        BinaryExpr,
        LiteralExpr,
        GroupingExpr,
    };

    #[test]
    fn test_print_unary() {
        let expr = UnaryExpr::new(
            Token::new_simple(SimpleToken::Minus, 1),
            LiteralExpr::new(Token::new_number("123", 123.0, 1)),
        );
        assert_eq!(expr.print(), "(- 123)");
    }

    #[test]
    fn test_print_binary() {
        let expr = BinaryExpr::new(
            Token::new_simple(SimpleToken::Plus, 1),
            LiteralExpr::new(Token::new_number("1", 1.0, 1)),
            LiteralExpr::new(Token::new_number("2", 2.0, 1)),
        );
        assert_eq!(expr.print(), "(+ 1 2)");
    }

    #[test]
    fn test_print_literal() {
        let expr = LiteralExpr::new(
            Token::new_string("\"hello\"", "hello", 1),
        );
        assert_eq!(expr.print(), "\"hello\"");
    }

    #[test]
    fn test_print_grouping() {
        let expr = GroupingExpr::new(
            LiteralExpr::new(Token::new_number("123", 123.0, 1)),
        );
        assert_eq!(expr.print(), "(group 123)");
    }

    #[test]
    fn test_print_ast() {
        let ast = BinaryExpr::new(
            Token::new_simple(SimpleToken::Star, 1),
            GroupingExpr::new(
                BinaryExpr::new(
                    Token::new_simple(SimpleToken::Plus, 1),
                    LiteralExpr::new(Token::new_number("1", 1.0, 1)),
                    LiteralExpr::new(Token::new_number("2", 2.0, 1)),
                )
            ),
            LiteralExpr::new(Token::new_number("3", 3.0, 1)),
        );
        assert_eq!(ast.print(), "(* (group (+ 1 2)) 3)");
    }
}
