use crate::parse::{
    expression::{
        assign::AssignExpression,
        binary::{
            BinaryExpression,
            BinaryExpressionEnum,
        },
        call::CallExpression,
        grouping::GroupingExpression,
        literal::{
            LiteralExpression,
            LiteralExpressionEnum,
        },
        logical::{
            LogicalExpression,
            LogicalExpressionEnum,
        },
        unary::{
            UnaryExpression,
            UnaryExpressionEnum,
        },
        variable::VariableExpression,
    },
    statement::{
        block::BlockStatement,
        expression::ExpressionStatement,
        r#for::ForStatement,
        fun_declare::FunDeclareStatement,
        ifelse::IfStatement,
        print::PrintStatement,
        var_declare::VarDeclareStatement,
        r#while::WhileStatement,
    },
};
use crate::scan::token::simple::{
    TRUE_LEXEME,
    FALSE_LEXEME,
    NIL_LEXEME,
    VAR_LEXEME,
    PRINT_LEXEME,
};

pub trait Print {
    fn print(&self) -> String;
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

impl Print for AssignExpression {
    fn print(&self) -> String {
        format!("(= {} {})", self.name(), self.value().print())
    }
}

impl_debug_for_printable!(AssignExpression);

impl Print for BinaryExpression {
    fn print(&self) -> String {
        match self.variant() {
            BinaryExpressionEnum::Equal => format!("(== {} {})", self.lhs().print(), self.rhs().print()),
            BinaryExpressionEnum::NotEqual => format!("(!= {} {})", self.lhs().print(), self.rhs().print()),
            BinaryExpressionEnum::Less => format!("(< {} {})", self.lhs().print(), self.rhs().print()),
            BinaryExpressionEnum::LessEqual => format!("(<= {} {})", self.lhs().print(), self.rhs().print()),
            BinaryExpressionEnum::Greater => format!("(> {} {})", self.lhs().print(), self.rhs().print()),
            BinaryExpressionEnum::GreaterEqual => format!("(>= {} {})", self.lhs().print(), self.rhs().print()),
            BinaryExpressionEnum::Plus => format!("(+ {} {})", self.lhs().print(), self.rhs().print()),
            BinaryExpressionEnum::Minus => format!("(- {} {})", self.lhs().print(), self.rhs().print()),
            BinaryExpressionEnum::Multiply => format!("(* {} {})", self.lhs().print(), self.rhs().print()),
            BinaryExpressionEnum::Divide => format!("(/ {} {})", self.lhs().print(), self.rhs().print()),
        }
    }
}

impl_debug_for_printable!(BinaryExpression);

impl Print for CallExpression {
    fn print(&self) -> String {
        format!(
            "(call {} {})",
            self.callee().print(),
            self.arguments().iter().map(|s| s.print()).collect::<Vec<String>>().join(" "),
        )
    }
}

impl_debug_for_printable!(CallExpression);

impl Print for GroupingExpression {
    fn print(&self) -> String {
        format!("(group {})", self.expression().print())
    }
}

impl_debug_for_printable!(GroupingExpression);

impl Print for LiteralExpression {
    fn print(&self) -> String {
        match self.variant() {
            LiteralExpressionEnum::True => TRUE_LEXEME.to_string(),
            LiteralExpressionEnum::False => FALSE_LEXEME.to_string(),
            LiteralExpressionEnum::Nil => NIL_LEXEME.to_string(),
            LiteralExpressionEnum::Number(t) => t.lexeme().to_string(),
            LiteralExpressionEnum::String(t) => t.lexeme().to_string(),
        }
    }
}

impl_debug_for_printable!(LiteralExpression);

impl Print for LogicalExpression {
    fn print(&self) -> String {
        match self.variant() {
            LogicalExpressionEnum::And => {
                format!("(and {} {})", self.lhs().print(), self.rhs().print())
            }
            LogicalExpressionEnum::Or => {
                format!("(or {} {})", self.lhs().print(), self.rhs().print())
            }
        }
    }
}

impl_debug_for_printable!(LogicalExpression);

impl Print for UnaryExpression {
    fn print(&self) -> String {
        match self.variant() {
            UnaryExpressionEnum::Negative => format!("(- {})", self.rhs().print()),
            UnaryExpressionEnum::Not => format!("(! {})", self.rhs().print()),
        }
    }
}

impl_debug_for_printable!(UnaryExpression);

impl Print for VariableExpression {
    fn print(&self) -> String {
        self.name().to_string()
    }
}

impl_debug_for_printable!(VariableExpression);

impl Print for BlockStatement {
    fn print(&self) -> String {
        let strs = self.statements().iter().map(|s| s.print()).collect::<Vec<String>>();
        format!("{{{}}}", strs.join(" "))
    }
}

impl_debug_for_printable!(BlockStatement);

impl Print for ExpressionStatement {
    fn print(&self) -> String {
        format!("{};", self.expression().print())
    }
}

impl_debug_for_printable!(ExpressionStatement);

impl Print for ForStatement {
    fn print(&self) -> String {
        let mut inparen;

        if let Some(initializer) = self.initializer() {
            inparen = format!("{}", initializer.print());
        }
        else {
            inparen = ";".to_owned();
        }

        if let Some(condition) = self.condition() {
            inparen = format!("{} {};", inparen, condition.print());
        }
        else {
            inparen = format!("{};", inparen);
        }

        if let Some(increment) = self.increment() {
            inparen = format!("{} {}", inparen, increment.print());
        }

        return format!("for ({}) {}", inparen, self.body().print());
    }
}

impl_debug_for_printable!(ForStatement);

impl Print for FunDeclareStatement {
    fn print(&self) -> String {
        return format!(
            "fun {}({}) {{{}}}",
            self.name().name(),
            self.parameters().iter().map(|i| i.name().to_owned()).collect::<Vec<String>>().join(", "),
            self.body().iter().map(|s| s.print()).collect::<Vec<String>>().join(" "),
        );
    }
}

impl_debug_for_printable!(FunDeclareStatement);

impl Print for IfStatement {
    fn print(&self) -> String {
        if let Some(else_statement) = self.else_statement() {
            format!(
                "if {} {} else {}",
                self.condition().print(),
                self.then_statement().print(),
                else_statement.print()
            )
        }
        else {
            format!(
                "if {} {}",
                self.condition().print(),
                self.then_statement().print()
            )
        }
    }
}

impl_debug_for_printable!(IfStatement);

impl Print for PrintStatement {
    fn print(&self) -> String {
        format!("{} {};", PRINT_LEXEME, self.value().print())
    }
}

impl_debug_for_printable!(PrintStatement);

impl Print for VarDeclareStatement {
    fn print(&self) -> String {
        if let Some(i) = self.initializer() {
            format!("{} {} = {};", VAR_LEXEME, self.name(), i.print())
        }
        else {
            format!("{} {};", VAR_LEXEME, self.name())
        }
    }
}

impl_debug_for_printable!(VarDeclareStatement);

impl Print for WhileStatement {
    fn print(&self) -> String {
        if let Some(condition) = self.condition() {
            format!("while {} {}", condition.print(), self.body().print())
        }
        else {
            format!("while true {}", self.body().print())
        }
    }
}

impl_debug_for_printable!(WhileStatement);

#[cfg(test)]
mod tests {
    use crate::scan::Scan;
    use crate::parse::parser::Parser;

    #[test]
    fn test_print_assignment_expression() {
        let tokens = "foo = true".scan().0;
        let expression = Parser::new(&tokens).expression().unwrap();
        assert_eq!(expression.print(), "(= foo true)");
    }

    #[test]
    fn test_print_binary_expression() {
        let tests: Vec<(&str, &str)> = vec![
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
        ];
        for (src, expect) in tests {
            let tokens = src.scan().0;
            let expression = Parser::new(&tokens).expression().unwrap();
            assert_eq!(expression.print(), expect);
        }
    }

    #[test]
    fn test_print_call_expression() {
        let tests: Vec<(&str, &str)> = vec![
            ("foo()", "(call foo )"),
            ("bar(foo, true, 123)", "(call bar foo true 123)"),
        ];
        for (src, expect) in tests {
            let tokens = src.scan().0;
            let expression = Parser::new(&tokens).expression().unwrap();
            assert_eq!(expression.print(), expect);
        }
    }

    #[test]
    fn test_print_grouping_expression() {
        let tests: Vec<(&str, &str)> = vec![
            ("(123)", "(group 123)"),
            ("(1 + 2) * 3", "(* (group (+ 1 2)) 3)"),
        ];
        for (src, expect) in tests {
            let tokens = src.scan().0;
            let expression = Parser::new(&tokens).expression().unwrap();
            assert_eq!(expression.print(), expect);
        }
    }

    #[test]
    fn test_print_literal_expression() {
        let tests: Vec<(&str, &str)> = vec![
            ("true", "true"),
            ("false", "false"),
            ("nil", "nil"),
            ("123.456", "123.456"),
            ("\"hello\"", "\"hello\""),
        ];
        for (src, expect) in tests {
            let tokens = src.scan().0;
            let expression = Parser::new(&tokens).expression().unwrap();
            assert_eq!(expression.print(), expect);
        }
    }

    #[test]
    fn test_print_logical_expression() {
        let tests: Vec<(&str, &str)> = vec![
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
    fn test_print_unary_expression() {
        let tests: Vec<(&str, &str)> = vec![
            ("-123", "(- 123)"),
            ("!123", "(! 123)"),
        ];
        for (src, expect) in tests {
            let tokens = src.scan().0;
            let expression = Parser::new(&tokens).expression().unwrap();
            assert_eq!(expression.print(), expect);
        }
    }

    #[test]
    fn test_print_variable_expression() {
        let tokens = "foo".scan().0;
        let expression = Parser::new(&tokens).expression().unwrap();
        assert_eq!(expression.print(), "foo");
    }

    #[test]
    fn test_print_block_statement() {
        let tests: Vec<(&str, &str)> = vec![
            ("{var foo; var bar = true;}", "{var foo; var bar = true;}"),
            ("{var a; {a = true;}}", "{var a; {(= a true);}}"),
        ];
        for (src, expect) in tests {
            let tokens = src.scan().0;
            let statement = Parser::new(&tokens).statement().unwrap();
            assert_eq!(statement.print(), expect);
        }
    }

    #[test]
    fn test_print_expression_statement() {
        let tests: Vec<(&str, &str)> = vec![
            ("true;", "true;"),
            ("1 + 1;", "(+ 1 1);"),
        ];
        for (src, expect) in tests {
            let tokens = src.scan().0;
            let statement = Parser::new(&tokens).statement().unwrap();
            assert_eq!(statement.print(), expect);
        }
    }

    #[test]
    fn test_print_for_statement() {
        let tests: Vec<(&str, &str)> = vec![
            ("for (;;) print i;", "for (;;) print i;"),
            ("for (var i;;) print i;", "for (var i;;) print i;"),
            ("for (; i < 10;) print i;", "for (; (< i 10);) print i;"),
            ("for (;; i = i + 1) print i;", "for (;; (= i (+ i 1))) print i;"),
            ("for (var i; i < 10; i = i + 1) print i;", "for (var i; (< i 10); (= i (+ i 1))) print i;"),
            ("for (var i; i < 10;) { print i; i = i + 1; }", "for (var i; (< i 10);) {print i; (= i (+ i 1));}"),
        ];
        for (src, expect) in tests {
            let tokens = src.scan().0;
            let statement = Parser::new(&tokens).statement().unwrap();
            assert_eq!(statement.print(), expect);
        }
    }

    #[test]
    fn test_print_fun_declare_statement() {
        let tests: Vec<(&str, &str)> = vec![
            ("fun foo() {print \"hello\";}", "fun foo() {print \"hello\";}"),
            ("fun bar(a, b) {var c = a + b; print c;}", "fun bar(a, b) {var c = (+ a b); print c;}"),
        ];
        for (src, expect) in tests {
            let tokens = src.scan().0;
            let statement = Parser::new(&tokens).statement().unwrap();
            assert_eq!(statement.print(), expect);
        }
    }

    #[test]
    fn test_print_ifelse_statement() {
        let tests: Vec<(&str, &str)> = vec![
            ("if (true) print \"hello\";", "if true print \"hello\";"),
            ("if (1 + 1 == 2) { print 1; } else { print 2; }", "if (== (+ 1 1) 2) {print 1;} else {print 2;}"),
        ];
        for (src, expect) in tests {
            let tokens = src.scan().0;
            let statement = Parser::new(&tokens).statement().unwrap();
            assert_eq!(statement.print(), expect);
        }
    }

    #[test]
    fn test_print_print_statement() {
        let tests: Vec<(&str, &str)> = vec![
            ("print foo;", "print foo;"),
            ("print 1 + 1;", "print (+ 1 1);"),
        ];
        for (src, expect) in tests {
            let tokens = src.scan().0;
            let statement = Parser::new(&tokens).statement().unwrap();
            assert_eq!(statement.print(), expect);
        }
    }

    #[test]
    fn test_print_vardeclare_statement() {
        let tests: Vec<(&str, &str)> = vec![
            ("var foo;", "var foo;"),
            ("var foo = true;", "var foo = true;"),
            ("var foo = 1 + 1;", "var foo = (+ 1 1);"),
        ];
        for (src, expect) in tests {
            let tokens = src.scan().0;
            let statement = Parser::new(&tokens).statement().unwrap();
            assert_eq!(statement.print(), expect);
        }
    }

    #[test]
    fn test_print_while_statement() {
        let tests: Vec<(&str, &str)> = vec![
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
