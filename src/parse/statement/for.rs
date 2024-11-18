use std::{
    rc::Rc,
    cell::RefCell,
};
use crate::{
    code::{
        Code,
        code_span::CodeSpan,
    },
    parse::{
        expression::Expression,
        statement::{
            Statement,
            AsStatement,
        }
    },
    environment::{
        Environment,
        EnvironmentT,
    },
    error::RuntimeError,
    execute::{
        Execute,
        ExecuteOk,
    },
    print::Print,
    resolve::{
        ResolveCtx,
        ResolveError,
    },
    impl_debug_for_printable,
};

pub struct ForStatement {
    initializer: Option<Statement>,
    condition: Option<Expression>,
    increment: Option<Expression>,
    body: Statement,
    code_span: CodeSpan,
}

impl ForStatement {
    pub fn new(
        initializer: Option<Statement>,
        condition: Option<Expression>,
        increment: Option<Expression>,
        body: Statement,
        code_span: CodeSpan,
    ) -> ForStatement
    {
        ForStatement {
            initializer,
            condition,
            increment,
            body,
            code_span,
        }
    }

    pub fn initializer(&self) -> Option<&Statement> {
        self.initializer.as_ref()
    }

    pub fn condition(&self) -> Option<&Expression> {
        self.condition.as_ref()
    }

    pub fn increment(&self) -> Option<&Expression> {
        self.increment.as_ref()
    }

    pub fn body(&self) -> &Statement {
        &self.body
    }
}

impl Code for ForStatement {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

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

impl Execute for ForStatement {
    fn execute(&self, env: &Rc<RefCell<Environment>>) -> Result<ExecuteOk, RuntimeError> {
        let env = env.new_child();

        if let Some(initializer) = self.initializer() {
            if let Err(e) = initializer.execute(&env) {
                return Err(RuntimeError::wrap(e, self.code_span()));
            }
        }

        loop {
            if let Some(condition) = self.condition() {
                match condition.evaluate(&env) {
                    Ok(v) => {
                        if !v.is_truthy() {
                            return Ok(ExecuteOk::KeepGoing);
                        }
                    }
                    Err(e) => {
                        return Err(RuntimeError::wrap(e, self.code_span()));
                    }
                }
            }

            match self.body().execute(&env) {
                Ok(ExecuteOk::KeepGoing) => {
                    // donothing.
                }
                Ok(ExecuteOk::Break) => {
                    return Ok(ExecuteOk::KeepGoing);
                }
                Ok(ExecuteOk::Return(v)) => {
                    return Ok(ExecuteOk::Return(v));
                }
                Err(e) => {
                    return Err(RuntimeError::wrap(e, self.code_span()));
                }
            }

            if let Some(increment) = self.increment() {
                if let Err(e) = increment.evaluate(&env) {
                    return Err(RuntimeError::wrap(e, self.code_span()));
                }
            }
        }
    }
}

impl AsStatement for ForStatement {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Statement, ResolveError> {
        context.begin();
        let initializer = if let Some(s) = self.initializer.as_ref() {
            match s.resolve(context) {
                Ok(s) => Some(s),
                Err(e) => {
                    context.end();
                    return Err(e);
                }
            }
        }
        else {
            None
        };
        let condition = if let Some(e) = self.condition.as_ref() {
            match e.resolve(context) {
                Ok(e) => Some(e),
                Err(e) => {
                    context.end();
                    return Err(e);
                }
            }
        }
        else {
            None
        };
        let increment = if let Some(e) = self.increment.as_ref() {
            match e.resolve(context) {
                Ok(e) => Some(e),
                Err(e) => {
                    context.end();
                    return Err(e);
                }
            }
        }
        else {
            None
        };
        let body = match self.body.resolve(context) {
            Ok(s) => s,
            Err(e) => {
                context.end();
                return Err(e);
            }
        };
        context.end();
        Ok(
            Statement(
                Rc::new(
                    ForStatement::new(
                        initializer,
                        condition,
                        increment,
                        body,
                        self.code_span.clone(),
                    )
                )
            )
        )
    }
}

#[macro_export]
macro_rules! for_statement {
    (
        $initializer:expr,
        $condition:expr,
        $increment:expr,
        $body:expr,
        $code_span:expr,
    ) => {
        Statement(
            Rc::new(
                ForStatement::new(
                    $initializer,
                    $condition,
                    $increment,
                    $body,
                    $code_span,
                )
            )
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        code::code_span::new_code_span,
        parse::{
            expression::{
                assign::AssignExpression,
                binary::BinaryExpression,
                variable::VariableExpression
            },
            statement::{
                block::BlockStatement,
                expression::ExpressionStatement,
                r#for::ForStatement,
                var_declare::VarDeclareStatement,
            },
        },
        value::Value,
        environment::EnvironmentT,
        error::{
            RuntimeError,
            RuntimeErrorEnum,
        },
        execute::ExecuteOk,
        print::Print,
        resolve::{
            ResolveError,
            ResolveErrorEnum,
        },
        utils::test_utils::{
            TestContext,
            parse_statement,
            parse_statement_unknown,
        }
    };

    #[test]
    fn test_for_statement_print() {
        let tests: Vec<(&str, &str)> = vec![
            ("for (;;) { print i; }", "for (;;) {print i;}"),
            ("for (var i;;) { print i; }", "for (var i;;) {print i;}"),
            ("for (; i < 10;) { print i; }", "for (; (< i 10);) {print i;}"),
            ("for (;; i = i + 1) { print i; }", "for (;; (= i (+ i 1))) {print i;}"),
            ("for (var i; i < 10; i = i + 1) { print i; }", "for (var i; (< i 10); (= i (+ i 1))) {print i;}"),
            ("for (var i; i < 10;) { print i; i = i + 1; }", "for (var i; (< i 10);) {print i; (= i (+ i 1));}"),
        ];
        for (src, expect) in tests {
            assert_eq!(parse_statement::<ForStatement>(src).print(), expect);
        }
    }

    #[test]
    fn test_for_statement_execute_keep_going() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var sum = 0;");
        assert_eq!(
            ctx.execute(
                parse_statement::<ForStatement>(
                    "
                    for (var i = 1; i <= 10; i = i + 1) {
                        sum = sum + i;
                    }
                    "
                )
                    .as_ref()
            ),
            Ok(ExecuteOk::KeepGoing)
        );
        assert_eq!(
            ctx.environment.get("sum", 0),
            Some(Value::Number(55.0))
        );
    }

    #[test]
    fn test_for_statement_execute_break() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var num = 0;");
        assert_eq!(
            ctx.execute(
                parse_statement::<ForStatement>(
                    "
                    for (var i = 0; i < 10; i = i + 1) {
                        num = num + 1;
                        if (num >= 5) {
                            break;
                        }
                    }
                    "
                )
                    .as_ref()
            ),
            Ok(ExecuteOk::KeepGoing)
        );
        assert_eq!(
            ctx.environment.get("num", 0).unwrap(),
            Value::Number(5.0),
        );
    }

    #[test]
    fn test_for_statement_execute_return() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var num = 0;");
        assert_eq!(
            ctx.execute(
                parse_statement::<ForStatement>(
                    "
                    for (var i = 0; i < 10; i = i + 1) {
                        num = num + 1;
                        if (num >= 5) {
                            return num;
                        }
                    }
                    "
                )
                    .as_ref()
            ),
            Ok(ExecuteOk::Return(Value::Number(5.0)))
        );
    }

    #[test]
    fn test_for_statement_execute_initializer_execute_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.execute(
                parse_statement::<ForStatement>(
                    "
                    for (var i = true + 1; i < 10; i = i + 1) {
                        print i;
                    }
                    "
                )
                    .as_ref()
            ),
            Err(
                RuntimeError::wrap(
                    RuntimeError::wrap(
                        RuntimeError::new(
                            RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                            new_code_span(1, 13, 1, 21),
                        ),
                        new_code_span(1, 5, 1, 22),
                    ),
                    new_code_span(1, 0, 3, 1),
                )
            )
        );
    }

    #[test]
    fn test_for_statement_execute_condition_evaluate_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.execute(
                parse_statement::<ForStatement>(
                    "
                    for (var i = 0; i < true; i = i + 1) {
                        print i;
                    }
                    "
                )
                    .as_ref()
            ),
            Err(
                RuntimeError::wrap(
                    RuntimeError::new(
                        RuntimeErrorEnum::InvalidCompare(Value::Number(0.0), Value::Bool(true)),
                        new_code_span(1, 16, 1, 24),
                    ),
                    new_code_span(1, 0, 3, 1),
                )
            )
        );
    }

    #[test]
    fn test_for_statement_execute_body_execute_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.execute(
                parse_statement::<ForStatement>(
                    "
                    for (var i = 0; i < 10; i = i + 1) {
                        print true + 1;
                    }
                    "
                )
                    .as_ref()
            ),
            Err(
                RuntimeError::wrap(
                    RuntimeError::wrap(
                        RuntimeError::new(
                            RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                            new_code_span(2, 6, 2, 14),
                        ),
                        new_code_span(2, 0, 2, 15),
                    ),
                    new_code_span(1, 0, 3, 1),
                )
            )
        );
    }

    #[test]
    fn test_for_statement_execute_increment_evaluate_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.execute(
                parse_statement::<ForStatement>(
                    "
                    for (var i = 0; i < 10; i = true + 1) {
                        print i;
                    }
                    "
                )
                    .as_ref()
            ),
            Err(
                RuntimeError::wrap(
                    RuntimeError::wrap(
                        RuntimeError::new(
                            RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                            new_code_span(1, 28, 1, 36),
                        ),
                        new_code_span(1, 24, 1, 36),
                    ),
                    new_code_span(1, 0, 3, 1),
                )
            )
        );
    }

    #[test]
    fn test_for_statement_resolve() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var j = 0;");

        let for_stmt = ctx.resolve_statement::<ForStatement>(
            parse_statement_unknown("for (var i = j; i < 10; i = i + 1) { j = i; }").as_ref()
        )
            .unwrap();

        let init_stmt = for_stmt.initializer().unwrap().downcast_ref::<VarDeclareStatement>().unwrap();
        let var_expr = init_stmt.initializer().unwrap().downcast_ref::<VariableExpression>().unwrap();
        assert_eq!(var_expr.binding(), 1);

        let cond_expr = for_stmt.condition().unwrap().downcast_ref::<BinaryExpression>().unwrap();
        let lhs_expr = cond_expr.lhs().downcast_ref::<VariableExpression>().unwrap();
        assert_eq!(lhs_expr.binding(), 0);

        let assign_expr = for_stmt.increment().unwrap().downcast_ref::<AssignExpression>().unwrap();
        assert_eq!(assign_expr.binding(), 0);
        let bin_expr = assign_expr.value().downcast_ref::<BinaryExpression>().unwrap();
        let lhs_expr = bin_expr.lhs().downcast_ref::<VariableExpression>().unwrap();
        assert_eq!(lhs_expr.binding(), 0);

        let body_stmt = for_stmt.body().downcast_ref::<BlockStatement>().unwrap();
        let expr_stmt = body_stmt.statements()[0].downcast_ref::<ExpressionStatement>().unwrap();
        let assign_expr = expr_stmt.expression().downcast_ref::<AssignExpression>().unwrap();
        assert_eq!(assign_expr.binding(), 2);

        let val_expr = assign_expr.value().downcast_ref::<VariableExpression>().unwrap();
        assert_eq!(val_expr.binding(), 1);
    }

    #[test]
    fn test_for_statement_resolve_initializer_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<ForStatement>("for (var i = j; i < 10; i = i + 1) { j = i; }").as_ref()
            )
                .unwrap_err(),
            ResolveError::new(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 13, 0, 14)
            )
        );
    }

    #[test]
    fn test_for_statement_resolve_condition_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<ForStatement>("for (var i = 1; j < 10; i = i + 1) { j = i; }").as_ref()
            )
                .unwrap_err(),
            ResolveError::new(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 16, 0, 17)
            )
        );
    }

    #[test]
    fn test_for_statement_resolve_increment_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<ForStatement>("for (var i = 1; i < 10; i = j + 1) { j = i; }").as_ref()
            )
                .unwrap_err(),
            ResolveError::new(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 28, 0, 29)
            )
        );
    }

    #[test]
    fn test_for_statement_resolve_body_resolve_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.resolve_statement_unknown(
                parse_statement::<ForStatement>("for (var i = 1; i < 10; i = i + 1) { j = i; }").as_ref()
            )
                .unwrap_err(),
            ResolveError::new(
                ResolveErrorEnum::VariableNotDeclared,
                new_code_span(0, 37, 0, 38)
            )
        );
    }
}
