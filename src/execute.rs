use crate::code::Code;
use crate::parse::statement::block::BlockStatement;
use crate::parse::statement::r#break::BreakStatement;
use crate::parse::statement::expression::ExpressionStatement;
use crate::parse::statement::r#for::ForStatement;
use crate::parse::statement::fun_declare::FunDeclareStatement;
use crate::parse::statement::ifelse::IfStatement;
use crate::parse::statement::print::PrintStatement;
use crate::parse::statement::r#return::ReturnStatement;
use crate::parse::statement::var_declare::VarDeclareStatement;
use crate::parse::statement::r#while::WhileStatement;
use crate::value::Value;
use crate::value::function::{
    Function,
    function_id,
};
use crate::environment::{
    Environment,
    EnvironmentOps,
};
use crate::error::{
    RuntimeError,
    RuntimeErrorEnum,
};
use crate::runtime_error;

#[derive(PartialEq, Debug)]
pub enum ExecuteOk {
    KeepGoing,
    Break,
    Return(Value),
}

pub type ExecuteResult = std::result::Result<ExecuteOk, RuntimeError>;

pub trait Execute {
    fn execute(&self, env: &Environment) -> ExecuteResult;
}

impl Execute for BlockStatement {
    fn execute(&self, env: &Environment) -> ExecuteResult {
        let env = env.new_child();
        for statement in self.statements() {
            let ok = statement.execute(&env)?;
            match ok {
                ExecuteOk::KeepGoing => {
                    // do nothing.
                }
                ExecuteOk::Break => {
                    return Ok(ExecuteOk::Break);
                }
                ExecuteOk::Return(v) => {
                    return Ok(ExecuteOk::Return(v));
                }
            }
        }
        return Ok(ExecuteOk::KeepGoing);
    }
}

impl Execute for BreakStatement {
    fn execute(&self, _: &Environment) -> ExecuteResult {
        return Ok(ExecuteOk::Break);
    }
}

impl Execute for ExpressionStatement {
    fn execute(&self, env: &Environment) -> ExecuteResult {
        if let Err(e) = self.expression().evaluate(env) {
            return Err(
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    self.code_span(),
                    e
                )
            );
        }
        else {
            return Ok(ExecuteOk::KeepGoing);
        }
    }
}

impl Execute for ForStatement {
    fn execute(&self, env: &Environment) -> ExecuteResult {
        let env = env.new_child();

        if let Some(initializer) = self.initializer() {
            if let Err(e) = initializer.execute(&env) {
                return Err(
                    runtime_error!(
                        RuntimeErrorEnum::RuntimeError,
                        self.code_span(),
                        e
                    )
                );
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
                        return Err(
                            runtime_error!(
                                RuntimeErrorEnum::RuntimeError,
                                self.code_span(),
                                e
                            )
                        );
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
                    return Err(
                        runtime_error!(
                            RuntimeErrorEnum::RuntimeError,
                            self.code_span(),
                            e
                        )
                    );
                }
            }

            if let Some(increment) = self.increment() {
                if let Err(e) = increment.evaluate(&env) {
                    return Err(
                        runtime_error!(
                            RuntimeErrorEnum::RuntimeError,
                            self.code_span(),
                            e
                        )
                    );
                }
            }
        }
    }
}

impl Execute for FunDeclareStatement {
    fn execute(&self, env: &Environment) -> ExecuteResult {
        if env.declare(
            self.name().name(),
            Value::Function(
                Function::new(
                    function_id(),
                    self.name().clone(),
                    self.parameters().clone(),
                    self.body().clone(),
                    env.clone(),
                )
            )
        )
            .is_err()
        {
            return Err(
                runtime_error!(
                    RuntimeErrorEnum::MultipleDeclaration,
                    self.code_span(),
                )
            );
        }
        else {
            return Ok(ExecuteOk::KeepGoing);
        }
    }
}

impl Execute for IfStatement {
    fn execute(&self, env: &Environment) -> ExecuteResult {
        let condition = match self.condition().evaluate(env) {
            Ok(val) => val.is_truthy(),
            Err(err) => {
                return Err(
                    runtime_error!(
                        RuntimeErrorEnum::RuntimeError,
                        self.code_span(),
                        err
                    )
                );
            }
        };
        let statement = if condition {
            Some(self.then_statement())
        }
        else {
            self.else_statement()
        };
        if let Some(stmt) = statement {
            match stmt.execute(env) {
                Err(err) => {
                    return Err(
                        runtime_error!(
                            RuntimeErrorEnum::RuntimeError,
                            self.code_span(),
                            err
                        )
                    );
                }
                Ok(ok) => {
                    return Ok(ok);
                }
            }
        }
        return Ok(ExecuteOk::KeepGoing);
    }
}

impl Execute for PrintStatement {
    fn execute(&self, env: &Environment) -> ExecuteResult {
        match self.value().evaluate(env) {
            Ok(v) => {
                println!("{}", v);
                return Ok(ExecuteOk::KeepGoing);
            }
            Err(e) => {
                return Err(
                    runtime_error!(
                        RuntimeErrorEnum::RuntimeError,
                        self.code_span(),
                        e
                    )
                );
            }
        }
    }
}

impl Execute for ReturnStatement {
    fn execute(&self, env: &Environment) -> ExecuteResult {
        if let Some(e) = self.expression() {
            match e.evaluate(env) {
                Ok(v) => {
                    return Ok(ExecuteOk::Return(v));
                }
                Err(e) => {
                    return Err(
                        runtime_error!(
                            RuntimeErrorEnum::RuntimeError,
                            self.code_span(),
                            e
                        )
                    );
                }
            }
        }
        else {
            return Ok(ExecuteOk::Return(Value::Nil));
        }
    }
}

impl Execute for VarDeclareStatement {
    fn execute(&self, env: &Environment) -> ExecuteResult {
        let mut value = Value::Nil;
        if let Some(i) = self.initializer() {
            match i.evaluate(env) {
                Ok(v) => value = v,
                Err(e) => {
                    return Err(
                        runtime_error!(
                            RuntimeErrorEnum::RuntimeError,
                            self.code_span(),
                            e
                        )
                    );
                }
            }
        };
        if env.declare(self.name().name(), value).is_err() {
            return Err(
                runtime_error!(
                    RuntimeErrorEnum::MultipleDeclaration,
                    self.code_span(),
                )
            );
        }
        return Ok(ExecuteOk::KeepGoing);
    }
}

impl Execute for WhileStatement {
    fn execute(&self, env: &Environment) -> ExecuteResult {
        loop {
            if let Some(condition) = self.condition() {
                match condition.evaluate(env) {
                    Ok(v) => {
                        if !v.is_truthy() {
                            return Ok(ExecuteOk::KeepGoing);
                        }
                    }
                    Err(e) => {
                        return Err(
                            runtime_error!(
                                RuntimeErrorEnum::RuntimeError,
                                self.code_span(),
                                e
                            )
                        );
                    }
                }
            }

            match self.body().execute(env) {
                Ok(ExecuteOk::KeepGoing) => {
                    // do nothing.
                }
                Ok(ExecuteOk::Break) => {
                    return Ok(ExecuteOk::KeepGoing);
                }
                Ok(ExecuteOk::Return(v)) => {
                    return Ok(ExecuteOk::Return(v));
                }
                Err(err) => {
                    return Err(
                        runtime_error!(
                            RuntimeErrorEnum::RuntimeError,
                            self.code_span(),
                            err
                        )
                    );
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::code::code_span::new_code_span;
    use crate::parse::{
        Parse,
        statement::{
            block::BlockStatement,
            r#break::BreakStatement,
            expression::ExpressionStatement,
            r#for::ForStatement,
            ifelse::IfStatement,
            print::PrintStatement,
            r#return::ReturnStatement,
            var_declare::VarDeclareStatement,
            r#while::WhileStatement,
        }
    };
    use crate::scan::Scan;
    use crate::value::Value;
    use crate::environment::{
        Environment,
        EnvironmentOps,
    };
    use crate::error::{
        RuntimeError,
        RuntimeErrorEnum,
    };
    use crate::execute::ExecuteOk;
    use crate::utils::test_utils::{
        TestContext,
        parse_statement,
    };
    use crate::runtime_error;

    #[test]
    fn test_block() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo = 1;");
        assert_eq!(
            ctx.execute(
                parse_statement::<BlockStatement>(
                    "
                    {
                        var bar = 2;
                        foo = bar;
                    }
                    "
                )
                    .as_ref()
            ),
            Ok(ExecuteOk::KeepGoing)
        );
        assert_eq!(
            ctx.environment.get("foo", 0),
            Some(Value::Number(2.0)),
        );
    }

    #[test]
    fn test_block_break() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo = 1;");
        assert_eq!(
            ctx.execute(
                parse_statement::<BlockStatement>(
                    "
                    {
                        foo = 2;
                        break;
                        foo = 3;
                    }
                    "
                )
                    .as_ref()
            ),
            Ok(ExecuteOk::Break)
        );
        assert_eq!(
            ctx.environment.get("foo", 0),
            Some(Value::Number(2.0))
        );
    }

    #[test]
    fn test_block_return() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo = 1;");
        assert_eq!(
            ctx.execute(
                parse_statement::<BlockStatement>(
                    "
                    {
                        foo = 2;
                        return foo;
                        foo = 3;
                    }
                    "
                )
                    .as_ref()
            ),
            Ok(ExecuteOk::Return(Value::Number(2.0)))
        );
        assert_eq!(
            ctx.environment.get("foo", 0),
            Some(Value::Number(2.0))
        );
    }

    #[test]
    fn test_block_shadow() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo = 1;");
        assert_eq!(
            ctx.execute(
                parse_statement::<BlockStatement>(
                    "
                    {
                        var foo = 2;
                        foo = 3;
                    }
                    "
                )
                    .as_ref()
            ),
            Ok(ExecuteOk::KeepGoing)
        );
        assert_eq!(
            ctx.environment.get("foo", 0),
            Some(Value::Number(1.0))
        );
    }

    #[test]
    fn test_block_execute_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.execute(
                parse_statement::<BlockStatement>(
                    "
                    {
                        var foo;
                        foo = true + 1;
                    }
                    "
                )
                    .as_ref()
            ),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    new_code_span(3, 0, 3, 15),
                    runtime_error!(
                        RuntimeErrorEnum::RuntimeError,
                        new_code_span(3, 0, 3, 14),
                        runtime_error!(
                            RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                            new_code_span(3, 6, 3, 14),
                        )
                    )
                )
            )
        );
    }

    #[test]
    fn test_break() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.execute(parse_statement::<BreakStatement>("break;").as_ref()),
            Ok(ExecuteOk::Break)
        );
    }

    #[test]
    fn test_expression() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.execute(parse_statement::<ExpressionStatement>("true;").as_ref()),
            Ok(ExecuteOk::KeepGoing)
        );
    }

    #[test]
    fn test_expression_evaluate_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.execute(
                parse_statement::<ExpressionStatement>("true + 1;").as_ref()
            ),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    new_code_span(0, 0, 0, 9),
                    runtime_error!(
                        RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                        new_code_span(0, 0, 0, 8),
                    )
                )
            )
        );
    }

    #[test]
    fn test_for() {
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
    fn test_for_break() {
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
    fn test_for_return() {
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
    fn test_for_initializer_execute_error() {
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
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    new_code_span(1, 0, 3, 1),
                    runtime_error!(
                        RuntimeErrorEnum::RuntimeError,
                        new_code_span(1, 5, 1, 22),
                        runtime_error!(
                            RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                            new_code_span(1, 13, 1, 21),
                        )
                    )
                )
            )
        );
    }

    #[test]
    fn test_for_condition_evaluate_error() {
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
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    new_code_span(1, 0, 3, 1),
                    runtime_error!(
                        RuntimeErrorEnum::InvalidCompare(Value::Number(0.0), Value::Bool(true)),
                        new_code_span(1, 16, 1, 24),
                    )
                )
            )
        );
    }

    #[test]
    fn test_for_body_execute_error() {
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
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    new_code_span(1, 0, 3, 1),
                    runtime_error!(
                        RuntimeErrorEnum::RuntimeError,
                        new_code_span(2, 0, 2, 15),
                        runtime_error!(
                            RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                            new_code_span(2, 6, 2, 14),
                        )
                    )
                )
            )
        );
    }

    #[test]
    fn test_for_increment_evaluate_error() {
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
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    new_code_span(1, 0, 3, 1),
                    runtime_error!(
                        RuntimeErrorEnum::RuntimeError,
                        new_code_span(1, 24, 1, 36),
                        runtime_error!(
                            RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                            new_code_span(1, 28, 1, 36),
                        )
                    )
                )
            )
        );
    }

    #[test]
    fn test_fun_declare() {
        let (tokens, errors) =
            "
            fun foo(a, b) {
                var c = a + b;
                print c;
            }
            "
            .scan();
        assert_eq!(errors.len(), 0);
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        let env = <Environment as EnvironmentOps>::new();
        assert_eq!(stmts[0].execute(&env), Ok(ExecuteOk::KeepGoing));
        let f = if let Value::Function(f) = env.get("foo", 0).unwrap() {
            f
        }
        else {
            panic!("Value should be function.");
        };
        assert_eq!(f.name(), "foo");
        let params = f.parameters();
        assert_eq!(params.len(), 2);
        assert_eq!(params[0].name(), "a");
        assert_eq!(params[1].name(), "b");
        let body = f.body();
        assert_eq!(body.len(), 2);
        assert_eq!(body[0].print(), "var c = (+ a b);");
        assert_eq!(body[1].print(), "print c;");
    }

    #[test]
    fn test_fun_declare_multiple_declaration_error() {
        let (tokens, errors) =
            "
            var foo = true;
            fun foo(a, b) {
                print \"foo\";
            }
            "
            .scan();
        assert_eq!(errors.len(), 0);
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        assert_eq!(stmts.len(), 2);
        let env = <Environment as EnvironmentOps>::new();
        assert_eq!(stmts[0].execute(&env), Ok(ExecuteOk::KeepGoing));
        assert_eq!(
            stmts[1].execute(&env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::MultipleDeclaration,
                    new_code_span(2, 0, 4, 1),
                )
            )
        );
    }

    #[test]
    fn test_if() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo = 1;");
        assert_eq!(
            ctx.execute(parse_statement::<IfStatement>("if (true) foo = 2;").as_ref()),
            Ok(ExecuteOk::KeepGoing)
        );
        assert_eq!(
            ctx.environment.get("foo", 0).unwrap(),
            Value::Number(2.0)
        );
    } 

    #[test]
    fn test_if_else() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var foo = 1;");
        assert_eq!(
            ctx.execute(
                parse_statement::<IfStatement>(
                    "
                    if (false) {
                        foo = 2;
                    } else {
                        foo = 3;
                    }
                    "
                )
                    .as_ref()
            ),
            Ok(ExecuteOk::KeepGoing)
        );
        assert_eq!(
            ctx.environment.get("foo", 0).unwrap(),
            Value::Number(3.0)
        );
    }

    #[test]
    fn test_print() {
        let env = <Environment as EnvironmentOps>::new();
        let tokens = "print true;".scan().0;
        let stmt = &tokens.parse().0[0];
        assert_eq!(stmt.execute(&env).is_ok(), true);
    }

    #[test]
    fn test_print_execute_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.execute(parse_statement::<PrintStatement>("print true + 1;").as_ref()),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    new_code_span(0, 0, 0, 15),
                    runtime_error!(
                        RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                        new_code_span(0, 6, 0, 14),
                    )
                )
            )
        );
    }

    #[test]
    fn test_return() {
        let env = <Environment as EnvironmentOps>::new();
        let stmt = &"return;".scan().0.parse().0[0];
        assert_eq!(
            stmt.execute(&env).unwrap(),
            ExecuteOk::Return(Value::Nil),
        );
    }

    #[test]
    fn test_return_value() {
        let env = <Environment as EnvironmentOps>::new();
        let stmt = &"return true;".scan().0.parse().0[0];
        assert_eq!(
            stmt.execute(&env).unwrap(),
            ExecuteOk::Return(Value::Bool(true)),
        );
    }

    #[test]
    fn test_return_execute_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.execute(parse_statement::<ReturnStatement>("return true + 1;").as_ref()),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    new_code_span(0, 0, 0, 16),
                    runtime_error!(
                        RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                        new_code_span(0, 7, 0, 15),
                    )
                )
            )
        );
    }

    #[test]
    fn test_var_declare() {
        let env = <Environment as EnvironmentOps>::new();
        let tokens = "var foo;".scan().0;
        let stmt = &tokens.parse().0[0];
        assert_eq!(stmt.execute(&env).is_ok(), true);
        assert_eq!(env.has("foo", 0), true);
        assert_eq!(env.get("foo", 0), Some(Value::Nil));
    }

    #[test]
    fn test_var_declare_initializer() {
        let env = <Environment as EnvironmentOps>::new();
        let tokens = "var foo = 1 + 1;".scan().0;
        let stmt = &tokens.parse().0[0];
        assert_eq!(stmt.execute(&env).is_ok(), true);
        assert_eq!(env.has("foo", 0), true);
        assert_eq!(env.get("foo", 0), Some(Value::Number(2.0)));
    }

    #[test]
    fn test_var_declare_multiple_declare() {
        let env = <Environment as EnvironmentOps>::new();
        let tokens = "var foo;".scan().0;
        let stmt = &tokens.parse().0[0];
        assert_eq!(stmt.execute(&env).is_ok(), true);
        assert_eq!(
            stmt.execute(&env).unwrap_err(),
            runtime_error!(
                RuntimeErrorEnum::MultipleDeclaration,
                new_code_span(0, 0, 0, 8),
            )
        );
    }

    #[test]
    fn test_var_declare_initializer_evaluate_error() {
        let mut ctx = TestContext::new();
        assert_eq!(
            ctx.execute(parse_statement::<VarDeclareStatement>("var foo = true + 1;").as_ref()),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    new_code_span(0, 0, 0, 19),
                    runtime_error!(
                        RuntimeErrorEnum::InvalidArithmetic(Value::Bool(true), Value::Number(1.0)),
                        new_code_span(0, 10, 0, 18),
                    )
                )
            )
        );
    }

    #[test]
    fn test_while() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var sum = 0;");
        ctx.execute_src("var i = 3;");
        assert_eq!(
            ctx.execute(
                parse_statement::<WhileStatement>(
                    "
                    while (i > 0) {
                        sum = sum + i;
                        i = i - 1;
                    }
                    "
                )
                    .as_ref()
            ),
            Ok(ExecuteOk::KeepGoing),
        );
        assert_eq!(
            ctx.environment.get("sum", 0).unwrap(),
            Value::Number(6.0),
        );
    }

    #[test]
    fn test_while_break() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var i = 0;");
        assert_eq!(
            ctx.execute(
                parse_statement::<WhileStatement>(
                    "
                    while (i <= 3) {
                        if (i == 2) {
                            break;
                        }
                        else {
                            i = i + 1;
                        }
                    }
                    "
                )
                    .as_ref()
            ),
            Ok(ExecuteOk::KeepGoing),
        );
        assert_eq!(
            ctx.environment.get("i", 0).unwrap(),
            Value::Number(2.0),
        );
    }

    #[test]
    fn test_while_return() {
        let mut ctx = TestContext::new();
        ctx.execute_src("var i = 0;");
        assert_eq!(
            ctx.execute(
                parse_statement::<WhileStatement>(
                    "
                    while (i <= 3) {
                        if (i == 2) {
                            return i;
                        }
                        else {
                            i = i + 1;
                        }
                    }
                    "
                )
                    .as_ref()
            ),
            Ok(ExecuteOk::Return(Value::Number(2.0))),
        );
        assert_eq!(
            ctx.environment.get("i", 0).unwrap(),
            Value::Number(2.0),
        );
    }
}
