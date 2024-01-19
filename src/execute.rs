use std::cell::RefCell;
use std::rc::Rc;
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
    new_child_environment,
    environment_declare,
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
    fn execute(&self, env: &Rc<RefCell<Environment>>) -> ExecuteResult;
}

impl Execute for BlockStatement {
    fn execute(&self, env: &Rc<RefCell<Environment>>) -> ExecuteResult {
        let env = new_child_environment(env);
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
    fn execute(&self, _: &Rc<RefCell<Environment>>) -> ExecuteResult {
        return Ok(ExecuteOk::Break);
    }
}

impl Execute for ExpressionStatement {
    fn execute(&self, env: &Rc<RefCell<Environment>>) -> ExecuteResult {
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
    fn execute(&self, env: &Rc<RefCell<Environment>>) -> ExecuteResult {
        let env = new_child_environment(env);

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
    fn execute(&self, env: &Rc<RefCell<Environment>>) -> ExecuteResult {
        if environment_declare(
            env,
            self.name().name(),
            Value::Function(
                Function::new(
                    function_id(),
                    self.name().clone(),
                    self.parameters().clone(),
                    self.body().clone(),
                    env,
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
    fn execute(&self, env: &Rc<RefCell<Environment>>) -> ExecuteResult {
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
        let cenv = new_child_environment(env);
        if let Some(stmt) = statement {
            match stmt.execute(&cenv) {
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
    fn execute(&self, env: &Rc<RefCell<Environment>>) -> ExecuteResult {
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
    fn execute(&self, env: &Rc<RefCell<Environment>>) -> ExecuteResult {
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
    fn execute(&self, env: &Rc<RefCell<Environment>>) -> ExecuteResult {
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
        if environment_declare(env, self.name(), value).is_err() {
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
    fn execute(&self, env: &Rc<RefCell<Environment>>) -> ExecuteResult {
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
    use crate::code::code_point::CodePoint;
    use crate::code::code_span::CodeSpan;
    use crate::parse::Parse;
    use crate::parse::parser::Parser;
    use crate::scan::Scan;
    use crate::value::Value;
    use crate::environment::{
        new_environment,
        environment_has_name,
        environment_get_value,
    };
    use crate::error::{
        RuntimeError,
        RuntimeErrorEnum,
    };
    use crate::execute::ExecuteOk;
    use crate::runtime_error;

    fn code_span(sl: usize, sc: usize, el: usize, ec: usize) -> CodeSpan {
        CodeSpan::new(CodePoint::new(sl, sc), CodePoint::new(el, ec))
    }

    #[test]
    fn test_block() {
        let (tokens, errors) =
            "
            var foo = 1;
            {
                var bar = 2;
                foo = bar;
            }
            "
            .scan();
        assert_eq!(errors.len(), 0);
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        assert_eq!(stmts.len(), 2);
        let env = new_environment();
        assert_eq!(stmts[0].execute(&env).is_ok(), true);
        assert_eq!(stmts[1].execute(&env).unwrap(), ExecuteOk::KeepGoing);
        assert_eq!(environment_get_value(&env, "foo").unwrap(), Value::Number(2.0));
    }

    #[test]
    fn test_block_break() {
        let (tokens, errors) =
            "
            var foo = 1;
            {
                foo = 2;
                break;
                foo = 3;
            }
            "
            .scan();
        assert_eq!(errors.len(), 0);
        let mut parser = Parser::new(&tokens);
        let env = new_environment();
        assert_eq!(
            parser.statement(true).unwrap().execute(&env).is_ok(),
            true
        );
        assert_eq!(
            parser.statement(true).unwrap().execute(&env).unwrap(),
            ExecuteOk::Break
        );
        assert_eq!(
            environment_get_value(&env, "foo").unwrap(),
            Value::Number(2.0),
        );
    }

    #[test]
    fn test_block_return() {
        let (tokens, errors) =
            "
            var foo = 1;
            {
                foo = 2;
                return foo;
                foo = 3;
            }
            "
            .scan();
        assert_eq!(errors.len(), 0);
        let mut parser = Parser::new(&tokens);
        let env = new_environment();
        assert_eq!(
            parser.statement(true).unwrap().execute(&env).is_ok(),
            true
        );
        assert_eq!(
            parser.statement(true).unwrap().execute(&env).unwrap(),
            ExecuteOk::Return(Value::Number(2.0))
        );
        assert_eq!(
            environment_get_value(&env, "foo").unwrap(),
            Value::Number(2.0),
        );
    }

    #[test]
    fn test_block_shadow() {
        let (tokens, errors) =
            "
            var foo = 1;
            {
                var foo = 2;
                foo = 3;
            }
            "
            .scan();
        assert_eq!(errors.len(), 0);
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        let env = new_environment();
        for stmt in stmts {
            assert_eq!(stmt.execute(&env).is_ok(), true);
        }
        assert_eq!(environment_get_value(&env, "foo").unwrap(), Value::Number(1.0));
    }

    #[test]
    fn test_block_execute_error() {
        let (tokens, errors) =
            "
            {
                var foo;
                foo = bar;
            }
            "
            .scan();
        assert_eq!(errors.len(), 0);
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        let env = new_environment();
        assert_eq!(
            stmts[0].execute(&env).unwrap_err(),
            runtime_error!(
                RuntimeErrorEnum::RuntimeError,
                code_span(3, 0, 3, 10),
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    code_span(3, 0, 3, 9),
                    runtime_error!(
                        RuntimeErrorEnum::VariableNotDeclared,
                        code_span(3, 6, 3, 9),
                    )
                )
            ),
        );
    }

    #[test]
    fn test_break() {
        let tokens = "break;".scan().0;
        let mut p = Parser::new(&tokens);
        let stmt = p.statement(true).unwrap();
        let env = new_environment();
        assert_eq!(stmt.execute(&env).unwrap(), ExecuteOk::Break);
    }

    #[test]
    fn test_expression() {
        let env = new_environment();
        let tokens = "true;".scan().0;
        let stmt = &tokens.parse().0[0];
        assert_eq!(
            stmt.execute(&env).unwrap(),
            ExecuteOk::KeepGoing
        );
    }

    #[test]
    fn test_expression_evaluate_error() {
        let env = new_environment();
        let stmt = &"foo;".scan().0.parse().0[0];
        assert_eq!(
            stmt.execute(&env).unwrap_err(),
            runtime_error!(
                RuntimeErrorEnum::RuntimeError,
                code_span(0, 0, 0, 4),
                runtime_error!(
                    RuntimeErrorEnum::VariableNotDeclared,
                    code_span(0, 0, 0, 3),
                )
            )
        );
    }

    #[test]
    fn test_for() {
        let src = "
            var sum = 0;
            for (var i = 1; i <= 10; i = i + 1) {
                sum = sum + i;
            }
        ";
        let env = new_environment();
        let (tokens, errors) = src.scan();
        assert_eq!(errors.len(), 0);
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        for stmt in stmts {
            assert_eq!(stmt.execute(&env).is_ok(), true);
        }
        assert_eq!(environment_get_value(&env, "sum").unwrap(), Value::Number(55.0));
    }

    #[test]
    fn test_for_break() {
        let (tokens, errors) =
            "
            var num = 0;
            for (var i = 0; i < 10; i = i + 1) {
                num = num + 1;
                if (num >= 5) {
                    break;
                }
            }
            "
            .scan();
        assert_eq!(errors.len(), 0);
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        let env = new_environment();
        assert_eq!(stmts[0].execute(&env).unwrap(), ExecuteOk::KeepGoing);
        assert_eq!(stmts[1].execute(&env).unwrap(), ExecuteOk::KeepGoing);
        assert_eq!(environment_get_value(&env, "num").unwrap(), Value::Number(5.0));
    }

    #[test]
    fn test_for_return() {
        let (tokens, errors) =
            "
            var num = 0;
            for (var i = 0; i < 10; i = i + 1) {
                num = num + 1;
                if (num >= 5) {
                    return num;
                }
            }
            "
            .scan();
        assert_eq!(errors.len(), 0);
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        let env = new_environment();
        assert_eq!(stmts[0].execute(&env).unwrap(), ExecuteOk::KeepGoing);
        assert_eq!(stmts[1].execute(&env).unwrap(), ExecuteOk::Return(Value::Number(5.0)));
        assert_eq!(environment_get_value(&env, "num").unwrap(), Value::Number(5.0));
    }

    #[test]
    fn test_for_initializer_execute_error() {
        let (tokens, errors) =
            "
            for (var i = foo; i < 10; i = i + 1) {
                print i;
            }
            "
            .scan();
        assert_eq!(errors.len(), 0);
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        let env = new_environment();
        assert_eq!(
            stmts[0].execute(&env).unwrap_err(),
            runtime_error!(
                RuntimeErrorEnum::RuntimeError,
                code_span(1, 0, 3, 1),
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    code_span(1, 5, 1, 17),
                    runtime_error!(
                        RuntimeErrorEnum::VariableNotDeclared,
                        code_span(1, 13, 1, 16),
                    )
                )
            )
        );
    }

    #[test]
    fn test_for_condition_evaluate_error() {
        let (tokens, errors) =
            "
            for (var i = 0; i < true; i = i + 1) {
                print i;
            }
            "
            .scan();
        assert_eq!(errors.len(), 0);
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        let env = new_environment();
        assert_eq!(
            stmts[0].execute(&env).unwrap_err(),
            runtime_error!(
                RuntimeErrorEnum::RuntimeError,
                code_span(1, 0, 3, 1),
                runtime_error!(
                    RuntimeErrorEnum::InvalidCompare(Value::Number(0.0), Value::Bool(true)),
                    code_span(1, 16, 1, 24),
                )
            )
        );
    }

    #[test]
    fn test_for_body_execute_error() {
        let (tokens, errors) =
            "
            for (var i = 0; i < 10; i = i + 1) {
                print foo;
            }
            "
            .scan();
        assert_eq!(errors.len(), 0);
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        let env = new_environment();
        assert_eq!(
            stmts[0].execute(&env).unwrap_err(),
            runtime_error!(
                RuntimeErrorEnum::RuntimeError,
                code_span(1, 0, 3, 1),
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    code_span(2, 0, 2, 10),
                    runtime_error!(
                        RuntimeErrorEnum::VariableNotDeclared,
                        code_span(2, 6, 2, 9),
                    )
                )
            )
        );
    }

    #[test]
    fn test_for_increment_evaluate_error() {
        let (tokens, errors) =
            "
            for (var i = 0; i < 10; i = j + 1) {
                print i;
            }
            "
            .scan();
        assert_eq!(errors.len(), 0);
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        let env = new_environment();
        assert_eq!(
            stmts[0].execute(&env).unwrap_err(),
            runtime_error!(
                RuntimeErrorEnum::RuntimeError,
                code_span(1, 0, 3, 1),
                runtime_error!(
                    RuntimeErrorEnum::RuntimeError,
                    code_span(1, 24, 1, 33),
                    runtime_error!(
                        RuntimeErrorEnum::RuntimeError,
                        code_span(1, 28, 1, 33),
                        runtime_error!(
                            RuntimeErrorEnum::VariableNotDeclared,
                            code_span(1, 28, 1, 29),
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
        let env = new_environment();
        assert_eq!(stmts[0].execute(&env), Ok(ExecuteOk::KeepGoing));
        let f = if let Value::Function(f) = environment_get_value(&env, "foo").unwrap() {
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
        let env = new_environment();
        assert_eq!(stmts[0].execute(&env), Ok(ExecuteOk::KeepGoing));
        assert_eq!(
            stmts[1].execute(&env),
            Err(
                runtime_error!(
                    RuntimeErrorEnum::MultipleDeclaration,
                    code_span(2, 0, 4, 1),
                )
            )
        );
    }

    #[test]
    fn test_if() {
        let (tokens, errors) =
            "
            var foo = 1;
            if (true) foo = 2;
            "
            .scan();
        assert_eq!(errors.len(), 0);
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        let env = new_environment();
        assert_eq!(stmts[0].execute(&env), Ok(ExecuteOk::KeepGoing));
        assert_eq!(stmts[1].execute(&env), Ok(ExecuteOk::KeepGoing));
        assert_eq!(environment_get_value(&env, "foo").unwrap(), Value::Number(2.0));
    } 

    #[test]
    fn test_if_else() {
        let (tokens, errors) =
            "
            var foo = 1;
            if (false) {
                foo = 2;
            } else {
                foo = 3;
            }
            "
            .scan();
        assert_eq!(errors.len(), 0);
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        let env = new_environment();
        assert_eq!(stmts[0].execute(&env), Ok(ExecuteOk::KeepGoing));
        assert_eq!(stmts[1].execute(&env), Ok(ExecuteOk::KeepGoing));
        assert_eq!(environment_get_value(&env, "foo").unwrap(), Value::Number(3.0));
    }

    #[test]
    fn test_print() {
        let env = new_environment();
        let tokens = "print true;".scan().0;
        let stmt = &tokens.parse().0[0];
        assert_eq!(stmt.execute(&env).is_ok(), true);
    }

    #[test]
    fn test_print_evaluate_error() {
        let env = new_environment();
        let stmt = &"print foo;".scan().0.parse().0[0];
        assert_eq!(
            stmt.execute(&env).unwrap_err(),
            runtime_error!(
                RuntimeErrorEnum::RuntimeError,
                code_span(0, 0, 0, 10),
                runtime_error!(
                    RuntimeErrorEnum::VariableNotDeclared,
                    code_span(0, 6, 0, 9),
                )
            )
        );
    }

    #[test]
    fn test_return() {
        let env = new_environment();
        let stmt = &"return;".scan().0.parse().0[0];
        assert_eq!(
            stmt.execute(&env).unwrap(),
            ExecuteOk::Return(Value::Nil),
        );
    }

    #[test]
    fn test_return_value() {
        let env = new_environment();
        let stmt = &"return true;".scan().0.parse().0[0];
        assert_eq!(
            stmt.execute(&env).unwrap(),
            ExecuteOk::Return(Value::Bool(true)),
        );
    }

    #[test]
    fn test_return_evaluate_error() {
        let env = new_environment();
        let stmt = &"return foo;".scan().0.parse().0[0];
        assert_eq!(
            stmt.execute(&env).unwrap_err(),
            runtime_error!(
                RuntimeErrorEnum::RuntimeError,
                code_span(0, 0, 0, 11),
                runtime_error!(
                    RuntimeErrorEnum::VariableNotDeclared,
                    code_span(0, 7, 0, 10),
                )
            )
        );
    }

    #[test]
    fn test_var_declare() {
        let env = new_environment();
        let tokens = "var foo;".scan().0;
        let stmt = &tokens.parse().0[0];
        assert_eq!(stmt.execute(&env).is_ok(), true);
        assert_eq!(environment_has_name(&env, "foo"), true);
        assert_eq!(environment_get_value(&env, "foo"), Ok(Value::Nil));
    }

    #[test]
    fn test_var_declare_initializer() {
        let env = new_environment();
        let tokens = "var foo = 1 + 1;".scan().0;
        let stmt = &tokens.parse().0[0];
        assert_eq!(stmt.execute(&env).is_ok(), true);
        assert_eq!(environment_has_name(&env, "foo"), true);
        assert_eq!(environment_get_value(&env, "foo"), Ok(Value::Number(2.0)));
    }

    #[test]
    fn test_var_declare_multiple_declare() {
        let env = new_environment();
        let tokens = "var foo;".scan().0;
        let stmt = &tokens.parse().0[0];
        assert_eq!(stmt.execute(&env).is_ok(), true);
        assert_eq!(
            stmt.execute(&env).unwrap_err(),
            runtime_error!(
                RuntimeErrorEnum::MultipleDeclaration,
                code_span(0, 0, 0, 8),
            )
        );
    }

    #[test]
    fn test_var_declare_initializer_evaluate_error() {
        let env = new_environment();
        let tokens = "var foo = bar;".scan().0;
        let stmt = &tokens.parse().0[0];
        assert_eq!(
            stmt.execute(&env).unwrap_err(),
            runtime_error!(
                RuntimeErrorEnum::RuntimeError,
                code_span(0, 0, 0, 14),
                runtime_error!(
                    RuntimeErrorEnum::VariableNotDeclared,
                    code_span(0, 10, 0, 13),
                )
            )
        );
    }

    #[test]
    fn test_while() {
        let (tokens, errors) =
            "
            var sum = 0;
            var i = 3;
            while (i > 0) {
                sum = sum + i;
                i = i - 1;
            }
            "
            .scan();
        assert_eq!(errors.len(), 0);
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        let env = new_environment();
        assert_eq!(stmts[0].execute(&env), Ok(ExecuteOk::KeepGoing));
        assert_eq!(stmts[1].execute(&env), Ok(ExecuteOk::KeepGoing));
        assert_eq!(stmts[2].execute(&env), Ok(ExecuteOk::KeepGoing));
        assert_eq!(environment_get_value(&env, "sum").unwrap(), Value::Number(6.0));
    }

    #[test]
    fn test_while_break() {
        let (tokens, errors) =
            "
            var i = 0;
            while (i <= 3) {
                if (i == 2) {
                    break;
                }
                else {
                    i = i + 1;
                }
            }
            "
            .scan();
        assert_eq!(errors.len(), 0);
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        let env = new_environment();
        assert_eq!(errors.len(), 0);
        assert_eq!(stmts[0].execute(&env), Ok(ExecuteOk::KeepGoing));
        assert_eq!(stmts[1].execute(&env), Ok(ExecuteOk::KeepGoing));
        assert_eq!(environment_get_value(&env, "i").unwrap(), Value::Number(2.0));
    }

    #[test]
    fn test_while_return() {
        let (tokens, errors) =
            "
            var i = 0;
            while (i <= 3) {
                if (i == 2) {
                    return i;
                }
                else {
                    i = i + 1;
                }
            }
            "
            .scan();
        assert_eq!(errors.len(), 0);
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        let env = new_environment();
        assert_eq!(errors.len(), 0);
        assert_eq!(stmts[0].execute(&env), Ok(ExecuteOk::KeepGoing));
        assert_eq!(stmts[1].execute(&env), Ok(ExecuteOk::Return(Value::Number(2.0))));
        assert_eq!(environment_get_value(&env, "i").unwrap(), Value::Number(2.0));
    }
}
