use std::cell::RefCell;
use std::rc::Rc;
use crate::code::Code;
use crate::code::code_span::CodeSpan;
use crate::evaluate::value::value::Value;
use crate::evaluate::evaluate::EvaluateError;
use crate::parse::statement::block::BlockStatement;
use crate::parse::statement::expression::ExpressionStatement;
use crate::parse::statement::r#for::ForStatement;
use crate::parse::statement::ifelse::IfStatement;
use crate::parse::statement::print::PrintStatement;
use crate::parse::statement::var_declare::VarDeclareStatement;
use crate::parse::statement::r#while::WhileStatement;
use crate::scope::Scope;

#[derive(PartialEq, Debug)]
pub enum ExecuteOk {
    KeepGoing,
    Break,
    Return(Value),
}

#[derive(Debug, PartialEq)]
pub enum ExecuteError {
    EvaluateError(EvaluateError),
    MultipleDeclaration(CodeSpan),
}

pub type ExecuteResult = std::result::Result<ExecuteOk, ExecuteError>;

pub trait Execute {
    fn execute(&self, scope: &Rc<RefCell<Scope>>) -> ExecuteResult;
}

impl Execute for BlockStatement {
    fn execute(&self, scope: &Rc<RefCell<Scope>>) -> ExecuteResult {
        let scope = Scope::new_child(scope).as_rc();
        for statement in self.statements() {
            let ok = statement.execute(&scope)?;
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

impl Execute for ExpressionStatement {
    fn execute(&self, scope: &Rc<RefCell<Scope>>) -> ExecuteResult {
        if let Err(e) = self.expression().evaluate(scope) {
            return Err(ExecuteError::EvaluateError(e));
        }
        else {
            return Ok(ExecuteOk::KeepGoing);
        }
    }
}

impl Execute for ForStatement {
    fn execute(&self, scope: &Rc<RefCell<Scope>>) -> ExecuteResult {
        let scope = Scope::new_child(scope).as_rc();

        if let Some(initializer) = self.initializer() {
            initializer.execute(&scope)?;
        }

        loop {
            if let Some(condition) = self.condition() {
                match condition.evaluate(&scope) {
                    Ok(v) => {
                        if !v.is_truthy() {
                            return Ok(ExecuteOk::KeepGoing);
                        }
                    }
                    Err(e) => {
                        return Err(ExecuteError::EvaluateError(e));
                    }
                }
            }

            let ok = self.body().execute(&scope)?;
            match ok {
                ExecuteOk::KeepGoing => {
                    // do nothing.
                }
                ExecuteOk::Break => {
                    return Ok(ExecuteOk::KeepGoing);
                }
                ExecuteOk::Return(v) => {
                    return Ok(ExecuteOk::Return(v));
                }
            }

            if let Some(increment) = self.increment() {
                if let Err(e) = increment.evaluate(&scope) {
                    return Err(ExecuteError::EvaluateError(e));
                }
            }
        }
    }
}

impl Execute for IfStatement {
    fn execute(&self, scope: &Rc<RefCell<Scope>>) -> ExecuteResult {
        let condition = self.condition().evaluate(scope)
            .map(|v| v.is_truthy())
            .map_err(|e| ExecuteError::EvaluateError(e))?;
        let scope = Scope::new_child(scope).as_rc();
        if condition {
            return self.then_statement().execute(&scope);
        }
        else if let Some(else_statement) = self.else_statement() {
            return else_statement.execute(&scope);
        }
        return Ok(ExecuteOk::KeepGoing);
    }
}

impl Execute for PrintStatement {
    fn execute(&self, scope: &Rc<RefCell<Scope>>) -> ExecuteResult {
        match self.value().evaluate(scope) {
            Ok(v) => {
                println!("{}", v);
                return Ok(ExecuteOk::KeepGoing);
            }
            Err(e) => {
                return Err(ExecuteError::EvaluateError(e));
            }
        }
    }
}

impl Execute for VarDeclareStatement {
    fn execute(&self, scope: &Rc<RefCell<Scope>>) -> ExecuteResult {
        let mut value = Value::Nil;
        if let Some(i) = self.initializer() {
            match i.evaluate(scope) {
                Ok(v) => value = v,
                Err(e) => {
                    return Err(ExecuteError::EvaluateError(e));
                }
            }
        };
        if scope.borrow_mut().declare(self.name(), value).is_err() {
            return Err(ExecuteError::MultipleDeclaration(self.code_span()));
        }
        return Ok(ExecuteOk::KeepGoing);
    }
}

impl Execute for WhileStatement {
    fn execute(&self, scope: &Rc<RefCell<Scope>>) -> ExecuteResult {
        loop {
            if let Some(condition) = self.condition() {
                match condition.evaluate(scope) {
                    Ok(v) => {
                        if !v.is_truthy() {
                            return Ok(ExecuteOk::KeepGoing);
                        }
                    }
                    Err(e) => {
                        return Err(ExecuteError::EvaluateError(e));
                    }
                }
            }

            let ok = self.body().execute(scope)?;
            match ok {
                ExecuteOk::KeepGoing => {
                    // do nothing
                }
                ExecuteOk::Break => {
                    return Ok(ExecuteOk::KeepGoing);
                }
                ExecuteOk::Return(v) => {
                    return Ok(ExecuteOk::Return(v));
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::evaluate::value::value::Value;
    use crate::scope::Scope;
    use crate::code::code_point::CodePoint;
    use crate::code::code_span::CodeSpan;
    use crate::scan::Scan;
    use crate::parse::Parse;
    use super::{
        ExecuteError,
        EvaluateError,
    };

    fn code_span(sl: usize, sc: usize, el: usize, ec: usize) -> CodeSpan {
        CodeSpan::new(CodePoint::new(sl, sc), CodePoint::new(el, ec))
    }

    #[test]
    fn test_var_declare() {
        let scope = Scope::new().as_rc();
        let tokens = "var foo;".scan().0;
        let stmt = &tokens.parse().0[0];
        assert_eq!(stmt.execute(&scope).is_ok(), true);
        assert_eq!(scope.borrow().has_name("foo"), true);
        assert_eq!(scope.borrow().get_value("foo"), Ok(Value::Nil));
    }

    #[test]
    fn test_var_declare_initializer() {
        let scope = Scope::new().as_rc();
        let tokens = "var foo = 1 + 1;".scan().0;
        let stmt = &tokens.parse().0[0];
        assert_eq!(stmt.execute(&scope).is_ok(), true);
        assert_eq!(scope.borrow().has_name("foo"), true);
        assert_eq!(scope.borrow().get_value("foo"), Ok(Value::Number(2.0)));
    }

    #[test]
    fn test_var_declare_multiple_declare() {
        let scope = Scope::new().as_rc();
        let tokens = "var foo;".scan().0;
        let stmt = &tokens.parse().0[0];
        assert_eq!(stmt.execute(&scope).is_ok(), true);
        assert_eq!(
            stmt.execute(&scope).unwrap_err(),
            ExecuteError::MultipleDeclaration(code_span(0, 0, 0, 8))
        );
    }

    #[test]
    fn test_var_declare_initializer_evaluate_error() {
        let scope = Scope::new().as_rc();
        let tokens = "var foo = bar;".scan().0;
        let stmt = &tokens.parse().0[0];
        assert_eq!(
            stmt.execute(&scope).unwrap_err(),
            ExecuteError::EvaluateError(
                EvaluateError::VariableNotDeclared(code_span(0, 10, 0, 13))
            )
        );
    }

    #[test]
    fn test_block() {
        let src = "
            var foo = 1;
            {
                var bar = 2;
                foo = bar;
            }
        ";
        let scope = Scope::new().as_rc();
        let (tokens, errors) = src.scan();
        assert_eq!(errors.len(), 0);
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        for stmt in stmts {
            assert_eq!(stmt.execute(&scope).is_ok(), true);
        }
        assert_eq!(scope.borrow().get_value("foo").unwrap(), Value::Number(2.0));
    }

    #[test]
    fn test_block_shadow() {
        let src = "
            var foo = 1;
            {
                var foo = 2;
                foo = 3;
            }
        ";
        let scope = Scope::new().as_rc();
        let (tokens, errors) = src.scan();
        assert_eq!(errors.len(), 0);
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        for stmt in stmts {
            assert_eq!(stmt.execute(&scope).is_ok(), true);
        }
        assert_eq!(scope.borrow().get_value("foo").unwrap(), Value::Number(1.0));
    }

    #[test]
    fn test_block_execute_error() {
        let src = "
            {
                var foo;
                foo = bar;
            }
        ";
        let scope = Scope::new().as_rc();
        let (tokens, errors) = src.scan();
        assert_eq!(errors.len(), 0);
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        assert_eq!(
            stmts[0].execute(&scope).unwrap_err(),
            ExecuteError::EvaluateError(
                EvaluateError::VariableNotDeclared(code_span(3, 6, 3, 9))
            )
    )
    }

    #[test]
    fn test_if() {
        let src = "
            var foo = 1;
            if (true) foo = 2;
        ";
        let scope = Scope::new().as_rc();
        let (tokens, errors) = src.scan();
        assert_eq!(errors.len(), 0);
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        for stmt in stmts {
            assert_eq!(stmt.execute(&scope).is_ok(), true);
        }
        assert_eq!(scope.borrow().get_value("foo").unwrap(), Value::Number(2.0));
    } 

    #[test]
    fn test_if_else() {
        let src = "
            var foo = 1;
            if (false) {
                foo = 2;
            } else {
                foo = 3;
            }
        ";
        let scope = Scope::new().as_rc();
        let (tokens, errors) = src.scan();
        assert_eq!(errors.len(), 0);
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        for stmt in stmts {
            assert_eq!(stmt.execute(&scope).is_ok(), true);
        }
        assert_eq!(scope.borrow().get_value("foo").unwrap(), Value::Number(3.0));
    }

    #[test]
    fn test_expression() {
        let scope = Scope::new().as_rc();
        let tokens = "true;".scan().0;
        let stmt = &tokens.parse().0[0];
        assert_eq!(stmt.execute(&scope).is_ok(), true);
    }

    #[test]
    fn test_expression_evaluate_error() {
        let scope = Scope::new().as_rc();
        let stmt = &"foo;".scan().0.parse().0[0];
        assert_eq!(
            stmt.execute(&scope).unwrap_err(),
            ExecuteError::EvaluateError(
                EvaluateError::VariableNotDeclared(code_span(0, 0, 0, 3))
            )
        );
    }

    #[test]
    fn test_print() {
        let scope = Scope::new().as_rc();
        let tokens = "print true;".scan().0;
        let stmt = &tokens.parse().0[0];
        assert_eq!(stmt.execute(&scope).is_ok(), true);
    }

    #[test]
    fn test_print_evaluate_error() {
        let scope = Scope::new().as_rc();
        let stmt = &"print foo;".scan().0.parse().0[0];
        assert_eq!(
            stmt.execute(&scope).unwrap_err(),
            ExecuteError::EvaluateError(EvaluateError::VariableNotDeclared(code_span(0, 6, 0, 9)))
        );
    }

    #[test]
    fn test_while() {
        let src = "
            var sum = 0;
            var i = 3;
            while (i > 0) {
                sum = sum + i;
                i = i - 1;
            }
        ";
        let scope = Scope::new().as_rc();
        let (tokens, errors) = src.scan();
        assert_eq!(errors.len(), 0);
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        for stmt in stmts {
            assert_eq!(stmt.execute(&scope).is_ok(), true);
        }
        assert_eq!(scope.borrow().get_value("sum").unwrap(), Value::Number(6.0));
    }

    #[test]
    fn test_for() {
        let src = "
            var sum = 0;
            for (var i = 1; i <= 10; i = i + 1) {
                sum = sum + i;
            }
        ";
        let scope = Scope::new().as_rc();
        let (tokens, errors) = src.scan();
        assert_eq!(errors.len(), 0);
        let (stmts, errors) = &tokens.parse();
        assert_eq!(errors.len(), 0);
        for stmt in stmts {
            assert_eq!(stmt.execute(&scope).is_ok(), true);
        }
        assert_eq!(scope.borrow().get_value("sum").unwrap(), Value::Number(55.0));
    }
}
