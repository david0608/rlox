use std::cell::RefCell;
use std::rc::Rc;
use crate::scope::Scope;
use crate::value::Value;
use crate::scan::span::Span;
use crate::parse::stmt::Stmt;
use crate::parse::stmt::block::BlockStatement;
use crate::parse::stmt::expression::ExpressionStatement;
use crate::parse::stmt::r#for::ForStatement;
use crate::parse::stmt::ifelse::IfStatement;
use crate::parse::stmt::print::PrintStatement;
use crate::parse::stmt::var_declare::VarDeclareStatement;
use crate::parse::stmt::r#while::WhileStatement;
use crate::visitor::evaluate::Error as EvaluateError;
use super::{
    ScopeVisit,
    ScopeAccept,
};

#[derive(Debug, PartialEq)]
pub enum Error {
    EvaluateError(EvaluateError),
    MultipleDeclaration(Span),
}

pub type ExecuteResult = std::result::Result<(), Error>;

pub struct Execute;

pub trait Executable
    where
    Self: for<'this> ScopeAccept<'this, Execute, ExecuteResult>
{
    fn execute<'this>(&'this self, scope: &Rc<RefCell<Scope>>) -> ExecuteResult {
        self.accept(Execute, scope)
    }
}

impl<T> Executable for T
    where
    T: for<'this> ScopeAccept<'this, Execute, ExecuteResult>
{ }

impl<'that> ScopeVisit<'that, BlockStatement, ExecuteResult> for Execute {
    fn visit(stmt: &'that BlockStatement, scope: &Rc<RefCell<Scope>>) -> ExecuteResult {
        let scope = Scope::new_child(scope).as_rc();
        for stmt in stmt.stmts() {
            stmt.execute(&scope)?;
        }
        Ok(())
    }
}

impl<'that> ScopeVisit<'that, ExpressionStatement, ExecuteResult> for Execute {
    fn visit(stmt: &'that ExpressionStatement, scope: &Rc<RefCell<Scope>>) -> ExecuteResult {
        if let Err(e) = stmt.expression().evaluate(scope) {
            Err(Error::EvaluateError(e))
        }
        else {
            Ok(())
        }
    }
}

impl<'that> ScopeVisit<'that, ForStatement, ExecuteResult> for Execute {
    fn visit(stmt: &'that ForStatement, scope: &Rc<RefCell<Scope>>) -> ExecuteResult {
        let scope = Scope::new_child(scope).as_rc();

        if let Some(initializer) = stmt.initializer() {
            initializer.execute(&scope)?;
        }

        loop {
            if let Some(condition) = stmt.condition() {
                match condition.evaluate(&scope) {
                    Ok(v) => {
                        if !v.is_truthy() {
                            break;
                        }
                    }
                    Err(e) => {
                        return Err(Error::EvaluateError(e));
                    }
                }
            }

            stmt.body().execute(&scope)?;

            if let Some(increment) = stmt.increment() {
                if let Err(e) = increment.evaluate(&scope) {
                    return Err(Error::EvaluateError(e));
                }
            }
        }

        return Ok(());
    }
}

impl<'that> ScopeVisit<'that, IfStatement, ExecuteResult> for Execute {
    fn visit(stmt: &'that IfStatement, scope: &Rc<RefCell<Scope>>) -> ExecuteResult {
        let condition = stmt.condition().evaluate(scope)
            .map(|v| v.is_truthy())
            .map_err(|e| Error::EvaluateError(e))?;
        let scope = Scope::new_child(scope).as_rc();
        if condition {
            stmt.then_stmt().execute(&scope)?;
        }
        else if let Some(else_stmt) = stmt.else_stmt() {
            else_stmt.execute(&scope)?;
        }
        return Ok(())
    }
}

impl<'that> ScopeVisit<'that, PrintStatement, ExecuteResult> for Execute {
    fn visit(stmt: &'that PrintStatement, scope: &Rc<RefCell<Scope>>) -> ExecuteResult {
        match stmt.value().evaluate(scope) {
            Ok(v) => {
                println!("{}", v);
                Ok(())
            }
            Err(e) => {
                Err(Error::EvaluateError(e))
            }
        }
    }
}

impl<'that> ScopeVisit<'that, VarDeclareStatement, ExecuteResult> for Execute {
    fn visit(stmt: &'that VarDeclareStatement, scope: &Rc<RefCell<Scope>>) -> ExecuteResult {
        let mut value = Value::Nil;
        if let Some(i) = stmt.initializer() {
            match i.evaluate(scope) {
                Ok(v) => value = v,
                Err(e) => {
                    return Err(Error::EvaluateError(e));
                }
            }
        };
        if scope.borrow_mut().declare(stmt.name(), value).is_err() {
            return Err(Error::MultipleDeclaration(stmt.span()))
        }
        Ok(())
    }
}

impl<'that> ScopeVisit<'that, WhileStatement, ExecuteResult> for Execute {
    fn visit(stmt: &'that WhileStatement, scope: &Rc<RefCell<Scope>>) -> ExecuteResult {
        loop {
            if let Some(condition) = stmt.condition() {
                match condition.evaluate(scope) {
                    Ok(v) => {
                        if !v.is_truthy() {
                            break;
                        }
                    }
                    Err(e) => {
                        return Err(Error::EvaluateError(e));
                    }
                }
            }

            stmt.body().execute(scope)?;
        }

        return Ok(());
    }
}

#[cfg(test)]
mod tests {
    use crate::value::Value;
    use crate::scope::Scope;
    use crate::scan::span::{
        Span,
        CodePoint,
    };
    use crate::visitor::{
        Scannable,
        Parsable,
    };
    use super::{
        Error,
        EvaluateError,
    };

    fn span(sl: usize, sc: usize, el: usize, ec: usize) -> Span {
        Span::new(CodePoint::new(sl, sc), CodePoint::new(el, ec))
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
            Error::MultipleDeclaration(span(0, 0, 0, 8))
        );
    }

    #[test]
    fn test_var_declare_initializer_evaluate_error() {
        let scope = Scope::new().as_rc();
        let tokens = "var foo = bar;".scan().0;
        let stmt = &tokens.parse().0[0];
        assert_eq!(
            stmt.execute(&scope).unwrap_err(),
            Error::EvaluateError(
                EvaluateError::VariableNotDeclared(span(0, 10, 0, 13))
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
            Error::EvaluateError(
                EvaluateError::VariableNotDeclared(span(3, 6, 3, 9))
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
            Error::EvaluateError(
                EvaluateError::VariableNotDeclared(span(0, 0, 0, 3))
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
            Error::EvaluateError(EvaluateError::VariableNotDeclared(span(0, 6, 0, 9)))
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
