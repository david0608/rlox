use std::cell::RefCell;
use std::rc::Rc;
use crate::scope::{
    Scope,
    Error as ScopeError,
};
use crate::value::Value;
use crate::parse::stmt::{
    VarDeclareStatement,
    BlockStatement,
    IfStatement,
    ExpressionStatement,
    PrintStatement,
    WhileStatement,
};
use crate::visitor::evaluate::Error as EvaluateError;
use super::{
    ScopeVisit,
    ScopeAccept,
};

#[derive(Debug)]
pub enum Error<'expr, 'src> {
    EvaluateError(EvaluateError<'expr, 'src>),
    DeclarationError(ScopeError<'src>)
}

pub type ExecuteResult<'expr, 'src> = std::result::Result<(), Error<'expr, 'src>>;

pub struct Execute;

pub trait Executable<'src>
    where
    Self: for<'this> ScopeAccept<'this, Execute, ExecuteResult<'this, 'src>>
{
    fn execute<'this>(&'this self, scope: &Rc<RefCell<Scope>>) -> ExecuteResult<'this, 'src> {
        self.accept(Execute, scope)
    }
}

impl<'src, T> Executable<'src> for T
    where
    T: for<'this> ScopeAccept<'this, Execute, ExecuteResult<'this, 'src>>
{ }

impl<'that, 'src> ScopeVisit<'that, VarDeclareStatement<'src>, ExecuteResult<'that, 'src>> for Execute {
    fn visit(stmt: &'that VarDeclareStatement<'src>, scope: &Rc<RefCell<Scope>>) -> ExecuteResult<'that, 'src> {
        let mut value = Value::Nil;
        if let Some(i) = stmt.initializer.as_ref() {
            match i.evaluate(scope) {
                Ok(v) => value = v,
                Err(e) => {
                    return Err(Error::EvaluateError(e));
                }
            }
        };
        if let Err(e) = scope.borrow_mut().declare(stmt.name.lexeme(), value) {
            return Err(Error::DeclarationError(e));
        };
        Ok(())
    }
}

impl<'that, 'src> ScopeVisit<'that, BlockStatement<'src>, ExecuteResult<'that, 'src>> for Execute {
    fn visit(stmt: &'that BlockStatement<'src>, scope: &Rc<RefCell<Scope>>) -> ExecuteResult<'that, 'src> {
        let scope = Scope::new_child(scope).as_rc();
        for stmt in &stmt.0 {
            stmt.execute(&scope)?;
        }
        Ok(())
    }
}

impl<'that, 'src> ScopeVisit<'that, IfStatement<'src>, ExecuteResult<'that, 'src>> for Execute {
    fn visit(stmt: &'that IfStatement<'src>, scope: &Rc<RefCell<Scope>>) -> ExecuteResult<'that, 'src> {
        let condition = stmt.condition.evaluate(scope)
            .map(|v| v.is_truthy())
            .map_err(|e| Error::EvaluateError(e))?;
        let scope = Scope::new_child(scope).as_rc();
        if condition {
            stmt.then_stmt.execute(&scope)?;
        }
        else if let Some(else_stmt) = stmt.else_stmt.as_ref() {
            else_stmt.execute(&scope)?;
        }
        return Ok(())
    }
}

impl<'that, 'src> ScopeVisit<'that, ExpressionStatement<'src>, ExecuteResult<'that, 'src>> for Execute {
    fn visit(stmt: &'that ExpressionStatement<'src>, scope: &Rc<RefCell<Scope>>) -> ExecuteResult<'that, 'src> {
        if let Err(e) = stmt.0.evaluate(scope) {
            Err(Error::EvaluateError(e))
        }
        else {
            Ok(())
        }
    }
}

impl<'that, 'src> ScopeVisit<'that, PrintStatement<'src>, ExecuteResult<'that, 'src>> for Execute {
    fn visit(stmt: &'that PrintStatement<'src>, scope: &Rc<RefCell<Scope>>) -> ExecuteResult<'that, 'src> {
        match stmt.0.evaluate(scope) {
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

impl<'that, 'src> ScopeVisit<'that, WhileStatement<'src>, ExecuteResult<'that, 'src>> for Execute {
    fn visit(stmt: &'that WhileStatement<'src>, scope: &Rc<RefCell<Scope>>) -> ExecuteResult<'that, 'src> {
        while stmt.condition.evaluate(scope).map(|v| v.is_truthy()).map_err(|e| Error::EvaluateError(e))? {
            stmt.body.execute(&scope)?;
        }
        return Ok(());
    }
}

#[cfg(test)]
mod tests {
    use crate::value::Value;
    use crate::scope::Scope;
    use crate::visitor::{
        Scannable,
        Parsable,
    };

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
            format!("{:?}", stmt.execute(&scope).unwrap_err()),
            "DeclarationError(MultipleDeclaration(\"foo\"))"
        );
    }

    #[test]
    fn test_var_declare_initializer_evaluate_error() {
        let scope = Scope::new().as_rc();
        let tokens = "var foo = bar;".scan().0;
        let stmt = &tokens.parse().0[0];
        assert_eq!(
            format!("{:?}", stmt.execute(&scope).unwrap_err()),
            "EvaluateError(VariableResolveError(bar, NotDeclared(\"bar\")))"
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
            format!("{:?}", stmts[0].execute(&scope).unwrap_err()),
            "EvaluateError(VariableResolveError(bar, NotDeclared(\"bar\")))",
        );
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
        let tokens = "foo;".scan().0;
        let stmt = &tokens.parse().0[0];
        assert_eq!(
            format!("{:?}", stmt.execute(&scope).unwrap_err()),
            "EvaluateError(VariableResolveError(foo, NotDeclared(\"foo\")))"
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
        let tokens = "print foo;".scan().0;
        let stmt = &tokens.parse().0[0];
        assert_eq!(
            format!("{:?}", stmt.execute(&scope).unwrap_err()),
            "EvaluateError(VariableResolveError(foo, NotDeclared(\"foo\")))"
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
}
