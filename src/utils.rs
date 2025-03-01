use std::{
    rc::Rc,
    any::Any,
};

pub trait Downcast {
    fn downcast<T: Any>(self) -> Option<Rc<T>>;

    fn downcast_ref<T: Any>(&self) -> Option<&T>;
}

#[cfg(test)]
pub mod test_utils {
    use std::{
        rc::Rc,
        cell::RefCell,
        collections::HashSet,
    };
    use crate::{
        parse::{
            expression::Expression,
            statement::{
                Statement,
                ExecuteOk,
            },
            parser::Parser,
        },
        scan::Scan,
        value::Value,
        environment::{
            Environment,
            EnvironmentT,
        },
        error::{
            ParserError,
            ResolveError,
            RuntimeError
        },
        resolve_context::ResolveContext,
        utils::Downcast,
    };

    pub fn parse_expression<T: Expression>(src: &str) -> Rc<T> {
        parse_expression_unknown(src).downcast::<T>().unwrap()
    }

    pub fn parse_expression_unknown(src: &str) -> Rc<dyn Expression> {
        let (tokens, errors) = src.scan();
        assert_eq!(errors.len(), 0);
        let mut parser = Parser::new(&tokens);
        parser.expression().unwrap()
    }

    pub fn parse_statement<T: Statement>(src: &str) -> Rc<T> {
        parse_statement_unknown(src).downcast::<T>().unwrap()
    }

    pub fn parse_statement_unknown(src: &str) -> Rc<dyn Statement> {
        let (tokens, errors) = src.scan();
        assert_eq!(errors.len(), 0);
        let mut parser = Parser::new(&tokens);
        parser.statement(true).unwrap()
    }

    pub fn try_parse_statement(src: &str) -> Result<Rc<dyn Statement>, ParserError> {
        let (tokens, errors) = src.scan();
        assert_eq!(errors.len(), 0);
        let mut parser = Parser::new(&tokens);
        parser.statement(true)
    }

    pub fn evaluate_src(src: &str) -> Result<Value, RuntimeError> {
        let mut ctx = TestContext::new();
        ctx.evaluate(parse_expression_unknown(src).as_ref())
    }

    pub struct TestContext {
        pub environment: Rc<RefCell<Environment>>,
        pub resolve_context: Vec<HashSet<String>>,
    }

    impl TestContext {
        pub fn new() -> TestContext {
            TestContext {
                environment: <Rc<RefCell<Environment>> as EnvironmentT>::new(),
                resolve_context: <Vec<HashSet<String>> as ResolveContext>::new(),
            }
        }

        pub fn resolve_expression<T>(
            &mut self,
            expr: &dyn Expression
        )
            -> Result<Rc<T>, ResolveError>
        where
            T: Expression
        {
            self.resolve_expression_unknown(expr)
                .map(|expr| expr.downcast::<T>().unwrap())
        }

        pub fn resolve_expression_unknown(
            &mut self,
            expr: &dyn Expression
        )
            -> Result<Rc<dyn Expression>, ResolveError>
        {
            expr.resolve(&mut self.resolve_context)
        }

        pub fn resolve_statement<T>(
            &mut self,
            stmt: &dyn Statement
        )
            -> Result<Rc<T>, ResolveError>
        where
            T: Statement
        {
            self.resolve_statement_unknown(stmt)
                .map(|stmt| stmt.downcast::<T>().unwrap())
        }

        pub fn resolve_statement_unknown(
            &mut self,
            stmt: &dyn Statement
        )
            -> Result<Rc<dyn Statement>, ResolveError>
        {
            stmt.resolve(&mut self.resolve_context)
        }

        pub fn evaluate(&mut self, expr: &dyn Expression) -> Result<Value, RuntimeError> {
            let expr = self.resolve_expression_unknown(expr).unwrap();
            expr.evaluate(&self.environment)
        }

        pub fn execute(&mut self, stmt: &dyn Statement) -> Result<ExecuteOk, RuntimeError> {
            let stmt = self.resolve_statement_unknown(stmt).unwrap();
            stmt.execute(&self.environment)
        }

        pub fn execute_src(&mut self, src: &str) {
            let stmt = parse_statement_unknown(src);
            self.execute(stmt.as_ref()).expect("execute src");
        }
    }
}
