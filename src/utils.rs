#[cfg(test)]
pub mod test_utils {
    use std::rc::Rc;
    use crate::{
        parse::{
            expression::{
                Expression,
                AsExpression,
            },
            statement::{
                Statement,
                AsStatement,
            },
            parser::{
                Parser,
                ParserError,
            },
        },
        scan::Scan,
        value::Value,
        environment::{
            Environment,
            EnvironmentOps,
        },
        error::RuntimeError,
        execute::ExecuteResult,
        resolve::{
            ResolveCtx,
            ResolveError,
        }
    };

    pub fn parse_expression<T: AsExpression>(src: &str) -> Rc<T> {
        parse_expression_unknown(src).downcast::<T>().unwrap()
    }

    pub fn parse_expression_unknown(src: &str) -> Expression {
        let (tokens, errors) = src.scan();
        assert_eq!(errors.len(), 0);
        let mut parser = Parser::new(&tokens);
        parser.expression().unwrap()
    }

    pub fn parse_statement<T: AsStatement>(src: &str) -> Rc<T> {
        parse_statement_unknown(src).downcast::<T>().unwrap()
    }

    pub fn parse_statement_unknown(src: &str) -> Statement {
        let (tokens, errors) = src.scan();
        assert_eq!(errors.len(), 0);
        let mut parser = Parser::new(&tokens);
        parser.statement(true).unwrap()
    }

    pub fn try_parse_statement(src: &str) -> Result<Statement, ParserError> {
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
        pub environment: Environment,
        pub resolve_context: ResolveCtx,
    }

    impl TestContext {
        pub fn new() -> TestContext {
            TestContext {
                environment: <Environment as EnvironmentOps>::new(),
                resolve_context: ResolveCtx::new(),
            }
        }

        pub fn resolve_expression<T>(
            &mut self,
            expr: &dyn AsExpression
        )
            -> Result<Rc<T>, ResolveError>
        where
            T: AsExpression
        {
            self.resolve_expression_unknown(expr)
                .map(|expr| expr.downcast::<T>().unwrap())
        }

        pub fn resolve_expression_unknown(
            &mut self,
            expr: &dyn AsExpression
        )
            -> Result<Expression, ResolveError>
        {
            expr.resolve(&mut self.resolve_context)
        }

        pub fn resolve_statement<T>(
            &mut self,
            stmt: &dyn AsStatement
        )
            -> Result<Rc<T>, ResolveError>
        where
            T: AsStatement
        {
            self.resolve_statement_unknown(stmt)
                .map(|stmt| stmt.downcast::<T>().unwrap())
        }

        pub fn resolve_statement_unknown(
            &mut self,
            stmt: &dyn AsStatement
        )
            -> Result<Statement, ResolveError>
        {
            stmt.resolve(&mut self.resolve_context)
        }

        pub fn evaluate(&mut self, expr: &dyn AsExpression) -> Result<Value, RuntimeError> {
            let expr = self.resolve_expression_unknown(expr).unwrap();
            expr.evaluate(&self.environment)
        }

        pub fn execute(&mut self, stmt: &dyn AsStatement) -> ExecuteResult {
            let stmt = self.resolve_statement_unknown(stmt).unwrap();
            stmt.execute(&self.environment)
        }

        pub fn execute_src(&mut self, src: &str) {
            let stmt = parse_statement_unknown(src);
            self.execute(stmt.as_ref()).expect("execute src");
        }
    }
}
