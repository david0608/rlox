use std::{
    rc::Rc,
    collections::HashMap,
};
use crate::code::code_span::CodeSpan;
use crate::code::Code;
use crate::parse::statement::{
    Statement,
    AsStatement,
};
use crate::scan::token::identifier::IdentifierToken;
use crate::value::{
    Value,
    class::Class,
};
use crate::environment::{
    Environment,
    EnvironmentOps,
};
use crate::error::{
    RuntimeError,
    RuntimeErrorEnum,
};
use crate::execute::{
    Execute,
    ExecuteResult,
    ExecuteOk,
};
use crate::print::Print;
use crate::resolve::{
    ResolveCtx,
    ResolveError,
    ResolveErrorEnum,
};
use crate::{
    impl_debug_for_printable,
    runtime_error,
    resolve_error,
};

#[derive(Debug, Clone)]
pub struct MethodDefinition {
    name: IdentifierToken,
    parameters: Vec<IdentifierToken>,
    body: Vec<Statement>,
    code_span: CodeSpan,
}

impl MethodDefinition {
    pub fn new(
        name: IdentifierToken,
        parameters: Vec<IdentifierToken>,
        body: Vec<Statement>,
        code_span: CodeSpan,
    )
        -> MethodDefinition
    {
        MethodDefinition {
            name,
            parameters,
            body,
            code_span,
        }
    }

    pub fn name(&self) -> &IdentifierToken {
        &self.name
    }

    pub fn parameters(&self) -> &Vec<IdentifierToken> {
        &self.parameters
    }

    pub fn body(&self) -> &Vec<Statement> {
        &self.body
    }

    pub fn code_span(&self) -> CodeSpan {
        self.code_span
    }

    pub fn resolve(&self, context: &mut ResolveCtx) -> Result<MethodDefinition, ResolveError> {
        context.begin();
        context.declare("this").expect("declare this");
        for p in self.parameters.iter() {
            if context.declare(p.name()).is_err() {
                context.end();
                return Err(
                    resolve_error!(
                        ResolveErrorEnum::VariableHasBeenDeclared,
                        p.code_span()
                    )
                );
            }
        }
        let body = self.body.iter().map(|s| s.resolve(context)).collect::<Result<Vec<Statement>, ResolveError>>()?;
        context.end();
        return Ok(
            MethodDefinition::new(
                self.name.clone(),
                self.parameters.clone(),
                body,
                self.code_span,
            )
        );
    }
}

pub struct ClassDeclareStatement {
    name: IdentifierToken,
    method_definitions: Rc<HashMap<String, Rc<MethodDefinition>>>,
    code_span: CodeSpan,
}

impl ClassDeclareStatement {
    pub fn new(
        name: IdentifierToken,
        method_definitions: Rc<HashMap<String, Rc<MethodDefinition>>>,
        code_span: CodeSpan,
    ) -> ClassDeclareStatement
    {
        ClassDeclareStatement {
            name,
            method_definitions,
            code_span,
        }
    }

    pub fn name(&self) -> &IdentifierToken {
        &self.name
    }
}

impl Code for ClassDeclareStatement {
    fn code_span(&self) -> CodeSpan {
        self.code_span
    }
}

impl Print for ClassDeclareStatement {
    fn print(&self) -> String {
        format!(
            "class {} {{{}}}",
            self.name.name(),
            self.method_definitions
        )
    }
}

impl_debug_for_printable!(ClassDeclareStatement);

impl Execute for ClassDeclareStatement {
    fn execute(&self, env: &Environment) -> ExecuteResult {
        if env.declare(
            self.name().name(),
            Value::Class(
                Class::new(
                    self.name.clone(),
                    env.clone(),
                    self.method_definitions.clone(),
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

impl AsStatement for ClassDeclareStatement {
    fn resolve(&self, context: &mut ResolveCtx) -> Result<Statement, ResolveError> {
        if context.declare(self.name.name()).is_err() {
            return Err(
                resolve_error!(
                    ResolveErrorEnum::VariableHasBeenDeclared,
                    self.name.code_span()
                )
            );
        }
        let mut method_defs = HashMap::new();
        for (key, method) in self.method_definitions.iter() {
            method_defs.insert(key.to_owned(), Rc::new(method.resolve(context)?));
        }
        return Ok(
            Statement(
                Rc::new(
                    ClassDeclareStatement::new(
                        self.name.clone(),
                        Rc::new(method_defs),
                        self.code_span,
                    )
                )
            )
        );
    }
}

#[macro_export]
macro_rules! class_declare_statement {
    ( $identifier:expr, $method_definitions:expr, $code_span:expr ) => {
        Statement(
            Rc::new(
                ClassDeclareStatement::new(
                    $identifier,
                    $method_definitions,
                    $code_span,
                )
            )
        )
    };
}

#[cfg(test)]
mod tests {

}
