use std::{
    io::Write,
    rc::Rc,
    cell::RefCell,
    collections::HashSet,
};
use rlox::{
    code::Annotation,
    parse::{
        Parse,
        statement::Statement,
    },
    scan::Scan,
    environment::{
        Environment,
        EnvironmentT,
    },
    error::ResolveError,
    resolve_context::ResolveContext,
};

// fn run_file(path: &str) -> Result {
//
// }

fn run_prompt() -> Result<(), Box<dyn std::error::Error>> {
    let mut buf = String::new();
    let input = std::io::stdin();
    let mut output = std::io::stdout().lock();
    let mut resolve_context = <Vec<HashSet<String>> as ResolveContext>::new();
    let env = <Rc<RefCell<Environment>> as EnvironmentT>::new();

    loop {
        output.write(b"> ")?;
        output.flush()?;

        buf.clear();
        input.read_line(&mut buf)?;
        let src = buf.as_str();
        let lines: Vec<&str> = src.lines().collect();

        let (tokens, errors) = src.scan();
        if errors.len() > 0 {
            for error in errors {
                output.write(error.string(&lines).as_bytes())?;
            }
            continue;
        }

        let (stmts_not_resolved, errors) = tokens.parse();
        if errors.len() > 0 {
            for error in errors {
                output.write(error.string(&lines).as_bytes())?;
            }
            continue;
        }

        let stmts = match stmts_not_resolved.iter().map(|s| s.resolve(&mut resolve_context)).collect::<Result<Vec<Rc<dyn Statement>>, ResolveError>>() {
            Ok(ok) => ok,
            Err(error) => {
                output.write(error.string(&lines).as_bytes())?;
                continue;
            }
        };

        for stmt in stmts {
            if let Err(error) = stmt.execute(&env) {
                output.write(error.string(&lines).as_bytes())?;
                continue;
            }
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    run_prompt()
}
