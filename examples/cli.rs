use std::io::Write;
use rlox::{
    parse::Parse,
    scan::Scan,
    environment::{
        Environment,
        EnvironmentOps,
    },
    error::LoxError,
    resolve::{
        Resolve,
        ResolveCtx,
    },
};

type Result = std::result::Result<(), Box<dyn std::error::Error>>;

// fn run_file(path: &str) -> Result {
//
// }

fn run_prompt() -> Result {
    let mut buf = String::new();
    let input = std::io::stdin();
    let mut output = std::io::stdout().lock();
    let mut resolve_context = ResolveCtx::new();
    let env = <Environment as EnvironmentOps>::new();

    loop {
        output.write(b"> ")?;
        output.flush()?;

        buf.clear();
        input.read_line(&mut buf)?;
        let src = buf.as_str();

        let (tokens, errors) = src.scan();
        if errors.len() > 0 {
            for error in errors {
                output.write(error.print(&src.lines().collect()).as_bytes())?;
            }
            continue;
        }

        let (stmts_not_resolved, errors) = tokens.parse();
        if errors.len() > 0 {
            for error in errors {
                output.write(error.print(&src.lines().collect()).as_bytes())?;
            }
            continue;
        }

        let stmts = match stmts_not_resolved.resolve(&mut resolve_context) {
            Ok(stmts) => stmts,
            Err(error) => {
                output.write(error.print(&src.lines().collect()).as_bytes())?;
                continue;
            }
        };

        for stmt in stmts {
            if let Err(error) = stmt.execute(&env) {
                output.write(error.print(&src.lines().collect()).as_bytes())?;
                continue;
            }
        }
    }
}

fn main() -> Result {
    run_prompt()
}
