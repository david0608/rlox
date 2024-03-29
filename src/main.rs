use std::io::Write;

mod value;
mod error;
pub mod scope;
pub mod scan;
mod parse;
mod visitor;

type Result = std::result::Result<(), Box<dyn std::error::Error>>;

// fn run_file(path: &str) -> Result {
//
// }

fn run_prompt() -> Result {
    let mut buf = String::new();
    let input = std::io::stdin();
    let mut output = std::io::stdout().lock();

    loop {
        output.write(b"> ")?;
        output.flush()?;

        input.read_line(&mut buf)?;

        output.write(format!("Enter content: {}", buf).as_bytes())?;

        buf.clear();
    }
}

fn main() -> Result {
    run_prompt()
}
