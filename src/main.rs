mod token;
mod scanner;

use std::io::{self, Write};
use std::error;

type Result = std::result::Result<(), Box<dyn error::Error>>;

// fn run_file(path: &str) -> Result {
//
// }

fn run_prompt() -> Result {
    let mut buf = String::new();
    let input = io::stdin();
    let mut output = io::stdout().lock();

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
