pub mod token;
pub mod scanner;

use scanner::{
    Scanner,
    ScannerOutput,
};

pub trait Scan {
    fn scan(&self) -> ScannerOutput;
}

impl Scan for str {
    fn scan(&self) -> ScannerOutput {
        Scanner::scan(self)
    }
}
