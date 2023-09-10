use std::rc::Rc;
use std::cell::RefCell;
use std::time::{
    UNIX_EPOCH,
    SystemTime,
};
use crate::evaluate::value::call::CallResult;
use crate::evaluate::value::value::Value;
use super::function::NativeFunction;
use crate::scope::Scope;

pub fn add_native_clock(scope: &Rc<RefCell<Scope>>) {
    scope.borrow_mut()
        .declare(
            "clock",
            Value::NativeFunction(
                NativeFunction::new("clock", native_function_clock_handler),
            )
        )
        .expect("Declare native function clock.");
}

fn native_function_clock_handler(_: &Rc<RefCell<Scope>>, _: Vec<Value>) -> CallResult {
    return Ok(
        Value::Number(
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map(|d| d.as_millis())
                .unwrap_or(0) as f64
        )
    );
}

#[cfg(test)]
mod tests {
    use std::thread::sleep;
    use std::time::Duration;
    use super::*;
    use crate::parse::parser::Parser;
    use crate::scan::Scan;
    use crate::scope::Scope;

    #[test]
    fn test_add_native_clock() {
        let scope = Scope::new().as_rc();
        add_native_clock(&scope);
        assert_eq!(scope.borrow().get_value("clock").is_ok(), true);
    }

    #[test]
    fn test_native_function_clock() {
        let scope = Scope::new().as_rc();
        add_native_clock(&scope);
        let tokens = "clock()".scan().0;
        let expression = Parser::new(&tokens).expression().unwrap();
        let start = expression.evaluate(&scope).unwrap();
        sleep(Duration::from_millis(100));
        let end = expression.evaluate(&scope).unwrap();
        if let (Value::Number(s), Value::Number(e)) = (start, end) {
            assert_eq!((e - s) >= 100.0, true);
        }
        else {
            panic!("Return value of native function clock should be type of Number.");
        }
    }
}
