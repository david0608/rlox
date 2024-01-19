use std::rc::Rc;
use std::cell::RefCell;
use std::time::{
    UNIX_EPOCH,
    SystemTime,
};
use crate::value::{
    Value,
    native_function::NativeFunction,
};
use crate::call::{
    CallResult,
    CallError,
};
use crate::environment::{
    Environment,
    environment_declare,
};

pub fn add_native_clock(env: &Rc<RefCell<Environment>>) {
    environment_declare(
        env,
        "clock",
        Value::NativeFunction(
            NativeFunction::new("clock", native_function_clock_handler),
        )
    )
        .expect("Declare native function clock.");
}

fn native_function_clock_handler(arguments: Vec<Value>) -> CallResult {
    let argn = arguments.len();
    if argn != 0 {
        return Err(CallError::ArgumentNumberMismatch(0, argn));
    }

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
    use crate::value::Value;
    use crate::call::{
        Call,
        CallError,
    };
    use crate::environment::{
        new_environment,
        environment_get_value,
    };

    #[test]
    fn test_native_function_clock() {
        let env = new_environment();
        add_native_clock(&env);
        let nf = if let Ok(Value::NativeFunction(nf)) = environment_get_value(&env, "clock") {
            nf
        }
        else {
            panic!("Native function clock should be declared.");
        };
        let start = if let Ok(Value::Number(ms)) = nf.call(vec![]) {
            ms
        }
        else {
            panic!("Call on native function clock should return timestamp number.");
        };
        sleep(Duration::from_millis(100));
        let end = if let Ok(Value::Number(ms)) = nf.call(vec![]) {
            ms
        }
        else {
            panic!("Call on native function clock should return timestamp number.");
        };
        assert_eq!((end - start) >= 100.0, true);
    }

    #[test]
    fn test_native_function_clock_call_argument_number_mismatch_error() {
        assert_eq!(
            native_function_clock_handler(vec![Value::Bool(true)]),
            Err(
                CallError::ArgumentNumberMismatch(0, 1)
            )
        );
    }
}
